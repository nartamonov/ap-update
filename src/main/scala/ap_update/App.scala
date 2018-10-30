package ap_update

import java.io.FileReader
import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.Comparator

import cats.{Applicative, FlatMap, Show}
import cats.effect.{Console, ExitCode, IO, IOApp, Sync}
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monad._
import cats.syntax.applicativeError._
import cats.syntax.show._
import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.int._
import cats.instances.string._
import cats.effect.Console.io._
import ap_update.console.ansi._
import ap_update.console.table._
import org.apache.commons.csv.{CSVFormat, CSVParser, CSVRecord}

import scala.util.Try
import scala.util.control.NonFatal

case class Config(csvFormat: CSVFormat,
                  delimiter: Char,
                  withHeader: Boolean,
                  dataFile: Path,
                  updateAt: LocalDateTime)

object App extends IOApp {

  implicit val showZoneId: Show[ZoneId] = Show.fromToString

  def run(args: List[String]): IO[ExitCode] = {
    AppCommand.appCmd.parse(args) match {
      case Right(config) =>
        withAnsiConsole {
          for {
            groups      <- readGroupsFromCSVFile(config.dataFile, config.csvFormat, config.delimiter, config.withHeader)
            _           <- putStrLn(s"Прочитано ${green(groups.size)} групп точек подключений.")
            _           <- putStrLn(s"Обновления будут начаты не ранее ${green(config.updateAt.format(Formats.RussianDateTimeFormat))} по местному времени группы.")
            localZoneId <- IO(ZoneId.systemDefault())
            now         <- IO(LocalDateTime.now(localZoneId))
            theSchedule  = schedule(groups, config.updateAt)
            _           <- printSchedule(theSchedule, now, localZoneId)
            continue    <- askToContinue
          } yield ExitCode.Success
        }.recoverWith {
          case NonFatal(e) =>
            putError(Option(e.getLocalizedMessage).getOrElse(s"Произошла непредвиденная ошибка: ${e.getClass}")).as(ExitCode.Error)
        }
      case Left(help) =>
        putError(help.toString()).as(ExitCode.Error)
    }
  }

  case class GroupsReadingError(msg: String, cause: Option[Throwable]) extends Exception(msg, cause.orNull)

  def readGroupsFromCSVFile(file: Path, format: CSVFormat, delimiter: Char, withHeader: Boolean): IO[Vector[APGroup]] = {
    import scala.collection.JavaConverters._

    def openFile: IO[CSVParser] = IO {
      val fmt = format.withDelimiter(delimiter)
      (if (withHeader) fmt.withFirstRecordAsHeader() else fmt).parse(new FileReader(file.toFile))
    }

    def parseZoneOffset(s: String): Option[ZoneOffset] = Try { ZoneOffset.of(s) }.toOption

    def throwErrorInRecord(rec: CSVRecord, msg: String): Nothing =
      throw GroupsReadingError(
        s"Не удалось прочитать группы точек доступа из файла $file. Ошибка в строке ${rec.getRecordNumber}: $msg", None)

    def readGroups(parser: CSVParser): IO[Vector[APGroup]] = IO {
      val records = for (record <- parser.asScala) yield {
        if (record.size() != 2)
          throwErrorInRecord(record, "обнаружено кол-во записей, не равное 2. Возможно, вы используете неверный символ-разделитель?")
        val zoneOffsetValue = record.get(1)
        parseZoneOffset(zoneOffsetValue) match {
          case Some(offset) => APGroup(record.get(0), ZoneOffset.of(record.get(1)))
          case None => throwErrorInRecord(record, s"неверный формат смещения часовой зоны ('$zoneOffsetValue'). Примеры верных значений: +04:00, +05:30, +00:00, -07:00.")
        }
      }
      records.toVector
    }

    openFile.bracket(readGroups)(parser => IO(parser.close()))
  }

  def askToContinue: IO[Boolean] = {
    def readYesOrNo: IO[Boolean] =
      readLn.flatMap { in =>
        in.toLowerCase match {
          case "y" | "yes" | "д" | "да" => IO.pure(true)
          case "n" | "no" | "н" | "нет" => IO.pure(false)
          case _ => putStr("Неверный ввод, введите yes/no/да/нет: ") >> readYesOrNo
        }
      }

    putStr("Продолжить (y/n)? ") >> readYesOrNo
  }

  /** Группа точек доступа (access point group) */
  case class APGroup(name: String, zoneOffset: ZoneOffset)

  case class ScheduledBucket(at: ZonedDateTime, groups: Set[APGroup]) {
    def isLate(now: ZonedDateTime): Boolean = at.isBefore(now)
  }

  object ScheduledBucket {
    implicit val zonedDateTimeOrdering: Ordering[ZonedDateTime] = Ordering.comparatorToOrdering(Comparator.naturalOrder())
    implicit val ordering: Ordering[ScheduledBucket] = Ordering.by(_.at)
  }

  case class Schedule(buckets: Vector[ScheduledBucket]) {
    /** Овоздавшие бакеты: время запуска которых уже прошло */
    def missedBuckets(now: ZonedDateTime): Vector[ScheduledBucket] = buckets.filter(_.isLate(now))
  }

  def schedule(groups: Vector[APGroup], at: LocalDateTime): Schedule = {
    val buckets = groups
      .groupBy(_.zoneOffset)
      .map { case (zoneOffset, groups) => ScheduledBucket(at.atZone(zoneOffset), groups.toSet) }
      .toVector
      .sorted

    Schedule(buckets)
  }

  def printSchedule(schedule: Schedule, now: LocalDateTime, localZoneId: ZoneId): IO[Unit] = {
    implicit val consoleIO: Console[IO] = Console.io

    def groupNames(bucket: ScheduledBucket): String = bucket.groups.map(_.name).mkString(",")
    def bucketLocalDT(bucket: ScheduledBucket): Cell = {
      val dt = bucket.at.withZoneSameInstant(localZoneId).format(Formats.RussianDateTimeFormat)
      // Те бакеты, запуск которых уже просрочен, выводим красными
      if (bucket.isLate(now.atZone(localZoneId)))
        Cell(dt,Some(Color.Red))
      else
        dt
    }

    def mkTable = ConsoleTable[IO]((
      Column("Порядок запуска", 16),
      Column("Местное время запуска", 22),
      Column("Часовая зона", 14),
      Column("Кол-во групп", 14),
      Column("Группы", 20)))

    val scheduleTable = schedule.buckets.zipWithIndex.foldLeft(mkTable) {
      case (t, (bucket, idx)) =>
        t.addRow((idx + 1, bucketLocalDT(bucket), bucket.at.getZone, bucket.groups.size, groupNames(bucket)))
    }

    for {
      _ <- putStrLn(s"Текущее местное время: ${green(now.format(Formats.RussianDateTimeFormat))} (в зоне ${green(localZoneId)})")
      _ <- putStrLn("Расписание запуска обновлений для групп точек доступа:\n")
      _ <- scheduleTable.print
      _ <- putStrLn("")
      missedBuckets = schedule.missedBuckets(now.atZone(localZoneId))
      _ <- putStrLn(s"${brightRed("ВНИМАНИЕ!")} Время запуска обновлений для ${missedBuckets.size} наборов групп было просрочено (помечены в расписании красным).").whenA(missedBuckets.nonEmpty)
    } yield {}
  }
}
