package ap_update

import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import java.time.format.DateTimeParseException

import cats.data.{Validated, ValidatedNel}
import cats.syntax.apply._
import com.monovore.decline.{Argument, Command, Opts}
import org.apache.commons.csv.CSVFormat

object AppCommand {
  implicit private val readCSVFormat: Argument[CSVFormat] = new Argument[CSVFormat] {
    def read(string: String): ValidatedNel[String, CSVFormat] = {
      string.toLowerCase match {
        case "csv"         => Validated.valid(CSVFormat.DEFAULT)
        case "csv-excel"   => Validated.valid(CSVFormat.EXCEL)
        case "csv-rfc4180" => Validated.valid(CSVFormat.RFC4180)
        case fmt           => Validated.invalidNel(s"Некорректный формат данных: $fmt")
      }
    }

    def defaultMetavar: String = "формат"
  }

  implicit private val readLocalDateTime: Argument[LocalDateTime] = new Argument[LocalDateTime] {
    def read(string: String): ValidatedNel[String, LocalDateTime] =
      try {
        Validated.valid(LocalDateTime.parse(string, Formats.RussianDateTimeFormat))
      } catch {
        case e: DateTimeParseException => Validated.invalidNel(
          s"Некорректный формат даты-времени: $string; пример допустимого формата: 21.12.2018 22:00.")
      }

    def defaultMetavar: String = "время"
  }

  implicit private val readChar: Argument[Char] = new Argument[Char] {
    def read(string: String): ValidatedNel[String, Char] =
      string.toSeq match {
        case Seq(c) => Validated.valid(c)
        case _      => Validated.invalidNel(s"Некорректный символ: '$string'")
      }

    def defaultMetavar: String = "символ"
  }

  implicit private val readBoolean: Argument[Boolean] = new Argument[Boolean] {
    def read(string: String): ValidatedNel[String, Boolean] =
      string.toLowerCase.trim match {
        case "y" => Validated.valid(true)
        case "n" => Validated.valid(false)
        case _      => Validated.invalidNel(s"Некорректное значение: '$string'")
      }

    def defaultMetavar: String = "y|n"
  }

  private val csvFormat = Opts.option[CSVFormat]("data-format", short = "f",
    help = "формат файла с группами точек подключения, один из: csv, csv-excel, csv-rfc4180; по умолчанию csv")
    .withDefault(CSVFormat.DEFAULT)

  private val delimiter = Opts.option[Char]("delimiter", short = "d",
    help = "Символ-разделитель колонок, по умолчанию запятая")
    .withDefault(',')

  private val withHeader = Opts.flag("with-header", short = "h",
    help = "Указать, если у CSV файла с группами есть заголовок").orFalse

  private val dataFile = Opts.argument[Path]("файл с группами")
    .mapValidated { p =>
      if (!Files.exists(p))
        Validated.invalidNel(s"Файл не найден: $p")
      else
        Validated.valid(p)
    }

  private val updateAt = Opts.argument[LocalDateTime]("время обновления")

  private def readArgs: Opts[Config] = (csvFormat, delimiter, withHeader, dataFile, updateAt).mapN(Config)

  val appCmd: Command[Config] = Command("ap-update",
    "Последовательное обновление точек доступа Wi-Fi", helpFlag = false)(readArgs)
}
