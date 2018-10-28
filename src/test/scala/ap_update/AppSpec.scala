package ap_update

import java.nio.file.Paths
import java.time.{LocalDateTime, ZoneOffset}

import org.apache.commons.csv.CSVFormat
import org.scalatest.{FunSpec, Inside, Matchers}

class AppSpec extends FunSpec with Matchers with Inside {
  import App._

  it("schedule") {
    val fc1 = APGroup("FC-1", ZoneOffset.of("+04:00"))
    val fc2 = APGroup("FC-2", ZoneOffset.of("-01:00"))
    val fc3 = APGroup("FC-3", ZoneOffset.of("+05:00"))
    val fc4 = APGroup("FC-4", ZoneOffset.of("+08:00"))
    val fc5 = APGroup("FC-5", ZoneOffset.of("+04:00"))
    val groups = Vector(fc1, fc2, fc3, fc4, fc5)

    val s = schedule(groups, LocalDateTime.parse("2018-01-10T10:15:30"))
    s.buckets should contain theSameElementsInOrderAs Vector(
      ScheduledBucket(LocalDateTime.parse("2018-01-10T10:15:30").atZone(ZoneOffset.of("+08:00")), Set(fc4)),
      ScheduledBucket(LocalDateTime.parse("2018-01-10T10:15:30").atZone(ZoneOffset.of("+05:00")), Set(fc3)),
      ScheduledBucket(LocalDateTime.parse("2018-01-10T10:15:30").atZone(ZoneOffset.of("+04:00")), Set(fc1, fc5)),
      ScheduledBucket(LocalDateTime.parse("2018-01-10T10:15:30").atZone(ZoneOffset.of("-01:00")), Set(fc2))
    )
  }

  describe("readGroupsFromCSVFile") {
    it("корректно читает валидный файл") {
      val sampleFile = Paths.get(getClass.getResource("sample-groups-excel.csv").toURI)
      val fmt = CSVFormat.EXCEL.withDelimiter(';')
      readGroupsFromCSVFile(sampleFile, fmt).unsafeRunSync() should contain theSameElementsInOrderAs Vector(
        APGroup("FC-1", ZoneOffset.ofHours(4)),
        APGroup("FC-2", ZoneOffset.ofHours(-1)),
        APGroup("FC-3", ZoneOffset.ofHours(5)),
        APGroup("FC-4", ZoneOffset.ofHours(8)),
        APGroup("FC-5", ZoneOffset.ofHours(4))
      )
    }

    it("обнаруживает проблему с несовпадающим символом разделитем") {
      val sampleFile = Paths.get(getClass.getResource("sample-groups-excel.csv").toURI)
      // Читаем файл, полагая, что в нем стандартный разделитель запятая, в то время как фактический разделитель - ';'
      val fmt = CSVFormat.EXCEL
      inside(readGroupsFromCSVFile(sampleFile, fmt).attempt.unsafeRunSync()) {
        case Left(GroupsReadingError(msg, _)) =>
          msg should include("неверный символ-разделитель")
      }
    }

    it("обнаруживает проблему с неверным форматом часовой зоны") {
      val sampleFile = Paths.get(getClass.getResource("sample-groups-invalid.csv").toURI)
      val fmt = CSVFormat.EXCEL.withDelimiter(';')
      inside(readGroupsFromCSVFile(sampleFile, fmt).attempt.unsafeRunSync()) {
        case Left(GroupsReadingError(msg, _)) =>
          msg should include("неверный формат смещения часовой зоны")
      }
    }
  }
}
