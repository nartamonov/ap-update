import java.time._

val `GMT+3` = ZoneOffset.of("+03:00")
val `GMT+4` = ZoneOffset.of("+04:00")
val `GMT-1` = ZoneOffset.of("-01:00")

`GMT-1`.compareTo(`GMT+4`)
`GMT+4`.compareTo(`GMT+4`)
`GMT+4`.compareTo(`GMT+3`)

val t = LocalDateTime.parse("2018-01-10T10:15:30").atZone(ZoneId.systemDefault())
val t2 = t.withZoneSameInstant(ZoneOffset.of("+01:00"))

t.isEqual(t2)

val t3 = t.withZoneSameLocal(ZoneOffset.of("+01:00"))

t.isEqual(t3)
t3.isAfter(t)

t3.compareTo(t)

val now = ZonedDateTime.now()
val inLondon = now.withZoneSameLocal(ZoneOffset.of("+01:00"))

now.isBefore(inLondon)

Duration.between(now,inLondon)

"--".padTo(4,"-")