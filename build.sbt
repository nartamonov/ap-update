lazy val apUpdate = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "ap-update",

    fork := true,

    mainClass in Compile := Some("ap_update.App"),

    assemblyJarName in assembly := "ap-update.jar",

    libraryDependencies ++= Seq(
      "org.typelevel"        %% "cats-effect"  % "1.0.0",
      "com.github.gvolpe"    %% "console4cats" % "0.3",
      "org.fusesource.jansi"  % "jansi"        % "1.17.1",
      "org.apache.commons"    % "commons-csv"  % "1.6",
      "com.monovore"         %% "decline"      % "0.5.1",

      "com.lihaoyi"          %% "pprint"       % "0.5.3",
      "org.scalactic"        %% "scalactic"    % "3.0.5",
      "org.scalatest"        %% "scalatest"    % "3.0.5" % "test"
    )
  )

lazy val commonSettings = Seq(
  version               := "0.1",
  startYear             := Some(2018),
  scalaVersion          := "2.12.7",
  scalacOptions         ++= Seq("-target:jvm-1.8", "-deprecation", "-unchecked", "-Xcheckinit", "-encoding", "utf8", "-feature"),
  scalacOptions         ++= Seq(
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-language:higherKinds"
  ),

  // configure prompt to show current project
  shellPrompt           := { s => Project.extract(s).currentProject.id + " > " },

  initialCommands in console :=
    """
      |import java.nio.file._
      |import scala.concurrent._
      |import scala.concurrent.duration._
      |import ExecutionContext.Implicits.global
    """.stripMargin
)
