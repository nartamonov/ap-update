package ap_update.console

import cats.Show
import cats.effect.IO
import cats.syntax.show._
import org.fusesource.jansi.{Ansi, AnsiConsole}

trait AnsiSupport {
  def withAnsiConsole[T](use: IO[T]): IO[T] =
    IO(AnsiConsole.systemInstall()).bracket(_ => use)(_ => IO(AnsiConsole.systemUninstall()))

  def ansiStr(builder: Ansi => Ansi): String = builder(Ansi.ansi()).toString

  def green[A : Show](s: A): String = ansiStr(_.fgGreen().a(s.show).reset())
  def brightRed[A : Show](s: A): String = ansiStr(_.fgBrightRed().a(s.show).reset())
}
