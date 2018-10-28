package ap_update.console

import ap_update.console.ansi.{green, brightRed}
import cats.{FlatMap, Show}
import cats.effect.{Console, Sync}
import cats.syntax.show._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.string._
import cats.instances.vector._

trait TableSupport {

  sealed trait Color {
    def apply(str: String): String = this match {
      case Color.Red => brightRed(str)
      case Color.Green => green(str)
    }
  }

  object Color {

    case object Green extends Color

    case object Red extends Color

  }

  case class Cell(value: String, fgColor: Option[Color]) {
    def fitTo(width: Int): String = {
      val formattedValue = if (value.length > width + 2)
        value.take(width - 3) + "..."
      else
        value.padTo(width, ' ')

      fgColor.fold(formattedValue)(col => col(formattedValue))
    }
  }

  object Cell {
    implicit def string2Cell(str: String): Cell = Cell(str, None)

    implicit def fromShow[A: Show](x: A): Cell = Cell(x.show, None)
  }

  case class Column(header: String, width: Int)

  trait ConsoleTable[F[_]] {
    final def print(implicit ev: FlatMap[F]): F[Unit] = {
      for {
        _ <- putHeader
        _ <- putRows
        _ <- putBorder
      } yield {}
    }

    protected def putHeader: F[Unit]

    protected def putRows: F[Unit]

    protected def putBorder: F[Unit]
  }

  case class ConsoleTable5[F[_] : Sync : Console](columns: (Column, Column, Column, Column, Column), rows: Vector[ConsoleTable5.Row]) extends ConsoleTable[F] {
    def addRow(row: ConsoleTable5.Row): ConsoleTable5[F] = ConsoleTable5(columns, rows :+ row)

    private def putRow(row: ConsoleTable5.Row): F[Unit] =
      Console[F].putStrLn(s"| ${row._1.fitTo(columns._1.width)} | ${row._2.fitTo(columns._2.width)} | ${row._3.fitTo(columns._3.width)} | ${row._4.fitTo(columns._4.width)} | ${row._5.fitTo(columns._5.width)} |")

    override protected def putHeader: F[Unit] =
      for {
        _ <- putBorder
        _ <- putRow((columns._1.header, columns._2.header, columns._3.header, columns._4.header, columns._5.header))
        _ <- putBorder
      } yield {}

    override protected def putRows: F[Unit] = rows.traverse(putRow).void

    override protected def putBorder: F[Unit] = {
      def border(width: Int): String = "-" * (width + 2)

      Console[F].putStrLn(s"+${border(columns._1.width)}+${border(columns._2.width)}+${border(columns._3.width)}+${border(columns._4.width)}+${border(columns._5.width)}+")
    }
  }

  object ConsoleTable5 {
    type Row = (Cell, Cell, Cell, Cell, Cell)
  }

  object ConsoleTable {
    def apply[F[_] : Sync : Console](columns: (Column, Column, Column, Column, Column)): ConsoleTable5[F] =
      ConsoleTable5(columns, Vector.empty)
  }
}
