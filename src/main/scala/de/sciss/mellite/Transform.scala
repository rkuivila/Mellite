package de.sciss.mellite

import java.io.File
import scala.concurrent.Future

object Transform {
  case object Unmodified extends Transform

  final case class Coded(source: String) extends Transform {
    def perform(in: File, out: File): Future[Unit] = {

      ???
    }
  }
}
sealed trait Transform