package de.sciss.mellite

import java.io.File
import scala.concurrent.Future
import impl.{TransformImpl => Impl}
import de.sciss.processor.Processor
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}
import scala.annotation.switch

object Transform {
  private final val COOKIE  = 0x54726E7366726D00L  // "Trnsfrm\0"

  case object Unmodified extends Transform

  final case class Coded(source: String) extends Transform {
    def perform(in: File, out: File, procHandler: Processor[Unit, _] => Unit = _ => ()): Future[Unit] =
      Impl.perform(in, out, source, procHandler)
  }

  final case class CompilationFailed() extends Exception
  final case class CodeIncomplete()    extends Exception

  implicit object Serializer extends ImmutableSerializer[Transform] {
    def write(v: Transform, out: DataOutput) {
      out.writeLong(COOKIE)
      v match {
        case Unmodified   => out.writeByte(0)
        case Coded(source) =>
          out.writeByte(1)
          out.writeUTF(source)
      }
    }

    def read(in: DataInput): Transform = {
      val cookie = in.readLong()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      (in.readByte(): @switch) match {
        case 0 => Unmodified
        case 1 =>
          val source = in.readUTF()
          Coded(source)
      }
    }
  }
}
sealed trait Transform