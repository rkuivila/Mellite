package de.sciss.mellite

import de.sciss.serial.{Writable, DataInput, DataOutput, ImmutableSerializer}
import scala.annotation.switch
import impl.{CodeImpl => Impl}
import java.io.File
import scala.concurrent.Future
import de.sciss.processor.Processor

object Code {
  final case class CompilationFailed() extends Exception
  final case class CodeIncomplete()    extends Exception

  implicit def serializer: ImmutableSerializer[Code] = Impl.serializer

  def read(in: DataInput): Code = serializer.read(in)

  object FileTransform {
    final val id = 0
  }
  final case class FileTransform(source: String) extends Code {
    type In     = (File, File, Processor[Any, _] => Unit)
    type Out    = Future[Unit]
    def id      = FileTransform.id

    def compile(): In => Out = Impl.compile[In, Out, FileTransform](this)
  }
}
sealed trait Code extends Writable {
  /** The interfacing input type */
  type In
  /** The interfacing output type */
  type Out

  def id: Int
  def source: String

  def compile(): In => Out

  def execute(in: In): Out = compile()(in)

  def write(out: DataOutput) {
    Code.serializer.write(this, out)
  }
}