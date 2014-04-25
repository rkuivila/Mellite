/*
 *  Code.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite

import de.sciss.serial.{Serializer, Writable, DataInput, DataOutput, ImmutableSerializer}
import impl.{CodeImpl => Impl, CodeImpl2 => Impl2}
import java.io.File
import scala.concurrent.Future
import de.sciss.processor.Processor
import de.sciss.synth
import scala.annotation.switch
import de.sciss.synth.proc
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.synth.proc.Obj

object Code {
  final case class CompilationFailed() extends Exception
  final case class CodeIncomplete()    extends Exception

  implicit def serializer: ImmutableSerializer[Code] = Impl.serializer

  def read(in: DataInput): Code = serializer.read(in)

  def apply(id: Int, source: String): Code = (id: @switch) match {
    case FileTransform.id => FileTransform(source)
    case SynthGraph   .id => SynthGraph   (source)
  }

  object FileTransform {
    final val id    = 0
    final val name  = "File Transform"
  }
  final case class FileTransform(source: String) extends Code {
    type In     = (File, File, Processor[Any, _] => Unit)
    type Out    = Future[Unit]
    def id      = FileTransform.id

    def compileBody(): Future[Unit] = Impl2.compileBody[In, Out, FileTransform](this)

    def execute(in: In): Out = Impl2.execute[In, Out, FileTransform](this, in)

    def contextName = FileTransform.name
  }

  object SynthGraph {
    final val id    = 1
    final val name  = "Synth Graph"
  }
  final case class SynthGraph(source: String) extends Code {
    type In     = Unit
    type Out    = synth.SynthGraph
    def id      = SynthGraph.id

    def compileBody(): Future[Unit] = Impl2.compileBody[In, Out, SynthGraph](this)

    def execute(in: In): Out = Impl2.execute[In, Out, SynthGraph](this, in)

    def contextName = SynthGraph.name
  }

  // ---- element ----
  object Elem {
    def apply[S <: Sys[S]](peer: Expr[S, Code])(implicit tx: S#Tx): Elem[S] = ???

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Code.Elem[S]] = ???

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, Code.Elem]] =
        if (obj.elem.isInstanceOf[Code.Elem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, Code.Elem]])
        else None
    }

    // implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Folder[S]] = ...
  }

  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer = Expr[S, Code]

    def mkCopy()(implicit tx: S#Tx): Elem[S]
  }
}
sealed trait Code extends Writable {
  /** The interfacing input type */
  type In
  /** The interfacing output type */
  type Out

  /** Identifier to distinguish types of code. */
  def id: Int

  /** Source code. */
  def source: String

  /** Human readable name. */
  def contextName: String

  /** Compiles the code body without executing it. */
  def compileBody(): Future[Unit]

  /** Compiles and executes the code. Returns the wrapped result. */
  def execute(in: In): Out // = compile()(in)

  def write(out: DataOutput): Unit = Code.serializer.write(this, out)
}