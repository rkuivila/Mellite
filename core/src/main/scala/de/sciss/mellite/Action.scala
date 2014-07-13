package de.sciss.mellite

import de.sciss.lucre.{event => evt, stm}
import evt.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.serial.{Serializer, Writable}
import de.sciss.synth.proc
import impl.{ActionImpl => Impl}

import scala.concurrent.Future

object Action {
  def compile[S <: Sys[S]](source: Code.Action)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Future[Action[S]] =
    Impl.compile(source)

  object Var {
    def apply[S <: Sys[S]](init: Action[S])(implicit tx: S#Tx): Var[S] = ???

    def unapply[S <: Sys[S]](a: Action[S]): Option[Var[S]] =
      a match {
        case x: Var[S] => Some(x)
        case _ => None
      }
  }
  trait Var[S <: Sys[S]] extends Action[S] {
    def apply()(implicit tx: S#Tx): Action[S]
    def update(value: Action[S])(implicit tx: S#Tx): Unit
  }

  // ---- element ----
  object Elem {
    def apply[S <: Sys[S]](peer: Action[S])(implicit tx: S#Tx): Code.Elem[S] = ??? // Impl.CodeElemImpl(peer)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Code.Elem[S]] = ??? // Impl.CodeElemImpl.serializer
  }

  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer         = Action[S]
    type PeerUpdate   = Unit // model.Change[Code]

    def mkCopy()(implicit tx: S#Tx): Elem[S]
  }

  object Obj {
    def unapply[S <: Sys[S]](obj: proc.Obj[S]): Option[Action.Obj[S]] =
      if (obj.elem.isInstanceOf[Action.Elem[S]]) Some(obj.asInstanceOf[Action.Obj[S]])
      else None
  }
  type Obj[S <: Sys[S]] = proc.Obj.T[S, Action.Elem]
}
trait Action[S <: Sys[S]] extends Writable with Disposable[S#Tx] with evt.Publisher[S, Unit] {
  def execute()(implicit tx: S#Tx): Unit
}