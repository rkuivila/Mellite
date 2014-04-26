/*
 *  RecursionImpl.scala
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

package de.sciss
package mellite
package impl

import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{Obj, AudioGraphemeElem, ExprImplicits, ProcGroup, Artifact}
import de.sciss.lucre.{expr, event => evt}
import expr.Expr
import de.sciss.span.SpanLike
import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput}
import scala.annotation.switch
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.event.Sys
import de.sciss.synth.proc.impl.ElemImpl

object RecursionImpl {
  import Recursion.Channels

  private final val COOKIE  = 0x5265    // "Re"

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Recursion[S]] with evt.Reader[S, Recursion[S]] =
    anySer.asInstanceOf[Ser[S]]

  // ---- elem ----

  object RecursionElemImpl extends ElemImpl.Companion[Recursion.Elem] {
    final val typeID = 0x20000

    def apply[S <: Sys[S]](peer: Recursion[S])(implicit tx: S#Tx): Recursion.Elem[S] = {
      val targets = evt.Targets[S]
      new Impl[S](targets, peer)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Recursion.Elem[S] =
      serializer[S].read(in, access)

    // ---- Elem.Extension ----

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): Recursion.Elem[S] with evt.Node[S] = {
      val peer = Recursion.read(in, access)
      new Impl[S](targets, peer)
    }

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Recursion.Elem[S] =
      sys.error("Constant Recursion not supported")

    // ---- implementation ----

    private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                          val peer: Recursion[S])
      extends Recursion.Elem[S]
      with ElemImpl.Active[S] {

      def typeID = RecursionElemImpl.typeID
      def prefix = "Recursion"

      override def toString() = s"$prefix$id"

      def mkCopy()(implicit tx: S#Tx): Recursion.Elem[S] = Recursion.Elem(peer)
    }
  }

  // ---- impl ----

  private val anySer = new Ser[InMemory]

  implicit object RangeSer extends ImmutableSerializer[Range.Inclusive] {
    def write(v: Range.Inclusive, out: DataOutput): Unit =
      if (v.start == v.end) {
        out.writeByte(0)
        out.writeInt(v.start)
      } else {
        out.writeByte(1)
        out.writeInt(v.start)
        out.writeInt(v.end  )
        out.writeInt(v.step )
      }

    def read(in: DataInput): Range.Inclusive = {
      (in.readByte(): @switch) match {
        case 0  =>
          val start = in.readInt()
          new Range.Inclusive(start, start, 1)
        case 1  =>
          val start = in.readInt()
          val end   = in.readInt()
          val step  = in.readInt()
          new Range.Inclusive(start, end, step)
      }
    }
  }

  private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Recursion[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Recursion[S] with evt.Node[S] = {
      val cookie  = in.readShort()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      val group       = ProcGroup.read(in, access)
      val span        = SpanLikeEx.readVar(in, access)
      val id          = targets.id
      val gain        = tx.readVar[Gain    ](id, in)
      val channels    = tx.readVar[Channels](id, in)(ImmutableSerializer.indexedSeq[Range.Inclusive])
      val transform   = serial.Serializer.option[S#Tx, S#Acc, Obj.T[S, Code.Elem]].read(in, access)
      //      val deployed    = Grapheme.Elem.Audio.readExpr(in, access) match {
      //        case ja: Grapheme.Elem.Audio[S] => ja // XXX TODO sucky shit
      //      }
      val deployed    = Obj.readT[S, AudioGraphemeElem](in, access)
      val product     = Artifact.Modifiable.read(in, access)
      val productSpec = AudioFileSpec.Serializer.read(in)
      new Impl[S](targets, group, span, gain, channels, transform, deployed, product, productSpec)
    }
  }

  def apply[S <: Sys[S]](group: ProcGroup[S], span: SpanLike, deployed: Obj.T[S, AudioGraphemeElem],
                         gain: Gain, channels: Channels, transform: Option[Obj.T[S, Code.Elem]])
                        (implicit tx: S#Tx): Recursion[S] = {
    val imp = ExprImplicits[S]
    import imp._

    val targets   = evt.Targets[S]
    val id        = targets.id
    val _span     = SpanLikeEx.newVar(span)
    val _gain     = tx.newVar(id, gain)
    val _channels = tx.newVar(id, channels)(ImmutableSerializer.indexedSeq[Range.Inclusive])

    //    val depArtif  = Artifact.Modifiable(artifact.location, artifact.value)
    //    val depOffset = Longs  .newVar(0L)
    //    val depGain   = Doubles.newVar(1.0)
    //    val deployed  = Grapheme.Elem.Audio.apply(depArtif, spec, depOffset, depGain)

    val depGraph  = deployed.elem.peer
    val product   = Artifact.Modifiable.copy(depGraph.artifact)
    val spec      = depGraph.value.spec  // XXX TODO: should that be a method on entity?

    new Impl[S](targets, group, _span, _gain, _channels, transform, deployed, product = product, productSpec = spec)
  }

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S], val group: ProcGroup[S],
      _span          : Expr.Var[S, SpanLike],
      _gain          : S#Var[Gain],
      _channels      : S#Var[Channels],
      val transform  : /* S#Var[ */ Option[Obj.T[S, Code.Elem]],
      val deployed   : Obj.T[S, AudioGraphemeElem],
      val product    : Artifact[S],
      val productSpec: AudioFileSpec)
    extends Recursion[S]
    with evt.impl.Generator     [S, Recursion.Update[S], Recursion[S]]
    with evt.impl.StandaloneLike[S, Recursion.Update[S], Recursion[S]] {

    def span(implicit tx: S#Tx): SpanLike = _span.value
    def span_=(value: SpanLike)(implicit tx: S#Tx): Unit = {
      val imp = ExprImplicits[S]
      import imp._
      _span() = value
    }

    def gain(implicit tx: S#Tx): Gain = _gain()
    def gain_=(value: Gain)(implicit tx: S#Tx): Unit = {
      _gain() = value
      fire(())
    }

    def channels(implicit tx: S#Tx): Channels = _channels()
    def channels_=(value: Channels)(implicit tx: S#Tx): Unit = {
      _channels() = value
      fire(())
    }

    //    def transform(implicit tx: S#Tx): Option[Element.Code[S]] = _transform()
    //    def transform_=(value: Option[Element.Code[S]])(implicit tx: S#Tx): Unit = {
    //      _transform() = value
    //      fire()
    //    }

    /** Moves the product to deployed position. */
    def iterate()(implicit tx: S#Tx): Unit = {
      val mod = deployed.elem.peer.artifact.modifiableOption.getOrElse(
        sys.error("Can't iterate - deployed artifact not modifiable")
      )
      val prodF = product.value
      val prodC = Artifact.relativize(mod.location.directory, prodF)
      mod.child_=(prodC)
    }

    // ---- event ----

    def changed: evt.EventLike[S, Recursion.Update[S]] = this

    protected def reader: evt.Reader[S, Recursion[S]] = serializer

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Recursion.Update[S]] = {
      if (pull.isOrigin(this)) return Some(())

      val spanEvt = _span.changed
      val spanUpd = if (pull.contains(spanEvt)) pull(spanEvt) else None
      if (spanUpd.isDefined) return Some(())

      val depEvt = deployed.changed
      val depUpd = if (pull.contains(depEvt)) pull(depEvt) else None
      if (depUpd.isDefined) return Some(())

      val prodEvt = product.changed
      val prodUpd = if (pull.contains(prodEvt)) pull(prodEvt) else None
      if (prodUpd.isDefined) return Some(())

      transform.foreach { t =>
        val tEvt = t.changed
        val tUpd = if (pull.contains(tEvt)) pull(tEvt) else None
        if (tUpd.isDefined) return Some(())
      }

      None
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      group    .write(out)
      _span    .write(out)
      _gain    .write(out)
      _channels.write(out)
      serial.Serializer.option[S#Tx, S#Acc, Obj.T[S, Code.Elem]].write(transform, out)
      deployed .write(out)
      product  .write(out)
      AudioFileSpec.Serializer.write(productSpec, out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      // group: NO
      _span    .dispose()
      _gain    .dispose()
      _channels.dispose()
    }

    def connect()(implicit tx: S#Tx): Unit = {
      // ignore group
      _span   .changed ---> this
      deployed.changed ---> this
      product .changed ---> this
      transform.foreach(_.changed ---> this)
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      _span   .changed -/-> this
      deployed.changed -/-> this
      product .changed -/-> this
      transform.foreach(_.changed -/-> this)
    }
  }
}