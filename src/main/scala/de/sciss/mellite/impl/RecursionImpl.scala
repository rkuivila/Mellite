package de.sciss
package mellite
package impl

import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{InMemory, Grapheme, Sys, ProcGroup, Artifact}
import de.sciss.lucre.{expr, event => evt}
import expr.Expr
import de.sciss.span.SpanLike
import de.sciss.synth.expr.{Doubles, Longs, SpanLikes, ExprImplicits}
import scala.collection.immutable.IndexedSeq
import de.sciss.serial.{ImmutableSerializer, DataInput, DataOutput}
import de.sciss.synth.proc.impl.CommonSerializers
import de.sciss.lucre.event.{Pull, Targets}
import scala.annotation.switch

object RecursionImpl {
  import Recursion.Channels

  private final val COOKIE  = 0x5265    // "Re"

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Recursion[S]] with evt.Reader[S, Recursion[S]] =
    anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  implicit object RangeSer extends ImmutableSerializer[Range.Inclusive] {
    def write(v: Range.Inclusive, out: DataOutput) {
      if (v.start == v.end) {
        out.writeByte(0)
        out.writeInt(v.start)
      } else {
        out.writeByte(1)
        out.writeInt(v.start)
        out.writeInt(v.end  )
        out.writeInt(v.step )
      }
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
      val span        = SpanLikes.readVar(in, access)
      val id          = targets.id
      val gain        = tx.readVar[Gain    ](id, in)
      val channels    = tx.readVar[Channels](id, in)(ImmutableSerializer.indexedSeq[Range.Inclusive])
      //      val deployed    = Grapheme.Elem.Audio.readExpr(in, access) match {
      //        case ja: Grapheme.Elem.Audio[S] => ja // XXX TODO sucky shit
      //      }
      val deployed    = Element.serializer[S].read(in, access) match {
        case a: Element.AudioGrapheme[S]  => a
        case other => sys.error(s"What the... expected an audio grapheme but got $other")
      }

      val product     = Artifact.Modifiable.read(in, access)
      val productSpec = AudioFileSpec.Serializer.read(in)
      new Impl(targets, group, span, gain, channels, deployed, product, productSpec)
    }
  }

  def apply[S <: Sys[S]](group: ProcGroup[S], span: SpanLike, deployed: Element.AudioGrapheme[S],
                         gain: Gain, channels: Channels)(implicit tx: S#Tx): Recursion[S] = {
    val imp = ExprImplicits[S]
    import imp._

    val targets   = evt.Targets[S]
    val id        = targets.id
    val _span     = SpanLikes.newVar(span)
    val _gain     = tx.newVar(id, gain)
    val _channels = tx.newVar(id, channels)(ImmutableSerializer.indexedSeq[Range.Inclusive])
    //    val depArtif  = Artifact.Modifiable(artifact.location, artifact.value)
    //    val depOffset = Longs  .newVar(0L)
    //    val depGain   = Doubles.newVar(1.0)
    //    val deployed  = Grapheme.Elem.Audio.apply(depArtif, spec, depOffset, depGain)

    val product   = deployed.entity.artifact
    val spec      = deployed.entity.value.spec  // XXX TODO: should that be a method on entity?

    new Impl(targets, group, _span, _gain, _channels, deployed, product = product, productSpec = spec)
  }

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S], val group: ProcGroup[S],
    _span: Expr.Var[S, SpanLike], _gain: S#Var[Gain], _channels: S#Var[Channels],
    val deployed: Element.AudioGrapheme[S] /* Grapheme.Elem.Audio[S] */, val product: Artifact[S],
    val productSpec: AudioFileSpec)
    extends Recursion[S]
    with evt.impl.Generator     [S, Recursion.Update[S], Recursion[S]]
    with evt.impl.StandaloneLike[S, Recursion.Update[S], Recursion[S]] {

    def span(implicit tx: S#Tx): SpanLike = _span.value
    def span_=(value: SpanLike)(implicit tx: S#Tx) {
      val imp = ExprImplicits[S]
      import imp._
      _span() = value
    }

    def gain(implicit tx: S#Tx): Gain = _gain()
    def gain_=(value: Gain)(implicit tx: S#Tx) {
      _gain() = value
      fire()
    }

    def channels(implicit tx: S#Tx): Channels = _channels()
    def channels_=(value: Channels)(implicit tx: S#Tx) {
      _channels() = value
      fire()
    }

    /** Moves the product to deployed position. */
    def iterate()(implicit tx: S#Tx) {
      val mod = deployed.entity.artifact.modifiableOption.getOrElse(
        sys.error("Can't iterate - deployed artifact not modifiable")
      )
      mod.child_=(product.value)
    }

    // ---- event dorfer ----

    def changed: evt.EventLike[S, Recursion.Update[S], Recursion[S]] = this

    protected def reader: evt.Reader[S, Recursion[S]] = serializer

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Recursion.Update[S]] = {
      if (pull.isOrigin(this)) return Some()

      val spanEvt = _span.changed
      val spanUpd = if (pull.contains(spanEvt)) pull(spanEvt) else None
      if (spanUpd.isDefined) return Some()

      val gainEvt = _span.changed
      val gainUpd = if (pull.contains(gainEvt)) pull(gainEvt) else None
      if (gainUpd.isDefined) return Some()

      val prodEvt = product.changed
      val prodUpd = if (pull.contains(prodEvt)) pull(prodEvt) else None
      if (prodUpd.isDefined) return Some()

      None
    }

    protected def writeData(out: DataOutput) {
      out.writeShort(COOKIE)
      group    .write(out)
      _span    .write(out)
      _gain    .write(out)
      _channels.write(out)
      deployed .write(out)
      product  .write(out)
      AudioFileSpec.Serializer.write(productSpec, out)
    }

    protected def disposeData()(implicit tx: S#Tx) {
      // group: NO
      _span    .dispose()
      _gain    .dispose()
      _channels.dispose()
    }

    def connect()(implicit tx: S#Tx) {
      // ignore group
      _span   .changed ---> this
      deployed.changed ---> this
      product .changed ---> this
    }

    def disconnect()(implicit tx: S#Tx) {
      _span   .changed -/-> this
      deployed.changed -/-> this
      product .changed -/-> this
    }
  }
}