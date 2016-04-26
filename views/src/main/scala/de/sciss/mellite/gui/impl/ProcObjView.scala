/*
 *  ProcObjView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.file._
import de.sciss.fingertree.RangedSeq
import de.sciss.icons.raphael
import de.sciss.lucre.expr.{IntObj, SpanLikeObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj, TxnLike}
import de.sciss.lucre.swing.{Window, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.sonogram.{Overview => SonoOverview}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, AuxContext, ObjKeys, Proc, TimeRef}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TSet}
import scala.language.implicitConversions
import scala.swing.Graphics2D
import scala.util.control.NonFatal

object ProcObjView extends ListObjView.Factory with TimelineObjView.Factory {
  type E[~ <: stm.Sys[~]] = Proc[~]

  val icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Cogs)
  val prefix    = "Proc"
  val humanName = "Process"
  def tpe       = Proc
  def category  = ObjView.categComposition

  def hasMakeDialog = true

  def mkListView[S <: Sys[S]](obj: Proc[S])(implicit tx: S#Tx): ProcObjView[S] with ListObjView[S] =
    new ListImpl(tx.newHandle(obj)).initAttrs(obj)

  type Config[S <: stm.Sys[S]] = String

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
      messageType = OptionPane.Message.Question, initial = prefix)
    opt.title = s"New $prefix"
    val res = opt.show(window)
    res
  }

  def makeObj[S <: Sys[S]](name: String)(implicit tx: S#Tx): List[Obj[S]] = {
    val obj  = Proc[S]
    obj.name = name
    obj :: Nil
  }

  type LinkMap[S <: stm.Sys[S]] = Map[String, Vec[ProcObjView.Link[S]]]
  type ProcMap[S <: stm.Sys[S]] = IdentifierMap[S#ID, S#Tx, ProcObjView[S]]
  type ScanMap[S <: stm.Sys[S]] = IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])]

  type SelectionModel[S <: Sys[S]] = gui.SelectionModel[S, ProcObjView[S]]

  // private final val DEBUG = false

//  private def addLink[A, B](map: Map[A, Vec[B]], key: A, value: B): Map[A, Vec[B]] =
//    map + (key -> (map.getOrElse(key, Vec.empty) :+ value))
//
//  private def removeLink[A, B](map: Map[A, Vec[B]], key: A, value: B): Map[A, Vec[B]] = {
//    import de.sciss.equal.Implicits._
//    val newVec = map.getOrElse(key, Vec.empty).filterNot(_ === value)
//    if (newVec.isEmpty) map - key else map + (key -> newVec)
//  }

  /** Constructs a new proc view from a given proc, and a map with the known proc (views).
    * This will automatically add the new view to the map!
    */
  def mkTimelineView[S <: Sys[S]](timedID: S#ID, span: SpanLikeObj[S], obj: Proc[S],
                                  context: TimelineObjView.Context[S])(implicit tx: S#Tx): ProcObjView.Timeline[S] = {
    // val proc    = obj
    // val inputs  = proc.inputs
    // val outputs = proc.outputs

    val attr    = obj.attr
    val bus     = attr.$[IntObj](ObjKeys.attrBus    ).map(_.value)
    val res = new TimelineImpl[S](tx.newHandle(obj), busOption = bus, context = context)
      .init(timedID, span, obj)

//    lazy val idH = tx.newHandle(timedID)
//
//    import context.{scanMap, viewMap}
//
//    def buildLinks(isInput: Boolean): Unit = {
//      val scans = if (isInput) inputs else outputs
//      scans.iterator.foreach { case (key, scan) =>
//        if (DEBUG) println(s"PV $timedID add scan ${scan.id}, $key")
//        scanMap.put(scan.id, key -> idH)
//        val it = scan.iterator
//        it.foreach {
//          case Scan.Link.Scan(peer) if scanMap.contains(peer.id) =>
//            val Some((thatKey, thatIdH)) = scanMap.get(peer.id)
//            val thatID = thatIdH()
//            viewMap.get(thatID).foreach {
//              case thatView: ProcObjView.Timeline[S] =>
//                if (DEBUG) println(s"PV $timedID add link from $key to $thatID, $thatKey")
//                if (isInput) {
//                  res     .addInput (key    , thatView, thatKey)
//                  thatView.addOutput(thatKey, res     , key    )
//                } else {
//                  res     .addOutput(key    , thatView, thatKey)
//                  thatView.addInput (thatKey, res     , key    )
//                }
//
//              case _ =>
//            }
//
//          case other =>
//            if (DEBUG) other match {
//              case Scan.Link.Scan(peer) =>
//                println(s"PV $timedID missing link from $key to scan $peer")
//              case _ =>
//            }
//        }
//      }
//    }
//    buildLinks(isInput = true )
//    buildLinks(isInput = false)
    // println("WARNING: ProcObjView.mkTimelineView - buildLinks not yet implemented")  // SCAN

    // procMap.put(timed.id, res)
    res
  }

  // -------- Proc --------

  private final class ListImpl[S <: Sys[S]](val objH: stm.Source[S#Tx, Proc[S]])
    extends Impl[S]

  private trait Impl[S <: Sys[S]]
    extends ListObjView[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.EmptyRenderer[S]
    with ListObjViewImpl.NonEditable[S]
    with ProcObjView[S] {

    override def obj(implicit tx: S#Tx): Proc[S] = objH()

    final def factory = ProcObjView

    final def isViewable = true

    // currently this just opens a code editor. in the future we should
    // add a scans map editor, and a convenience button for the attributes
    final def openView(parent: Option[Window[S]])
                      (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      import de.sciss.mellite.Mellite.compiler
      val frame = CodeFrame.proc(obj)
      Some(frame)
    }
  }

  private trait InputAttr[S <: Sys[S]] extends Disposable[S#Tx] {
    def paintInputAttr(g: Graphics2D): Unit
  }

  private final class InputAttrTimeline[S <: Sys[S]](parent: ProcObjView.Timeline[S],
                                                     tl: proc.Timeline[S], tx0: S#Tx)
    extends InputAttr[S] {

    // source views are updated by calling `copy` as they appear and disappear
    private final class Elem(val span: SpanLike, val source: Option[ProcObjView.Timeline[S]],
                             obs: Disposable[S#Tx]) extends Disposable[S#Tx] {
      def point: (Long, Long) = TimelineObjView.spanToPoint(span)

      def dispose()(implicit tx: S#Tx): Unit = obs.dispose()

      def copy(newSource: Option[ProcObjView.Timeline[S]]): Elem =
        new Elem(span = span, source = newSource, obs = obs)
    }

    private[this] val viewMap   = tx0.newInMemoryIDMap[Elem]
    private[this] val viewSet   = TSet.empty[Elem]  // because `viewMap.iterator` does not exist...

    // EDT
    private[this] var rangeSeq  = RangedSeq.empty[Elem, Long](_.point, Ordering.Long)

    private[this] val observer: Disposable[S#Tx] =
      tl.changed.react { implicit tx => upd => upd.changes.foreach {
        case proc.Timeline.Added  (span  , entry) => addAttrIn   (span, entry, fire = true)
        case proc.Timeline.Removed(span  , entry) => removeAttrIn(span, entry)
        case proc.Timeline.Moved  (spanCh, entry) =>
          removeAttrIn(spanCh.before, entry)
          addAttrIn   (spanCh.now   , entry, fire = true)
      }} (tx0)

    // init
    tl.iterator(tx0).foreach { case (span, xs) =>
      xs.foreach(addAttrIn(span, _, fire = false)(tx0))
    }

    def paintInputAttr(g: Graphics2D): Unit = {
      println(s"paintInputAttr(${rangeSeq.iterator.size})")
    }

    private def addAttrIn(span: SpanLike, entry: proc.Timeline.Timed[S], fire: Boolean)(implicit tx: S#Tx): Unit =
      entry.value match {
        case out: proc.Output[S] =>
          import TxnLike.peer
          val idH      = tx.newHandle(entry.id)
          val viewInit = parent.context.getAux[ProcObjView.Timeline[S]](out.id)
          val obs  = parent.context.observeAux[ProcObjView.Timeline[S]](out.id) { implicit tx => upd =>
            val id = idH()
            viewMap.get(id).foreach { elem1 =>
              // elem2 keeps the observer, so no `dispose` call here
              val elem2 = upd match {
                case AuxContext.Added(_, sourceView)  => elem1.copy(Some(sourceView))
                case AuxContext.Removed(_)            => elem1.copy(None)
              }
              viewMap.put(id, elem2)  // replace
              viewSet.remove(elem1)
              viewSet.add   (elem2)
              deferTx {
                rangeSeq -= elem1
                rangeSeq += elem2
              }
              parent.fireRepaint()
            }
          }
          val elem0 = new Elem(span, viewInit, obs)
          viewMap.put(entry.id, elem0)
          viewSet.add(elem0)
          deferTx {
            rangeSeq += elem0
          }
          if (fire) parent.fireRepaint()

        case _ => // no others supported ATM
      }

    private def removeAttrIn(span: SpanLike, entry: proc.Timeline.Timed[S])(implicit tx: S#Tx): Unit =
      viewMap.get(entry.id).foreach { elem0 =>
        import TxnLike.peer
        viewMap.remove(entry.id)
        viewSet.remove(elem0)
        deferTx {
          rangeSeq -= elem0
        }
        elem0.dispose()
        parent.fireRepaint()
      }

    def dispose()(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      observer.dispose()
      viewSet.foreach(_.dispose())
      viewSet.clear()
    }
  }

  private final class TimelineImpl[S <: Sys[S]](val objH: stm.Source[S#Tx, Proc[S]],
                                                var busOption : Option[Int], val context: TimelineObjView.Context[S])
    extends Impl[S]
    with TimelineObjViewImpl.HasGainImpl[S]
    with TimelineObjViewImpl.HasMuteImpl[S]
    with TimelineObjViewImpl.HasFadeImpl[S]
    with ProcObjView.Timeline[S] { self =>

    override def toString = s"ProcView($name, $spanValue, $audio)"

    private[this] var audio         = Option.empty[AudioCue]
    private[this] var failedAcquire = false
    private[this] var sonogram      = Option.empty[SonoOverview]

    def debugString: String = {
      val basic1S = s"span = $spanValue, trackIndex = $trackIndex, nameOption = $nameOption"
      val basic2S = s"muted = $muted, audio = $audio"
      val basic3S = s"fadeIn = $fadeIn, fadeOut = $fadeOut, gain = $gain, busOption = $busOption"
      val procS   = s"ProcView($basic1S, $basic2S, $basic3S)"
      // val inputS   = inputs.mkString("  inputs  = [", ", ", "]\n")
      // val outputS  = outputs.mkString("  outputs = [", ", ", "]\n")
      // s"$procC\n$inputS$outputS"
      procS
    }

    def fireRepaint()(implicit tx: S#Tx): Unit = fire(ObjView.Repaint(this))

    def init(id: S#ID, span: SpanLikeObj[S], obj: Proc[S])(implicit tx: S#Tx): this.type = {
      initAttrs(id, span, obj)

      // XXX TODO -- should use a dynamic AttrCellView here
      val attr = obj.attr
      attr.$[AudioCue.Obj](Proc.graphAudio).foreach { audio0 =>
        disposables ::= audio0.changed.react { implicit tx => upd =>
          val newAudio = upd.now // calcAudio(upd.grapheme)
          deferAndRepaint {
            val newSonogram = upd.before.artifact != upd.now.artifact
            audio = Some(newAudio)
            if (newSonogram) releaseSonogram()
          }
        }
        audio = Some(audio0.value) // calcAudio(g)
      }

      // attr.iterator.foreach { case (key, value) => addAttr(key, value) }
      import Proc.mainIn
      attr.get(mainIn).foreach(addAttrIn(_, fire = false))
      disposables ::= attr.changed.react { implicit tx => upd => upd.changes.foreach {
        case Obj.AttrAdded   (`mainIn`, value) => addAttrIn(value, fire = true)
        case Obj.AttrRemoved (`mainIn`, value) => ???!
        case Obj.AttrReplaced(`mainIn`, before, now) => ???!
        case _ =>
      }}

      this
    }

    private[this] val attrInRef = Ref(Option.empty[InputAttr[S]])
    private[this] var attrInEDT =     Option.empty[InputAttr[S]]

    private[this] def addAttrIn(value: Obj[S], fire: Boolean)(implicit tx: S#Tx): Unit = value match {
      case tl: proc.Timeline[S] =>
        import TxnLike.peer
        // println("addAttrIn: Timeline")
        val tlView  = new InputAttrTimeline(this, tl, tx)
        val opt     =  Some(tlView)
        attrInRef.swap(opt).foreach(_.dispose())
        deferAndRepaint {
          attrInEDT = opt
        }

      case gr: proc.Grapheme[S] =>
        println("addAttrIn: Grapheme")
        ???!
      case f: proc.Folder  [S] =>
        println("addAttrIn: Folder")
        ???!
      case out: proc.Output  [S] =>
        println("addAttrIn: Output")
        ???!
      case _ =>
    }

    override def paintFront(g: Graphics2D, tlv: TimelineView[S], r: TimelineRendering): Unit =
      attrInEDT.foreach { attrInView =>
        attrInView.paintInputAttr(g)
      }

    // paint sonogram
    override protected def paintInner(g: Graphics2D, tlv: TimelineView[S], r: TimelineRendering,
                                      selected: Boolean): Unit =
      audio.foreach { audioVal =>
        val sonogramOpt = sonogram.orElse(acquireSonogram())

        sonogramOpt.foreach { sonogram =>
          val srRatio     = sonogram.inputSpec.sampleRate / TimeRef.SampleRate
          // dStart is the frame inside the audio-file corresponding
          // to the region's left margin. That is, if the grapheme segment
          // starts early than the region (its start is less than zero),
          // the frame accordingly increases.
          val dStart      = (audioVal.fileOffset /* offset - segm.span.start */ +
            (if (selected) r.ttResizeState.deltaStart else 0L)) * srRatio
          // a factor to convert from pixel space to audio-file frames
          val canvas      = tlv.canvas
          val s2f         = canvas.screenToFrames(1) * srRatio
          val lenC        = (px2c - px1c) * s2f
          val visualBoost = canvas.trackTools.visualBoost
          val boost       = if (selected) r.ttGainState.factor * visualBoost else visualBoost
          r.sonogramBoost = (audioVal.gain * gain).toFloat * boost
          val startP      = (px1c - px) * s2f + dStart
          val stopP       = startP + lenC
          val w1          = px2c - px1c
          // println(s"${pv.name}; audio.offset = ${audio.offset}, segm.span.start = ${segm.span.start}, dStart = $dStart, px1C = $px1C, startC = $startC, startP = $startP")
          // println(f"spanStart = $startP%1.2f, spanStop = $stopP%1.2f, tx = $px1c, ty = $pyi, width = $w1, height = $phi, boost = ${r.sonogramBoost}%1.2f")

          sonogram.paint(spanStart = startP, spanStop = stopP, g2 = g,
            tx = px1c, ty = pyi, width = w1, height = phi, ctrl = r)
        }
      }

    private[this] def releaseSonogram(): Unit =
      sonogram.foreach { ovr =>
        sonogram = None
        SonogramManager.release(ovr)
      }

    override def name = nameOption.getOrElse {
      audio.fold(TimelineObjView.Unnamed)(_./* value. */artifact.base)
    }

    private[this] def acquireSonogram(): Option[SonoOverview] = {
      if (failedAcquire) return None
      releaseSonogram()
      sonogram = audio.flatMap { audioVal =>
        try {
          val ovr = SonogramManager.acquire(audioVal./* value. */artifact)  // XXX TODO: remove `Try` once manager is fixed
          failedAcquire = false
          Some(ovr)
        } catch {
          case NonFatal(_) =>
          failedAcquire = true
          None
        }
      }
      sonogram
    }

    override def dispose()(implicit tx: S#Tx): Unit = {
      val proc = obj
      // SCAN
//      proc.inputs.iterator.foreach { case (_, scan) =>
//        context.scanMap.remove(scan.id)
//      }
      proc.outputs.iterator.foreach { case scan /* (_, scan) */ =>
        context.removeAux(scan.id)
      }
      deferTx(disposeGUI())
    }

    private[this] def disposeGUI(): Unit = {
      releaseSonogram()

//      def removeLinks(inp: Boolean): Unit = {
//        val map = if (inp) inputs else outputs
//        map.foreach { case (thisKey, links) =>
//          links.foreach { link =>
//            val thatView  = link.target
//            val thatKey   = link.targetKey
//            if (inp)
//              thatView.removeOutput(thatKey, self, thisKey)
//            else
//              thatView.removeInput (thatKey, self, thisKey)
//          }
//        }
//        if (inp) inputs = Map.empty else outputs = Map.empty
//      }
//
//      removeLinks(inp = true )
//      removeLinks(inp = false)
    }

    def isGlobal: Boolean = {
      import de.sciss.equal.Implicits._
      spanValue === Span.All
    }
  }

  final case class Link[S <: stm.Sys[S]](target: ProcObjView.Timeline[S], targetKey: String)

  /** A data set for graphical display of a proc. Accessors and mutators should
    * only be called on the event dispatch thread. Mutators are plain variables
    * and do not affect the underlying model. They should typically only be called
    * in response to observing a change in the model.
    */
  trait Timeline[S <: stm.Sys[S]]
    extends ProcObjView[S] with TimelineObjView[S]
    with TimelineObjView.HasMute
    with TimelineObjView.HasGain
    with TimelineObjView.HasFade {

    /** Convenience check for `span == Span.All` */
    def isGlobal: Boolean

    def context: TimelineObjView.Context[S]

    def fireRepaint()(implicit tx: S#Tx): Unit

    var busOption: Option[Int]

    def debugString: String
  }
}
trait ProcObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): Proc[S]

  def objH: stm.Source[S#Tx, Proc[S]]
}