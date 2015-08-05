/*
 *  ProcObjView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.icons.raphael
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.IdentifierMap
import de.sciss.lucre.swing.{Window, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.sonogram.{Overview => SonoOverview}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.ElemImpl
import de.sciss.synth.proc.{FadeSpec, Grapheme, IntElem, Obj, ObjKeys, Proc, Scan}
import org.scalautils.TypeCheckedTripleEquals

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions
import scala.util.control.NonFatal

object ProcObjView extends ListObjView.Factory with TimelineObjView.Factory {
  type E[S <: evt.Sys[S]] = Proc.Elem[S]

  val icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Cogs)
  val prefix    = "Proc"
  val humanName = "Process"
  def typeID    = ElemImpl.Proc.typeID
  def category  = ObjView.categComposition

  def hasMakeDialog = true

  def mkListView[S <: Sys[S]](obj: Obj.T[S, Proc.Elem])(implicit tx: S#Tx): ProcObjView[S] with ListObjView[S] =
    new ListImpl(tx.newHandle(obj)).initAttrs(obj)

  type Config[S <: evt.Sys[S]] = String

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
      messageType = OptionPane.Message.Question, initial = prefix)
    opt.title = s"New $prefix"
    val res = opt.show(window)
    res
  }

  def makeObj[S <: Sys[S]](name: String)(implicit tx: S#Tx): List[Obj[S]] = {
    import proc.Implicits._
    val peer  = Proc[S]
    val elem  = Proc.Elem(peer)
    val obj   = Obj(elem)
    obj.name = name
    obj :: Nil
  }

  type LinkMap[S <: evt.Sys[S]] = Map[String, Vec[ProcObjView.Link[S]]]
  type ProcMap[S <: evt.Sys[S]] = IdentifierMap[S#ID, S#Tx, ProcObjView[S]]
  type ScanMap[S <: evt.Sys[S]] = IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])]

  type SelectionModel[S <: Sys[S]] = gui.SelectionModel[S, ProcObjView[S]]

  private final val DEBUG = false

  private def addLink[A, B](map: Map[A, Vec[B]], key: A, value: B): Map[A, Vec[B]] =
    map + (key -> (map.getOrElse(key, Vec.empty) :+ value))

  private def removeLink[A, B](map: Map[A, Vec[B]], key: A, value: B): Map[A, Vec[B]] = {
    import TypeCheckedTripleEquals._
    val newVec = map.getOrElse(key, Vec.empty).filterNot(_ === value)
    if (newVec.isEmpty) map - key else map + (key -> newVec)
  }

  /** Constructs a new proc view from a given proc, and a map with the known proc (views).
    * This will automatically add the new view to the map!
    */
  def mkTimelineView[S <: Sys[S]](timedID: S#ID, span: Expr[S, SpanLike], obj: Proc.Obj[S],
                                  context: TimelineObjView.Context[S])(implicit tx: S#Tx): ProcObjView.Timeline[S] = {
    val spanV = span.value
    import SpanLikeEx._
    // println("--- scan keys:")
    // proc.scans.keys.foreach(println)

    // XXX TODO: DRY - use getAudioRegion, and nextEventAfter to construct the segment value
    val proc    = obj.elem.peer
    val inputs  = proc.inputs
    val outputs = proc.outputs
    val audio   = inputs.get(Proc.Obj.graphAudio).flatMap { scanW =>
      // println("--- has scan")
      scanW.iterator.flatMap {
        case Scan.Link.Grapheme(g) =>
          // println("--- scan is linked")
          spanV match {
            case Span.HasStart(frame) =>
              // println("--- has start")
              g.segment(frame) match {
                case Some(segm @ Grapheme.Segment.Audio(gSpan, _)) /* if (gspan.start == frame) */ => Some(segm)
                // case Some(Grapheme.Segment.Audio(gspan, _audio)) =>
                //   // println(s"--- has audio segment $gspan offset ${_audio.offset}}; proc $spanV")
                //   // if (gspan == spanV) ... -> no, because segment will give as a Span.From(_) !
                //   if (gspan.start == frame) Some(_audio) else None
                case _ => None
              }
            case _ => None
          }
        case _ => None
      } .toList.headOption
    }

    val attr    = obj.attr
    val bus     = attr[IntElem](ObjKeys.attrBus    ).map(_.value)
    val res = new TimelineImpl[S](tx.newHandle(obj), audio = audio, busOption = bus, context = context)
      .initAttrs(timedID, span, obj)

    TimelineObjViewImpl.initGainAttrs(span, obj, res)
    TimelineObjViewImpl.initMuteAttrs(span, obj, res)
    TimelineObjViewImpl.initFadeAttrs(span, obj, res)

    import de.sciss.lucre.synth.expr.IdentifierSerializer
    lazy val idH = tx.newHandle(timedID)

    import context.{scanMap, viewMap}

    def buildLinks(isInput: Boolean): Unit = {
      val scans = if (isInput) inputs else outputs
      scans.iterator.foreach { case (key, scan) =>
        if (DEBUG) println(s"PV $timedID add scan ${scan.id}, $key")
        scanMap.put(scan.id, key -> idH)
        val it = scan.iterator
        it.foreach {
          case Scan.Link.Scan(peer) if scanMap.contains(peer.id) =>
            val Some((thatKey, thatIdH)) = scanMap.get(peer.id)
            val thatID = thatIdH()
            viewMap.get(thatID).foreach {
              case thatView: ProcObjView.Timeline[S] =>
                if (DEBUG) println(s"PV $timedID add link from $key to $thatID, $thatKey")
                if (isInput) {
                  res     .addInput (key    , thatView, thatKey)
                  thatView.addOutput(thatKey, res     , key    )
                } else {
                  res     .addOutput(key    , thatView, thatKey)
                  thatView.addInput (thatKey, res     , key    )
                }

              case _ =>
            }

          case other =>
            if (DEBUG) other match {
              case Scan.Link.Scan(peer) =>
                println(s"PV $timedID missing link from $key to scan $peer")
              case _ =>
            }
        }
      }
    }
    buildLinks(isInput = true )
    buildLinks(isInput = false)

    // procMap.put(timed.id, res)
    res
  }

  // -------- Proc --------

  private final class ListImpl[S <: Sys[S]](val objH: stm.Source[S#Tx, Proc.Obj[S]])
    extends Impl[S]

  private trait Impl[S <: Sys[S]]
    extends ListObjView[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.EmptyRenderer[S]
    with ListObjViewImpl.NonEditable[S]
    with ProcObjView[S] {

    override def obj(implicit tx: S#Tx): Proc.Obj[S] = objH()

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

  private final class TimelineImpl[S <: Sys[S]](val objH: stm.Source[S#Tx, Obj.T[S, Proc.Elem]],
                                        var audio     : Option[Grapheme.Segment.Audio],
                                        var busOption : Option[Int], context: TimelineObjView.Context[S])
    extends Impl[S] with TimelineObjViewImpl.BasicImpl[S] with ProcObjView.Timeline[S] { self =>

    override def toString = s"ProcView($name, $spanValue, $audio)"

    var gain        : Double          = _
    var muted       : Boolean         = _
    var fadeIn      : FadeSpec        = _
    var fadeOut     : FadeSpec        = _

    def debugString =
      s"ProcView(span = $spanValue, trackIndex = $trackIndex, nameOption = $nameOption, muted = $muted, audio = $audio, " +
      s"fadeIn = $fadeIn, fadeOut = $fadeOut, gain = $gain, busOption = $busOption)\n" +
      inputs .mkString("  inputs  = [", ", ", "]\n") +
      outputs.mkString("  outputs = [", ", ", "]\n")

    private var failedAcquire = false
    var sonogram  = Option.empty[SonoOverview]

    var inputs    = Map.empty[String, Vec[ProcObjView.Link[S]]]
    var outputs   = Map.empty[String, Vec[ProcObjView.Link[S]]]

    def releaseSonogram(): Unit =
      sonogram.foreach { ovr =>
        sonogram = None
        SonogramManager.release(ovr)
      }

    override def name = nameOption.getOrElse {
      audio.fold(TimelineObjView.Unnamed)(_.value.artifact.base)
    }

    def acquireSonogram(): Option[SonoOverview] = {
      if (failedAcquire) return None
      releaseSonogram()
      sonogram = audio.flatMap { segm =>
        try {
          val ovr = SonogramManager.acquire(segm.value.artifact)  // XXX TODO: remove `Try` once manager is fixed
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
      val proc = obj.elem.peer
      proc.inputs.iterator.foreach { case (_, scan) =>
        context.scanMap.remove(scan.id)
      }
      proc.outputs.iterator.foreach { case (_, scan) =>
        context.scanMap.remove(scan.id)
      }
      deferTx(disposeGUI())
    }

    private def disposeGUI(): Unit = {
      releaseSonogram()

      def removeLinks(inp: Boolean): Unit = {
        val map = if (inp) inputs else outputs
        map.foreach { case (thisKey, links) =>
          links.foreach { link =>
            val thatView  = link.target
            val thatKey   = link.targetKey
            if (inp)
              thatView.removeOutput(thatKey, self, thisKey)
            else
              thatView.removeInput (thatKey, self, thisKey)
          }
        }
        if (inp) inputs = Map.empty else outputs = Map.empty
      }

      removeLinks(inp = true )
      removeLinks(inp = false)
    }

    def addInput (thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit =
      inputs  = addLink(inputs , thisKey, Link(thatView, thatKey))

    def addOutput(thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit =
      outputs = addLink(outputs, thisKey, Link(thatView, thatKey))

    def removeInput (thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit =
      inputs  = removeLink(inputs , thisKey, Link(thatView, thatKey))

    def removeOutput(thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit =
      outputs = removeLink(outputs, thisKey, Link(thatView, thatKey))

    def isGlobal: Boolean = {
      import TypeCheckedTripleEquals._
      spanValue === Span.All
    }
  }

  case class Link[S <: evt.Sys[S]](target: ProcObjView.Timeline[S], targetKey: String)

  /** A data set for graphical display of a proc. Accessors and mutators should
    * only be called on the event dispatch thread. Mutators are plain variables
    * and do not affect the underlying model. They should typically only be called
    * in response to observing a change in the model.
    */
  trait Timeline[S <: evt.Sys[S]]
    extends ProcObjView[S] with TimelineObjView[S]
    with TimelineObjView.HasMute
    with TimelineObjView.HasGain
    with TimelineObjView.HasFade {

    // override type E[~ <: evt.Sys[~]] = Proc.Elem[~]

    /** Convenience check for `span == Span.All` */
    def isGlobal: Boolean

    var inputs : LinkMap[S]
    var outputs: LinkMap[S]

    def addInput    (thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit
    def addOutput   (thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit

    def removeInput (thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit
    def removeOutput(thisKey: String, thatView: ProcObjView.Timeline[S], thatKey: String): Unit

    var busOption: Option[Int]

    /** If this proc is bound to an audio grapheme for the default scan key, returns
      * this grapheme segment (underlying audio file of a tape object). */
    var audio: Option[Grapheme.Segment.Audio]

    var sonogram: Option[SonoOverview]

    /** Releases a sonogram view. If none had been acquired, this is a safe no-op.
      * Updates the `sono` variable.
      */
    def releaseSonogram(): Unit

    /** Attempts to acquire a sonogram view. Updates the `sono` variable if successful. */
    def acquireSonogram(): Option[SonoOverview]

    def debugString: String
  }
}
trait ProcObjView[S <: evt.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): Proc.Obj[S]

  def objH: stm.Source[S#Tx, Proc.Obj[S]]
}