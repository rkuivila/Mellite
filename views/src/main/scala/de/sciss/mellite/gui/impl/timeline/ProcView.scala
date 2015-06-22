/*
 *  ProcView.scala
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
package timeline

import de.sciss.lucre.event.Sys
import de.sciss.synth.proc.{IntElem, ObjKeys, Obj, FadeSpec, Grapheme, Scan, Proc}
import de.sciss.lucre.{stm, expr}
import de.sciss.span.{Span, SpanLike}
import de.sciss.sonogram.{Overview => SonoOverview}
import expr.Expr
import org.scalautils.TypeCheckedTripleEquals
import language.implicitConversions
import scala.util.control.NonFatal
import de.sciss.file._
import de.sciss.lucre.stm.IdentifierMap
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.swing.deferTx

object ProcView extends TimelineObjView.Factory {
  def typeID: Int = Proc.typeID

  type E[S <: Sys[S]] = Proc.Elem[S]

  type LinkMap[S <: Sys[S]] = Map[String, Vec[ProcView.Link[S]]]
  type ProcMap[S <: Sys[S]] = IdentifierMap[S#ID, S#Tx, ProcView[S]]
  type ScanMap[S <: Sys[S]] = IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])]

  type SelectionModel[S <: Sys[S]] = gui.SelectionModel[S, ProcView[S]]

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
  def apply[S <: Sys[S]](timedID: S#ID, span: Expr[S, SpanLike], obj: Proc.Obj[S], context: TimelineObjView.Context[S])
                        (implicit tx: S#Tx): ProcView[S] = {
    val spanV = span.value
    import SpanLikeEx._
    // println("--- scan keys:")
    // proc.scans.keys.foreach(println)

    // XXX TODO: DRY - use getAudioRegion, and nextEventAfter to construct the segment value
    val scans = obj.elem.peer.scans
    val audio = scans.get(Proc.Obj.graphAudio).flatMap { scanW =>
      // println("--- has scan")
      scanW.sources.flatMap {
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
    val res = new Impl(span = tx.newHandle(span), obj = tx.newHandle(obj), audio = audio, busOption = bus,
      context = context)

    TimelineObjViewImpl.initAttrs    (span, obj, res)
    TimelineObjViewImpl.initGainAttrs(span, obj, res)
    TimelineObjViewImpl.initMuteAttrs(span, obj, res)
    TimelineObjViewImpl.initFadeAttrs(span, obj, res)

    import de.sciss.lucre.synth.expr.IdentifierSerializer
    lazy val idH = tx.newHandle(timedID)

    import context.{scanMap, viewMap}

    scans.iterator.foreach { case (key, scan) =>
      def findLinks(inp: Boolean): Unit = {
        val it = if (inp) scan.sources else scan.sinks
        it.foreach {
          case Scan.Link.Scan(peer) if scanMap.contains(peer.id) =>
            val Some((thatKey, thatIdH)) = scanMap.get(peer.id)
            val thatID = thatIdH()
            viewMap.get(thatID).foreach {
              case thatView: ProcView[S] =>
                if (DEBUG) println(s"PV $timedID add link from $key to $thatID, $thatKey")
                if (inp) {
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
      if (DEBUG) println(s"PV $timedID add scan ${scan.id}, $key")
      scanMap.put(scan.id, key -> idH)
      findLinks(inp = true )
      findLinks(inp = false)
    }

    // procMap.put(timed.id, res)
    res
  }

  private final class Impl[S <: Sys[S]](val span: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val obj: stm.Source[S#Tx, Obj.T[S, Proc.Elem]],
                                        var audio     : Option[Grapheme.Segment.Audio],
                                        var busOption : Option[Int], context: TimelineObjView.Context[S])
    extends ProcView[S] { self =>

    override def toString = s"ProcView($name, $spanValue, $audio)"

    var trackIndex  : Int             = _
    var trackHeight : Int             = _
    var nameOption  : Option[String]  = _
    var spanValue   : SpanLike        = _
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

    var inputs    = Map.empty[String, Vec[ProcView.Link[S]]]
    var outputs   = Map.empty[String, Vec[ProcView.Link[S]]]

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

    def dispose()(implicit tx: S#Tx): Unit = {
      // procMap.remove(timed.id)
      val proc  = obj().elem.peer
      // val proc = timed.value.elem.peer
      proc.scans.iterator.foreach { case (_, scan) =>
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

    def addInput (thisKey: String, thatView: ProcView[S], thatKey: String): Unit =
      inputs  = addLink(inputs , thisKey, Link(thatView, thatKey))

    def addOutput(thisKey: String, thatView: ProcView[S], thatKey: String): Unit =
      outputs = addLink(outputs, thisKey, Link(thatView, thatKey))

    def removeInput (thisKey: String, thatView: ProcView[S], thatKey: String): Unit =
      inputs  = removeLink(inputs , thisKey, Link(thatView, thatKey))

    def removeOutput(thisKey: String, thatView: ProcView[S], thatKey: String): Unit =
      outputs = removeLink(outputs, thisKey, Link(thatView, thatKey))

    def isGlobal: Boolean = {
      import TypeCheckedTripleEquals._
      spanValue === Span.All
    }

    def proc(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = obj()
  }

  case class Link[S <: Sys[S]](target: ProcView[S], targetKey: String)
}

/** A data set for graphical display of a proc. Accessors and mutators should
  * only be called on the event dispatch thread. Mutators are plain variables
  * and do not affect the underlying model. They should typically only be called
  * in response to observing a change in the model.
  */
trait ProcView[S <: Sys[S]]
  extends TimelineObjView[S]
  with TimelineObjView.HasMute
  with TimelineObjView.HasGain
  with TimelineObjView.HasFade
  {

  import ProcView.{LinkMap, ProcMap, ScanMap}
  
  override def obj: stm.Source[S#Tx, Proc.Obj[S]]

  /** Convenience for `obj()` */
  def proc(implicit tx: S#Tx): Proc.Obj[S]

  // var track: Int

  // var nameOption: Option[String]

  /** Convenience check for `span == Span.All` */
  def isGlobal: Boolean

  var inputs : LinkMap[S]
  var outputs: LinkMap[S]

  def addInput    (thisKey: String, thatView: ProcView[S], thatKey: String): Unit
  def addOutput   (thisKey: String, thatView: ProcView[S], thatKey: String): Unit

  def removeInput (thisKey: String, thatView: ProcView[S], thatKey: String): Unit
  def removeOutput(thisKey: String, thatView: ProcView[S], thatKey: String): Unit

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