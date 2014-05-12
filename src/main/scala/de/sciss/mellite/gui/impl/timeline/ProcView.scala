/*
 *  ProcView.scala
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
package gui
package impl
package timeline

import de.sciss.synth.proc.{Obj, FadeSpec, ProcKeys, Grapheme, Scan, Proc, TimedProc}
import de.sciss.lucre.{stm, expr}
import de.sciss.span.{Span, SpanLike}
import de.sciss.sonogram.{Overview => SonoOverview}
import expr.Expr
import language.implicitConversions
import scala.util.control.NonFatal
import de.sciss.file._
import de.sciss.lucre.stm.IdentifierMap
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}

object ProcView {
  type LinkMap[S <: Sys[S]] = Map[String, Vec[ProcView.Link[S]]]
  type ProcMap[S <: Sys[S]] = IdentifierMap[S#ID, S#Tx, ProcView[S]]
  type ScanMap[S <: Sys[S]] = IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])]

  private final val DEBUG = false

  final val Unnamed = "<unnamed>"

  private def addLink[A, B](map: Map[A, Vec[B]], key: A, value: B): Map[A, Vec[B]] =
    map + (key -> (map.getOrElse(key, Vec.empty) :+ value))

  private def removeLink[A, B](map: Map[A, Vec[B]], key: A, value: B): Map[A, Vec[B]] = {
    val newVec = map.getOrElse(key, Vec.empty).filterNot(_ == value)
    if (newVec.isEmpty) map - key else map + (key -> newVec)
  }

  /** Constructs a new proc view from a given proc, and a map with the known proc (views).
    * This will automatically add the new view to the map!
    *
    * @param timed    the proc to create the view for
    * @param procMap  a map from `TimedProc` ids to their views. This is used to establish scan links.
    * @param scanMap  a map from `Scan` ids to their keys and a handle on the timed-proc's id.
    */
  def apply[S <: Sys[S]](timed  : TimedProc[S],
                         procMap: ProcMap  [S],
                         scanMap: ScanMap  [S])
                        (implicit tx: S#Tx): ProcView[S] = {
    val span  = timed.span
    val proc  = timed.value
    val spanV = span.value
    import SpanLikeEx._
    // println("--- scan keys:")
    // proc.scans.keys.foreach(println)

    // XXX TODO: DRY - use getAudioRegion, and nextEventAfter to construct the segment value
    val scans = proc.elem.peer.scans
    val audio = scans.get(ProcKeys.graphAudio).flatMap { scanw =>
      // println("--- has scan")
      scanw.sources.flatMap {
        case Scan.Link.Grapheme(g) =>
          // println("--- scan is linked")
          spanV match {
            case Span.HasStart(frame) =>
              // println("--- has start")
              g.segment(frame) match {
                case Some(segm @ Grapheme.Segment.Audio(gspan, _)) /* if (gspan.start == frame) */ => Some(segm)
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

    val attr    = proc.attr

    val track   = attr.expr[Int     ](ProcKeys.attrTrack  ).fold(0)(_.value)
    val name    = attr.expr[String  ](ProcKeys.attrName   ).map(_.value)
    val mute    = attr.expr[Boolean ](ProcKeys.attrMute).exists(_.value)
    val fadeIn  = attr.expr[FadeSpec](ProcKeys.attrFadeIn ).fold(TrackTool.EmptyFade)(_.value)
    val fadeOut = attr.expr[FadeSpec](ProcKeys.attrFadeOut).fold(TrackTool.EmptyFade)(_.value)
    val gain    = attr.expr[Double  ](ProcKeys.attrGain   ).fold(1.0)(_.value)
    val bus     = attr.expr[Int     ](ProcKeys.attrBus    ).map(_.value)

    val res = new Impl(spanSource = tx.newHandle(span), procSource = tx.newHandle(proc),
      span = spanV, track = track, nameOption = name, muted = mute, audio = audio,
      fadeIn = fadeIn, fadeOut = fadeOut, gain = gain, busOption = bus)

    import de.sciss.lucre.synth.expr.IdentifierSerializer
    lazy val idH = tx.newHandle(timed.id)

    scans.iterator.foreach { case (key, scan) =>
      def findLinks(inp: Boolean): Unit = {
        val it = if (inp) scan.sources else scan.sinks
        it.foreach {
          case Scan.Link.Scan(peer) if scanMap.contains(peer.id) =>
            val Some((thatKey, thatIdH)) = scanMap.get(peer.id)
            val thatID = thatIdH()
            procMap.get(thatID).foreach { thatView =>
              if (DEBUG) println(s"PV ${timed.id} add link from $key to $thatID, $thatKey")
              if (inp) {
                res     .addInput (key    , thatView, thatKey)
                thatView.addOutput(thatKey, res     , key    )
              } else {
                res     .addOutput(key    , thatView, thatKey)
                thatView.addInput (thatKey, res     , key    )
              }
            }

          case other =>
            if (DEBUG) other match {
              case Scan.Link.Scan(peer) =>
                println(s"PV ${timed.id} missing link from $key to scan $peer")
              case _ =>
            }
        }
      }
      if (DEBUG) println(s"PV ${timed.id} add scan ${scan.id}, $key")
      scanMap.put(scan.id, key -> idH)
      findLinks(inp = true )
      findLinks(inp = false)
    }

    procMap.put(timed.id, res)
    res
  }

  private final class Impl[S <: Sys[S]](val spanSource: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val procSource: stm.Source[S#Tx, Obj.T[S, Proc.Elem]],
                                        var span      : SpanLike,
                                        var track     : Int,
                                        var nameOption: Option[String],
                                        var muted     : Boolean,
                                        var audio     : Option[Grapheme.Segment.Audio],
                                        var fadeIn    : FadeSpec,
                                        var fadeOut   : FadeSpec,
                                        var gain      : Double,
                                        var busOption : Option[Int])
    extends ProcView[S] { self =>

    override def toString = s"ProcView($name, $span, $audio)"

    def debugString =
      s"ProcView(span = $span, track = $track, nameOption = $nameOption, muted = $muted, audio = $audio, " +
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

    def name = nameOption.getOrElse {
      audio.fold(Unnamed)(_.value.artifact.base)
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

    def disposeTx(timed: TimedProc[S],
                  procMap: ProcMap[S],
                  scanMap: ScanMap[S])(implicit tx: S#Tx): Unit = {
      procMap.remove(timed.id)
      val proc = timed.value.elem.peer
      proc.scans.iterator.foreach { case (_, scan) =>
        scanMap.remove(scan.id)
      }
    }

    def disposeGUI(): Unit = {
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

    def isGlobal = span == Span.All

    def proc(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = procSource()
  }

  implicit def span[S <: Sys[S]](view: ProcView[S]): (Long, Long) = {
    view.span match {
      case Span(start, stop)  => (start, stop)
      case Span.From(start)   => (start, Long.MaxValue)
      case Span.Until(stop)   => (Long.MinValue, stop)
      case Span.All           => (Long.MinValue, Long.MaxValue)
      case Span.Void          => (Long.MinValue, Long.MinValue)
    }
  }

  case class Link[S <: Sys[S]](target: ProcView[S], targetKey: String)
}

/** A data set for graphical display of a proc. Accessors and mutators should
  * only be called on the event dispatch thread. Mutators are plain variables
  * and do not affect the underlying model. They should typically only be called
  * in response to observing a change in the model.
  */
sealed trait ProcView[S <: Sys[S]] {
  import ProcView.{LinkMap, ProcMap, ScanMap}
  
  def spanSource: stm.Source[S#Tx, Expr[S, SpanLike]]
  def procSource: stm.Source[S#Tx, Obj.T[S, Proc.Elem]]

  /** Convenience for `procSource()` */
  def proc(implicit tx: S#Tx): Obj.T[S, Proc.Elem]

  var span: SpanLike
  var track: Int
  var nameOption: Option[String]

  /** Convenience check for `span == Span.All` */
  def isGlobal: Boolean

  /** The proc's name or a place holder name if no name is set. */
  def name: String

  var inputs : LinkMap[S]
  var outputs: LinkMap[S]

  def addInput    (thisKey: String, thatView: ProcView[S], thatKey: String): Unit
  def addOutput   (thisKey: String, thatView: ProcView[S], thatKey: String): Unit

  def removeInput (thisKey: String, thatView: ProcView[S], thatKey: String): Unit
  def removeOutput(thisKey: String, thatView: ProcView[S], thatKey: String): Unit

  var muted: Boolean

  var busOption: Option[Int]

  /** If this proc is bound to an audio grapheme for the default scan key, returns
    * this grapheme segment (underlying audio file of a tape object). */
  var audio: Option[Grapheme.Segment.Audio]

  var sonogram: Option[SonoOverview]

  var fadeIn : FadeSpec
  var fadeOut: FadeSpec

  var gain: Double

  /** Releases a sonogram view. If none had been acquired, this is a safe no-op.
    * Updates the `sono` variable.
    */
  def releaseSonogram(): Unit

  /** Attemps to acquire a sonogram view. Updates the `sono` variable if successful. */
  def acquireSonogram(): Option[SonoOverview]

  def disposeTx(timed: TimedProc[S],
                procMap: ProcMap[S],
                scanMap: ScanMap[S])(implicit tx: S#Tx): Unit

  def disposeGUI(): Unit

  def debugString: String
}