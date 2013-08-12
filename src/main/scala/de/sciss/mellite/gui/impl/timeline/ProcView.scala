/*
 *  ProcView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl
package timeline

import de.sciss.synth.proc.{FadeSpec, ProcKeys, Attribute, Grapheme, Scan, Proc, TimedProc, Sys}
import de.sciss.lucre.{stm, expr}
import de.sciss.span.{Span, SpanLike}
import de.sciss.sonogram.{Overview => SonoOverview}
import expr.Expr
import de.sciss.synth.expr.SpanLikes
import language.implicitConversions
import scala.util.control.NonFatal
import de.sciss.file._
import de.sciss.lucre.stm.IdentifierMap
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.synth.proc.impl.CommonSerializers

object ProcView {
  type LinkMap[S <: Sys[S]] = Map[String, Vec[ProcView.Link[S]]]
  type ProcMap[S <: Sys[S]] = IdentifierMap[S#ID, S#Tx, ProcView[S]]
  type ScanMap[S <: Sys[S]] = IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])]

  private final val DEBUG = false

  final val Unnamed = "<unnamed>"
  
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
    import SpanLikes._
    // println("--- scan keys:")
    // proc.scans.keys.foreach(println)

    // XXX TODO: DRY - use getAudioRegion, and nextEventAfter to construct the segment value
    val audio = proc.scans.get(ProcKeys.graphAudio).flatMap { scanw =>
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

    val attr    = proc.attributes

    val track   = attr[Attribute.Int     ](ProcKeys.attrTrack  ).map(_.value).getOrElse(0)
    val name    = attr[Attribute.String  ](ProcKeys.attrName   ).map(_.value)
    val mute    = attr[Attribute.Boolean ](ProcKeys.attrMute).exists(_.value)
    val fadeIn  = attr[Attribute.FadeSpec](ProcKeys.attrFadeIn ).map(_.value).getOrElse(TrackTool.EmptyFade)
    val fadeOut = attr[Attribute.FadeSpec](ProcKeys.attrFadeOut).map(_.value).getOrElse(TrackTool.EmptyFade)

    val res = new Impl(spanSource = tx.newHandle(span), procSource = tx.newHandle(proc),
      span = spanV, track = track, nameOption = name, muted = mute, audio = audio,
      fadeIn = fadeIn, fadeOut = fadeOut)

    import CommonSerializers.Identifier
    lazy val idH = tx.newHandle(timed.id)

    def add[A, B](map: Map[A, Vec[B]], key: A, value: B): Map[A, Vec[B]] =
      map + (key -> (map.getOrElse(key, Vec.empty) :+ value))

    proc.scans.iterator.foreach { case (key, scan) =>
      def findLinks(inp: Boolean): Unit = {
        val it = if (inp) scan.sources else scan.sinks
        it.foreach {
          case Scan.Link.Scan(peer) if scanMap.contains(peer.id) =>
            val Some((thatKey, thatIdH)) = scanMap.get(peer.id)
            val thatID = thatIdH()
            procMap.get(thatID).foreach { thatView =>
              if (DEBUG) println(s"PV ${timed.id} add link from $key to $thatID, $thatKey")
              if (inp) {
                res.inputs       = add(res.inputs      , key    , Link(thatView, thatKey))
                thatView.outputs = add(thatView.outputs, thatKey, Link(res     , key    ))
              } else {
                res.outputs      = add(res.outputs     , key    , Link(thatView, thatKey))
                thatView.inputs  = add(thatView.inputs , thatKey, Link(res     , key    ))
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
                                        val procSource: stm.Source[S#Tx, Proc[S]],
                                        var span      : SpanLike,
                                        var track     : Int,
                                        var nameOption: Option[String],
                                        var muted     : Boolean,
                                        var audio     : Option[Grapheme.Segment.Audio],
                                        var fadeIn    : FadeSpec.Value,
                                        var fadeOut   : FadeSpec.Value)
    extends ProcView[S] { self =>

    override def toString = s"ProvView($name, $span, $audio)"

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
      audio.map(_.value.artifact.base).getOrElse(Unnamed)
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
      val proc = timed.value
      proc.scans.iterator.foreach { case (_, scan) =>
        scanMap.remove(scan.id)
      }
    }

    def disposeGUI(): Unit = {
      releaseSonogram()
      
      def removeLinks(map: LinkMap[S])(getMap: ProcView[S] => LinkMap[S])
                                      (setMap: (ProcView[S], LinkMap[S]) => Unit): Unit = {
        map.foreach { case (_, links) =>
          links.foreach { link =>
            val thatView  = link.target
            val thatMap   = getMap(thatView)
            val thatKey   = link.targetKey
            thatMap.get(thatKey).foreach { thatLinks =>
              val updLinks = thatLinks.filterNot(_ == self)
              val updMap   = if (updLinks.isEmpty)
                thatMap - thatKey
              else
                thatMap + (thatKey -> updLinks)
              setMap(thatView, updMap)
            }
          }
        }
      }

      removeLinks(inputs )(_.outputs)(_.outputs = _)
      removeLinks(outputs)(_.inputs )(_.inputs  = _)
      inputs  = Map.empty
      outputs = Map.empty
    }
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
  def procSource: stm.Source[S#Tx, Proc[S]]

  var span: SpanLike
  var track: Int
  var nameOption: Option[String]

  /** The proc's name or a place holder name if no name is set. */
  def name: String

  var inputs : LinkMap[S]
  var outputs: LinkMap[S]

  var muted: Boolean

  /** If this proc is bound to an audio grapheme for the default scan key, returns
    * this grapheme segment (underlying audio file of a tape object). */
  var audio: Option[Grapheme.Segment.Audio]

  var sonogram: Option[SonoOverview]

  var fadeIn : FadeSpec.Value
  var fadeOut: FadeSpec.Value

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
}