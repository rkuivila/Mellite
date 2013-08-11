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
import de.sciss.sonogram
import expr.Expr
import de.sciss.synth.expr.SpanLikes
import language.implicitConversions
import scala.util.control.NonFatal
import de.sciss.file._

object ProcView {
  def apply[S <: Sys[S]](timed: TimedProc[S])(implicit tx: S#Tx): ProcView[S] = {
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

    new Impl(spanSource = tx.newHandle(span), procSource = tx.newHandle(proc),
      span = spanV, track = track, nameOpt = name, muted = mute, audio = audio,
      fadeIn = fadeIn, fadeOut = fadeOut)
  }

  private final class Impl[S <: Sys[S]](val spanSource: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val procSource: stm.Source[S#Tx, Proc[S]],
                                        var span: SpanLike, var track: Int, var nameOpt: Option[String],
                                        var muted: Boolean,
                                        var audio: Option[Grapheme.Segment.Audio],
                                        var fadeIn: FadeSpec.Value, var fadeOut: FadeSpec.Value)
    extends ProcView[S] {

    var sono = Option.empty[sonogram.Overview]
    override def toString = s"ProvView($name, $span, $audio)"

    private var failedAcquire = false

    def release(): Unit =
      sono.foreach { ovr =>
        sono = None
        SonogramManager.release(ovr)
      }

    def name = nameOpt.getOrElse {
      audio.map(_.value.artifact.base).getOrElse("<unnamed>")
    }

    def acquire(): Option[sonogram.Overview] = {
      if (failedAcquire) return None
      release()
      sono = audio.flatMap { segm =>
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
      sono
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
}
sealed trait ProcView[S <: Sys[S]] {
  def spanSource: stm.Source[S#Tx, Expr[S, SpanLike]]
  def procSource: stm.Source[S#Tx, Proc[S]]

  var span: SpanLike
  var track: Int
  var nameOpt: Option[String]
  def name: String
  // var gain: Float
  var muted: Boolean
  // var audio: Option[Grapheme.Value.Audio]
  var audio: Option[Grapheme.Segment.Audio]

  // EDT access only
  var sono: Option[sonogram.Overview]

  var fadeIn : FadeSpec.Value
  var fadeOut: FadeSpec.Value

  // def updateSpan(span: Expr[S, SpanLike])(implicit tx: S#Tx): Unit

  def release(): Unit

  def acquire(): Option[sonogram.Overview]
}