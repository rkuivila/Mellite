/*
 *  GlobalProcsViewImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.synth.proc.{Grapheme, Scan, ProcKeys, Proc, Sys}
import de.sciss.synth.proc
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.event.Change
import de.sciss.span.Span

object GlobalProcsViewImpl {
  def apply[S <: Sys[S]](document: Document[S], group: Element.ProcGroup[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): GlobalProcsView[S] = {

//    val obsGroup = group.changed.react { implicit tx => _.changes.foreach {
//      case BiGroup.Added  (Span.All, timed) => view.addProc   (timed)
//      case BiGroup.Removed(Span.All, timed) => view.removeProc(timed)
//      case BiGroup.ElementMoved(timed, Change(Span.All, _)) => view.removeProc(timed)
//      case BiGroup.ElementMoved(timed, Change(_, Span.All)) => view.addProc   (timed)
//      case BiGroup.ElementMutated(timed, procUpd) =>
//        procUpd.changes.foreach {
//          case Proc.AssociationAdded  (key) =>
//            key match {
//              case Proc.AttributeKey(name) => attrChanged(timed, name)
//              case Proc.ScanKey     (name) => scanAdded  (timed, name)
//            }
//          case Proc.AssociationRemoved(key) =>
//            key match {
//              case Proc.AttributeKey(name) => attrChanged(timed, name)
//              case Proc.ScanKey     (name) => scanRemoved(timed, name)
//            }
//          case Proc.AttributeChange(name, attr, ach) =>
//            (name, ach) match {
//              case (ProcKeys.attrTrack, Change(before: Int, now: Int)) =>
//                view.procMoved(timed, spanCh = Change(Span.Void, Span.Void), trackCh = Change(before, now))
//
//              case _ => attrChanged(timed, name)
//            }
//
//          case Proc.ScanChange(name, scan, scanUpds) =>
//            scanUpds.foreach {
//              case Scan.GraphemeChange(grapheme, segms) =>
//                if (name == ProcKeys.graphAudio) {
//                  timed.span.value match {
//                    case Span.HasStart(startFrame) =>
//                      val segmOpt = segms.find(_.span.contains(startFrame)) match {
//                        case Some(segm: Grapheme.Segment.Audio) => Some(segm)
//                        case _ => None
//                      }
//                      view.procAudioChanged(timed, segmOpt)
//                    case _ =>
//                  }
//                }
//
//              case Scan.SinkAdded    (Scan.Link.Scan(peer)) =>
//                val test: Scan[S] = scan
//                view.scanSinkAdded    (timed, name, test, peer)
//              case Scan.SinkRemoved  (Scan.Link.Scan(peer)) => view.scanSinkRemoved  (timed, name, scan, peer)
//              case Scan.SourceAdded  (Scan.Link.Scan(peer)) => view.scanSourceAdded  (timed, name, scan, peer)
//              case Scan.SourceRemoved(Scan.Link.Scan(peer)) => view.scanSourceRemoved(timed, name, scan, peer)
//
//              case _ => // Scan.SinkAdded(_) | Scan.SinkRemoved(_) | Scan.SourceAdded(_) | Scan.SourceRemoved(_)
//            }
//
//          case Proc.GraphChange(ch) =>
//        }
//    }}
//    disp ::= obsGroup

    ???
  }
}