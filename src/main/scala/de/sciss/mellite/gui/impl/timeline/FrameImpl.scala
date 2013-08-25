/*
 *  FrameImpl.scala
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

import de.sciss.synth.proc.{AuralSystem, Sys, ProcGroup}
import de.sciss.mellite.{Mellite, Element, Document}
import de.sciss.lucre.stm
import de.sciss.mellite.gui._
import de.sciss.desktop.{OptionPane, Window}
import de.sciss.desktop.impl.WindowImpl
import scala.swing.event.WindowClosing
import scala.swing.Action
import de.sciss.lucre.bitemp.impl.BiGroupImpl

object FrameImpl {
  def apply[S <: Sys[S]](document: Document[S], group: Element.ProcGroup[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S],
                         aural: AuralSystem): TimelineFrame[S] = {
    val tlv     = TimelineView(document, group)
    val name    = group.name.value
    import ProcGroup.serializer
    val groupH  = tx.newHandle(group.entity)
    val res     = new Impl(tlv, name, groupH)
    guiFromTx {
      res.init()
    }
    res
  }

  private final class Impl[S <: Sys[S]](val view: TimelineView[S], name: String,
                                        groupH: stm.Source[S#Tx, ProcGroup[S]])
                                       (implicit _cursor: stm.Cursor[S])
    extends TimelineFrame[S] {

    private var _window: Window = _

    def window = _window

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      guiFromTx(_window.dispose())
    }

    private def disposeData()(implicit tx: S#Tx): Unit =
      view.dispose()

    private def frameClosing(): Unit =
      _cursor.step { implicit tx =>
        disposeData()
      }

    def init(): Unit = {
      _window = new WindowImpl {
        def handler = Mellite.windowHandler
        def style   = Window.Regular
        component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
        title       = name
        contents    = view.component

        bindMenus(
          "file.bounce"           -> view.bounceAction,
          "edit.delete"           -> view.deleteAction,
          "actions.stopAllSound"  -> view.stopAllSoundAction,
          "timeline.splitObjects" -> view.splitObjectsAction,
          "actions.debugPrint"    -> Action(null) {
            val it = view.procSelectionModel.iterator
            if (it.hasNext)
              it.foreach { pv =>
                println(pv.debugString)
              }
            else {
              val (treeS, opt) = _cursor.step { implicit tx =>
                val s1 = groupH().debugPrint
                val s2 = BiGroupImpl.verifyConsistency(groupH(), reportOnly = true)
                (s1, s2)
              }
              if (opt.isEmpty) {
                println("No problems found!")
              } else {
                println(treeS)
                println()
                opt.foreach(println)

                val pane = OptionPane.confirmation(message = "Correct the data structure?",
                  optionType = OptionPane.Options.YesNo, messageType = OptionPane.Message.Warning)
                pane.title = "Sanitize Timeline"
                val sel = pane.show(Some(this))
                if (sel == OptionPane.Result.Yes) _cursor.step { implicit tx =>
                  BiGroupImpl.verifyConsistency(groupH(), reportOnly = false)
                }
              }
            }
          }
        )

        reactions += {
          case WindowClosing(_) => frameClosing()
        }

        pack()
        // centerOnScreen()
        GUI.placeWindow(this, 0f, 0.25f, 24)
        front()
      }
    }
  }
}