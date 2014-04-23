/*
 *  FrameImpl.scala
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

import de.sciss.synth.proc.{AuralSystem, ProcGroup}
import de.sciss.mellite.{Element, Document}
import de.sciss.lucre.stm
import de.sciss.mellite.gui._
import de.sciss.desktop.{OptionPane, Window}
import scala.swing.event.WindowClosing
import scala.swing.Action
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing._

object FrameImpl {
  def apply[S <: Sys[S]](document: Document[S], group: Element.ProcGroup[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S],
                         aural: AuralSystem): TimelineFrame[S] = {
    val tlv     = TimelineView(document, group)
    val name    = group.name.value
    import ProcGroup.serializer
    val groupH  = tx.newHandle(group.entity)
    val res     = new Impl(tlv, name, groupH)
    deferTx {
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
      deferTx(_window.dispose())
    }

    private def disposeData()(implicit tx: S#Tx): Unit =
      view.dispose()

    private def frameClosing(): Unit =
      _cursor.step { implicit tx =>
        disposeData()
      }

    def init(): Unit = {
      _window = new WindowImpl {
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

      view.component.peer.putClientProperty("de.sciss.mellite.Window", _window)
    }
  }
}