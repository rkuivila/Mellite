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

import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.synth.proc.Timeline
import de.sciss.lucre.stm
import de.sciss.desktop.{Window, KeyStrokes, Menu, OptionPane}
import scala.swing.event.Key
import scala.swing.Action
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc

object FrameImpl {
  def apply[S <: Sys[S]](group: Timeline.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): TimelineFrame[S] = {
    implicit val undoMgr  = new UndoManagerImpl
    val tlv     = TimelineView[S](group)
    val name    = ExprView.name(group)
    import Timeline.serializer
    val groupH  = tx.newHandle(group.elem.peer)
    val res     = new Impl(tlv, name, groupH)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S]](val view: TimelineView[S], name: ExprView[S#Tx, String],
                                        groupH: stm.Source[S#Tx, Timeline[S]])
                                       (implicit _cursor: stm.Cursor[S])
    extends WindowImpl[S](name.map(n => s"$n : Timeline"))
    with TimelineFrame[S] {

    override protected def initGUI(): Unit = {
      bindMenus(
        "file.bounce"             -> view.bounceAction,
        "edit.delete"             -> view.deleteAction,
        "actions.stop-all-sound"  -> view.stopAllSoundAction,
        // "timeline.splitObjects" -> view.splitObjectsAction,

        "actions.debug-print"     -> Action(null) {
          val it = view.selectionModel.iterator
          if (it.hasNext)
            it.foreach {
              case pv: ProcView[S] => println(pv.debugString)
              case _ =>
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
              val sel = pane.show(Some(window))
              if (sel == OptionPane.Result.Yes) _cursor.step { implicit tx =>
                BiGroupImpl.verifyConsistency(groupH(), reportOnly = false)
              }
            }
          }
        }
      )

      // --- timeline menu ---
      import Menu.{Group, Item, proxy}
      import KeyStrokes._
      val mTimeline = Group("timeline", "Timeline")
        // .add(Item("trimToSelection",    proxy("Trim to Selection",        (menu1 + Key.F5))))
        .add(Item("insert-span",        proxy("Insert Span...",           menu1 + shift + Key.E)))
        .add(Item("clear-span",         proxy("Clear Selected Span",      menu1 + Key.BackSlash)))
        .add(Item("remove-span",        proxy("Remove Selected Span",     menu1 + shift + Key.BackSlash)))
        .add(Item("dup-span-to-pos",    "Duplicate Span to Cursor"))
        .addLine()
        .add(Item("nudge-amount",       "Nudge Amount..."))
        .add(Item("nudge-left",         proxy("Nudge Objects Backward",   plain + Key.Minus)))
        .add(Item("nudge-right",        proxy("Nudge Objects Forward",    plain + Key.Plus)))
        .addLine()
        .add(Item("select-following",   proxy("Select Following Objects", menu2 + Key.F)))
        .add(Item("align-obj-start-to-pos", "Align Objects Start To Cursor"))
        // .add(Item("splitObjects",       proxy("Split Selected Objects",   menu2 + Key.Y)))
        .add(Item("split-objects", view.splitObjectsAction))
        .addLine()
        .add(Item("sel-stop-to-start",     "Flip Selection Backward"))
        .add(Item("sel-start-to-stop",     "Flip Selection Forward"))

      window.reactions += {
        case Window.Activated(_) => view.canvasComponent.requestFocusInWindow()
      }

      Application.windowHandler.menuFactory.add(Some(window), mTimeline)
    }

    // GUI.placeWindow(this, 0f, 0.25f, 24)
  }
}