/*
 *  MenuBar.scala
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

import de.sciss.desktop.{KeyStrokes, Menu}
import java.awt.event.KeyEvent

object MenuBar {
  def apply(): Menu.Root = {
    import Menu._
    import KeyStrokes._
    import KeyEvent._
    Root().add(
      Group("file", "File")
        .add(Group("new", "New")
          .add(Item("new-doc", ActionNewFile))
          .add(Item("repl",    InterpreterFrame.Action))
        )
        .add(Item("open", ActionOpenFile))
        .add(ActionOpenFile.recentMenu)
        .add(Item("close",              proxy("Close",                    (menu1 + VK_W))))
        .add(Item("closeAll",           "Close All"))
        .addLine()
        .add(Item("bounce",             proxy("Bounce...",                (menu1 + VK_B))))
        .add(Item("bounce-transform",   proxy("Bounce And Transform...",  (menu1 + shift + VK_B))))
    ).add(
      Group("edit", "Edit")
        // eventually Undo / Redo here
        .add(Item("cut",                proxy("Cut",                      (menu1 + VK_X))))
        .add(Item("copy",               proxy("Copy",                     (menu1 + VK_C))))
        .add(Item("paste",              proxy("Paste",                    (menu1 + VK_V))))
        .add(Item("delete",             proxy("Delete",                   (plain + VK_BACK_SPACE))))
        .addLine()
        .add(Item("selectAll",          proxy("Select All",               (menu1 + VK_A))))
    ).add(
      // --- timeline menu ---
      Group("timeline", "Timeline")
        // .add(Item("trimToSelection",    proxy("Trim to Selection",        (menu1 + VK_F5))))
        .add(Item("insertSpan",         proxy("Insert Span...",           (menu1 + shift + VK_E))))
        .add(Item("clearSpan",          proxy("Clear Selected Span",      (menu1 + VK_BACK_SLASH))))
        .add(Item("removeSpan",         proxy("Remove Selected Span",     (menu1 + shift + VK_BACK_SLASH))))
        .add(Item("dupSpanToPos",       "Duplicate Span to Cursor"))
        .addLine()
        .add(Item("nudgeAmount",        "Nudge Amount..."))
        .add(Item("nudgeLeft",          proxy("Nudge Objects Backward",   (plain + VK_MINUS))))
        .add(Item("nudgeRight",         proxy("Nudge Objects Forward",    (plain + VK_PLUS ))))
        .addLine()
        .add(Item("selectFollowing",    proxy("Select Following Objects", (menu2 + VK_F))))
        .add(Item("alignObjStartToPos", "Align Objects Start To Cursor"))
        .add(Item("splitObjects",       proxy("Split Selected Objects",   (menu2 + VK_Y))))
        .addLine()
        .add(Item("selStopToStart",     "Flip Selection Backward"))
        .add(Item("selStartToStop",     "Flip Selection Forward"))
    ).add(
      Group("operation", "Operation")
        .add(Item("cursorFollows",      "Cursor Follows Playhead"))
        .addLine()
        .add(Item("preferences", ActionPreferences))
    )
  }
}
