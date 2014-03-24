/*
 *  MenuBar.scala
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

import de.sciss.desktop.{Desktop, KeyStrokes, Menu}
import java.awt.event.KeyEvent
import de.sciss.mellite.{Mellite => App}

object MenuBar {
  lazy val instance: Menu.Root = {
    import Menu._
    import KeyStrokes._
    import KeyEvent._

    val itPrefs = Item.Preferences(App)(ActionPreferences())
    val itQuit  = Item.Quit(App)

    val mFile = Group("file", "File")
      .add(Group("new", "New")
        .add(Item("new-doc", ActionNewFile))
        .add(Item("repl",    InterpreterFrame.Action))
      )
      .add(Item("open", ActionOpenFile))
      .add(ActionOpenFile.recentMenu)
      .add(Item("close",              proxy("Close",                    menu1 + VK_W)))
      .add(Item("closeAll",           "Close All"))
      .addLine()
      .add(Item("bounce",             proxy("Bounce...",                menu1 + VK_B)))
      // .add(Item("bounce-transform",   proxy("Bounce And Transform...",  (menu1 + shift + VK_B))))
    if (itQuit.visible) mFile.addLine().add(itQuit)

    val mEdit = Group("edit", "Edit")
      // eventually Undo / Redo here
      .add(Item("cut",                proxy("Cut",                      menu1 + VK_X)))
      .add(Item("copy",               proxy("Copy",                     menu1 + VK_C)))
      .add(Item("paste",              proxy("Paste",                    menu1 + VK_V)))
      .add(Item("delete",             proxy("Delete",                   plain + VK_BACK_SPACE)))
      .addLine()
      .add(Item("selectAll",          proxy("Select All",               menu1 + VK_A)))

    if (itPrefs.visible && Desktop.isLinux) mEdit.addLine().add(itPrefs)

    val mActions = Group("actions", "Actions")
      .add(Item("stopAllSound",       proxy("Stop All Sound",           menu1 + VK_PERIOD)))
      .add(Item("debugPrint",         proxy("Debug Print",              menu2 + VK_P)))
      .add(Item("windowShot",         proxy("Export Window as PDF...")))

    // --- timeline menu ---
    val mTimeline = Group("timeline", "Timeline")
      // .add(Item("trimToSelection",    proxy("Trim to Selection",        (menu1 + VK_F5))))
      .add(Item("insertSpan",         proxy("Insert Span...",           menu1 + shift + VK_E)))
      .add(Item("clearSpan",          proxy("Clear Selected Span",      menu1 + VK_BACK_SLASH)))
      .add(Item("removeSpan",         proxy("Remove Selected Span",     menu1 + shift + VK_BACK_SLASH)))
      .add(Item("dupSpanToPos",       "Duplicate Span to Cursor"))
      .addLine()
      .add(Item("nudgeAmount",        "Nudge Amount..."))
      .add(Item("nudgeLeft",          proxy("Nudge Objects Backward",   plain + VK_MINUS)))
      .add(Item("nudgeRight",         proxy("Nudge Objects Forward",    plain + VK_PLUS)))
      .addLine()
      .add(Item("selectFollowing",    proxy("Select Following Objects", menu2 + VK_F)))
      .add(Item("alignObjStartToPos", "Align Objects Start To Cursor"))
      .add(Item("splitObjects",       proxy("Split Selected Objects",   menu2 + VK_Y)))
      .addLine()
      .add(Item("selStopToStart",     "Flip Selection Backward"))
      .add(Item("selStartToStop",     "Flip Selection Forward"))

    val mOperation = Group("operation", "Operation")
      .add(Item("cursorFollows",      "Cursor Follows Playback Head"))

    if (itPrefs.visible && !Desktop.isLinux) mOperation.addLine().add(itPrefs)

    Root().add(mFile).add(mEdit).add(mActions).add(mTimeline).add(mOperation)
  }
}
