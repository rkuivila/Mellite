/*
 *  MenuBar.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import java.net.URL

import de.sciss.desktop.{OptionPane, Desktop, KeyStrokes, Menu}
import de.sciss.lucre.synth.Txn
import de.sciss.osc
import scala.concurrent.stm.TxnExecutor
import scala.swing.Label
import scala.swing.event.{MouseClicked, Key}

object MenuBar {
  private def showAbout(): Unit = {
    val url   = Mellite.homepage
    val addr  = url // url.substring(math.min(url.length, url.indexOf("//") + 2))
    val html  =
      s"""<html><center>
         |<font size=+1><b>${Mellite.name}</b></font><p>
         |Version ${Mellite.version}<p>
         |<p>
         |Copyright (c) 2012&ndash;2015 Hanns Holger Rutz. All rights reserved.<p>
         |This software is published under the ${Mellite.license}<p>
         |<p>
         |<a href="$url">$addr</a>
         |""".stripMargin
    val lb = new Label(html) {
      // cf. http://stackoverflow.com/questions/527719/how-to-add-hyperlink-in-jlabel
      // There is no way to directly register a HyperlinkListener, despite hyper links
      // being rendered... A simple solution is to accept any mouse click on the label
      // to open the corresponding website.
      cursor = java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.HAND_CURSOR)
      listenTo(mouse.clicks)
      reactions += {
        case MouseClicked(_, _, _, 1, false) => Desktop.browseURI(new URL(url).toURI)
      }
    }

    OptionPane.message(message = lb.peer, icon = Logo.icon(128)).show(None, title = "About")
  }

  lazy val instance: Menu.Root = {
    import Menu._
    import KeyStrokes._

    val itPrefs = Item.Preferences(Application)(ActionPreferences())
    val itQuit  = Item.Quit(Application)
    val itAbout = Item.About(Application)(showAbout())

    val mFile = Group("file", "File")
      .add(Group("new", "New")
        .add(Item("new-doc", ActionNewWorkspace))
        .add(Item("repl",    InterpreterFrame.Action))
      )
      .add(Item("open", ActionOpenWorkspace))
      .add(ActionOpenWorkspace.recentMenu)
      .add(Item("close",              proxy("Close",                    menu1 + Key.W)))
      .add(Item("close-all", ActionCloseAllWorkspaces))
      .addLine()
      .add(Item("bounce",             proxy("Bounce...",                menu1 + Key.B)))
      // .add(Item("bounce-transform",   proxy("Bounce And Transform...",  (menu1 + shift + Key.B))))
    if (itQuit.visible) mFile.addLine().add(itQuit)

    val keyRedo = if (Desktop.isWindows) menu1 + Key.Y else menu1 + shift + Key.Z

    val mEdit = Group("edit", "Edit")
      .add(Item("undo",               proxy("Undo",                     menu1 + Key.Z)))
      .add(Item("redo",               proxy("Redo",                     keyRedo)))
      .addLine()
      .add(Item("cut",                proxy("Cut",                      menu1 + Key.X)))
      .add(Item("copy",               proxy("Copy",                     menu1 + Key.C)))
      .add(Item("paste",              proxy("Paste",                    menu1 + Key.V)))
      .add(Item("delete",             proxy("Delete",                   plain + Key.BackSpace)))
      .addLine()
      .add(Item("select-all",         proxy("Select All",               menu1 + Key.A)))

    if (itPrefs.visible /* && Desktop.isLinux */) mEdit.addLine().add(itPrefs)

    val mActions = Group("actions", "Actions")
      .add(Item("stop-all-sound",     proxy("Stop All Sound",           menu1 + Key.Period)))
      .add(Item("debug-print",        proxy("Debug Print",              menu2 + Key.P)))
      .add(Item("debug-threads")("Debug Thread Dump")(debugThreads()))
      .add(Item("dump-osc")("Dump OSC" -> (ctrl + shift + Key.D))(dumpOSC()))
      .add(Item("window-shot",        proxy("Export Window as PDF...")))

    val mView = Group("view", "View")
      .add(Item("show-log" )("Show Log Window"  -> (menu1         + Key.P))(Mellite.logToFront()))
      .add(Item("clear-log")("Clear Log Window" -> (menu1 + shift + Key.P))(Mellite.clearLog  ()))

    //    // --- timeline menu ---
    //    val mTimeline = Group("timeline", "Timeline")
    //      // .add(Item("trimToSelection",    proxy("Trim to Selection",        (menu1 + Key.F5))))
    //      .add(Item("insertSpan",         proxy("Insert Span...",           menu1 + shift + Key.E)))
    //      .add(Item("clearSpan",          proxy("Clear Selected Span",      menu1 + Key.BackSlash)))
    //      .add(Item("removeSpan",         proxy("Remove Selected Span",     menu1 + shift + Key.BackSlash)))
    //      .add(Item("dupSpanToPos",       "Duplicate Span to Cursor"))
    //      .addLine()
    //      .add(Item("nudgeAmount",        "Nudge Amount..."))
    //      .add(Item("nudgeLeft",          proxy("Nudge Objects Backward",   plain + Key.Minus)))
    //      .add(Item("nudgeRight",         proxy("Nudge Objects Forward",    plain + Key.Plus)))
    //      .addLine()
    //      .add(Item("selectFollowing",    proxy("Select Following Objects", menu2 + Key.F)))
    //      .add(Item("alignObjStartToPos", "Align Objects Start To Cursor"))
    //      .add(Item("splitObjects",       proxy("Split Selected Objects",   menu2 + Key.Y)))
    //      .addLine()
    //      .add(Item("selStopToStart",     "Flip Selection Backward"))
    //      .add(Item("selStartToStop",     "Flip Selection Forward"))

    //    val mOperation = Group("operation", "Operation")
    //      .add(Item("cursorFollows",      "Cursor Follows Playback Head"))

    // if (itPrefs.visible && !Desktop.isLinux) mOperation.addLine().add(itPrefs)

    val res = Root()
      .add(mFile)
      .add(mEdit)
      .add(mActions)
      .add(mView)
      // .add(mTimeline)
      // .add(mOperation)

    if (itAbout.visible) {
      val mHelp = Group("help", "Help").add(itAbout)
      res.add(mHelp)
    }

    res
  }

  private def debugThreads(): Unit = {
    import scala.collection.JavaConverters._
    val m = Thread.getAllStackTraces.asScala.toVector.sortBy(_._1.getId)
    println("Id__ State_________ Name___________________ Pri")
    m.foreach { case (t, _) =>
      println(f"${t.getId}%3d  ${t.getState}%-13s  ${t.getName}%-23s  ${t.getPriority}%2d")
    }
    m.foreach { case (t, stack) =>
      println()
      println(f"${t.getId}%3d  ${t.getState}%-13s  ${t.getName}%-23s")
      if (t == Thread.currentThread()) println("    (self)")
      else stack.foreach { elem =>
        println(s"    $elem")
      }
    }
  }

  private var dumpMode: osc.Dump = osc.Dump.Off

  private def dumpOSC(): Unit = {
    val sOpt = TxnExecutor.defaultAtomic { itx =>
      implicit val tx = Txn.wrap(itx)
      Mellite.auralSystem.serverOption
    }
    sOpt.foreach { s =>
      dumpMode = if (dumpMode == osc.Dump.Off) osc.Dump.Text else osc.Dump.Off
      s.peer.dumpOSC(dumpMode, filter = {
        case m: osc.Message if m.name == "/$meter" => false
        case _ => true
      })
      println(s"DumpOSC is ${if (dumpMode == osc.Dump.Text) "ON" else "OFF"}")
    }
  }
}