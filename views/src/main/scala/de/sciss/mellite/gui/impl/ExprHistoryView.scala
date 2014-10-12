/*
 *  ExprHistoryView.scala
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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.lucre.stm
import de.sciss.lucre.confluent.{Sys, Cursor}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Identifiable
import de.sciss.lucre.swing.{defer, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.processor.GenericProcessor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.serial.Serializer
import de.sciss.swingplus.{SpinningProgressBar, ListView}
import de.sciss.synth.proc.Confluent

import scala.swing.{Orientation, BoxPanel, ScrollPane, Component}

object ExprHistoryView {
  type S = Confluent
  type D = S#D

  var DEBUG = false

  def apply[A](workspace: Workspace.Confluent, expr: Expr[S, A])
              (implicit tx: S#Tx, serializer: Serializer[S#Tx, S#Acc, Expr[S, A]]): ViewHasWorkspace[S] = {
    val sys       = workspace.system
    val cursor    = Cursor[S, D](tx.inputAccess)(tx.durable, sys)
    val exprH     = tx.newHandle(expr)
    val pos0      = tx.inputAccess
    val time0     = pos0.info.timeStamp
    val val0      = expr.value

    val stop = expr match {
      case hid: Identifiable[_] =>
        val id    = hid.id.asInstanceOf[Sys.ID[S]]
        val head  = de.sciss.lucre.confluent.Peek.head(id.path).toInt
        if (DEBUG) println(s"ID = $id, head = $head")
        head
      case _ => 0
    }

    val res       = new Impl[A](workspace, cursor, exprH, pos0, time0, val0, stop = stop)
    deferTx(res.guiInit())
    res
  }

  private final class Impl[A](val workspace: Workspace[S],
                              val cursor: Cursor[S, D], exprH: stm.Source[S#Tx, Expr[S, A]],
                              pos0: S#Acc, time0: Long, value0: A, stop: Int)
    extends ViewHasWorkspace[S] with ComponentHolder[Component] {

    private val mod     = ListView.Model.empty[String]
    private val format  = new SimpleDateFormat("yyyy MM dd MM | HH:mm:ss", Locale.US) // don't bother user with alpha characters

    private var busy: SpinningProgressBar = _

    private def mkString(time: Long, value: A): String = {
      val date  = format.format(new Date(time))
      s"$date    $value"
    }

    private object proc extends GenericProcessor[Unit] with ProcessorImpl[Unit, GenericProcessor[Unit]] {
      protected def body(): Unit = {
        var path    = pos0
        var ok      = true
        var time    = time0
        var value   = value0

        def addEntry(): Unit = {
          val s = mkString(time, value)
          if (DEBUG) println(s"----add $path, $s")
          defer {
            mod.prepend(s)
          }
        }

        while (ok) {
          val (newTime, newValue, newPath) = cursor.stepFrom(path) { implicit tx =>
            if (DEBUG) println(s"----path = $path")
            val v     = try {
              val expr  = exprH()
              if (DEBUG) println(s"----try $expr")
              val res   = expr.value
              if (DEBUG) println(s"----ok $res")
              res
            } catch {
              case _: NoSuchElementException => value // XXX TODO - ugly ugly ugly
            }
            val time  = path.info.timeStamp
            val path1 = path.takeUntil(time - 1L)
            (time, v, path1)
          }
          checkAborted()

          if (value != newValue) {
            addEntry()
            value     = newValue
            time      = newTime
          }

          ok    = path != newPath && (de.sciss.lucre.confluent.Peek.last(newPath).toInt >= stop)
          path  = newPath
        }
        addEntry()
      }
    }

    def guiInit(): Unit = {
      val lv      = new ListView(mod)
      lv.prototypeCellValue = s"${mkString(System.currentTimeMillis(), value0)}XXXXX"
      val scroll  = new ScrollPane(lv)
      scroll.border = null
      busy        = new SpinningProgressBar
      proc.onComplete(_ => defer(busy.spinning = false))
      proc.start()
      busy.spinning = true
      component   = new BoxPanel(Orientation.Vertical) {
        contents += scroll
        contents += busy
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      deferTx {
        proc.abort()
        busy.spinning = false
      }
      cursor.dispose()(tx.durable)    // XXX TODO - should we check the processor first?
    }
  }
}