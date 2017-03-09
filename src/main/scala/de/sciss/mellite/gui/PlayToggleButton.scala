 /*
 *  PlayToggleButton.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite.gui

import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.Mellite
import de.sciss.synth.proc.{SoundProcesses, Transport, WorkspaceHandle}

import scala.swing.ToggleButton
import scala.swing.event.ButtonClicked

object PlayToggleButton {
  def apply[S <: Sys[S]](transport: Transport[S])(implicit tx: S#Tx): PlayToggleButton[S] =
    new Impl(transport, disposeTransport = false).init()

  def apply[S <: SSys[S]](obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      workspace: WorkspaceHandle[S]): PlayToggleButton[S] = {
    val t = Transport[S](Mellite.auralSystem)
    t.addObject(obj)
    new Impl(t, disposeTransport = true).init()
  }

  private final class Impl[S <: Sys[S]](val transport: Transport[S], disposeTransport: Boolean)
    extends PlayToggleButton[S] with ComponentHolder[ToggleButton] {

    private[this] var obs: Disposable[S#Tx] = _

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      if (disposeTransport) transport.dispose()
    }

    private def select(state: Boolean)(implicit tx: S#Tx): Unit =
      deferTx {
        component.selected = state
      }

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      obs = transport.react { implicit tx => {
        case Transport.Play(_, _) => select(true )
        case Transport.Stop(_, _) => select(false)
        case _ =>
      }}
      this
    }

    private def guiInit(): Unit = {
      val ggPower = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val sel = selected
            SoundProcesses.atomic[S, Unit] { implicit tx =>
              transport.stop()
              transport.seek(0L)
              if (sel) transport.play()
            } (transport.scheduler.cursor)
        }
      }
      val shpPower          = raphael.Shapes.Power _
      ggPower.icon          = GUI.iconNormal  (shpPower)
      ggPower.disabledIcon  = GUI.iconDisabled(shpPower)
      ggPower.tooltip       = "Toggle DSP"
      component             = ggPower
    }
  }
}
trait PlayToggleButton[S <: Sys[S]] extends View[S] {
  def transport: Transport[S]
}