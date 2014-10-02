/*
 *  EnsembleViewImpl.scala
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
package document

import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.swingplus.Separator
import de.sciss.synth.proc.{Transport, Ensemble}

import scala.swing.event.ButtonClicked
import scala.swing.{Swing, Label, ToggleButton, Orientation, BoxPanel, Component}
import Swing._

object EnsembleViewImpl {
  def apply[S <: Sys[S]](ensObj: Ensemble.Obj[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S], undoManager: UndoManager): EnsembleView[S] = {
    val ens     = ensObj.elem.peer
    val folder  = FolderView(ens.folder)
    val folder1 = new FolderFrameImpl.ViewImpl[S, S](folder) {
      protected def nameObserver: Disposable[S#Tx] = ExprView.DummyDisposable
    }
    folder1.init()
    val playing = BooleanCheckBoxView(ens.playing, "Playing State")
    val transport = Transport[S](Mellite.auralSystem)
    transport.addObject(ensObj)
    val res = new Impl(tx.newHandle(ensObj), transport, folder1, playing)
    deferTx {
      res.guiInit()
    }
    res
  }

  private final class Impl[S <: Sys[S]](ensembleH: stm.Source[S#Tx, Ensemble.Obj[S]], transport: Transport[S],
                                        folder: View[S], playing: View[S])
                                       (implicit val undoManager: UndoManager, val workspace: Workspace[S],
                                        val cursor: stm.Cursor[S])
    extends ComponentHolder[Component] with EnsembleView[S] { impl =>

    def ensemble(implicit tx: S#Tx): Ensemble[S] = ensembleH().elem.peer

    def guiInit(): Unit = {
      val ggPower = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            impl.cursor.step { implicit tx =>
              transport.stop()
              transport.seek(0L)
              if (selected) transport.play()
            }
        }
      }
      val shpPower = raphael.Shapes.Power _
      ggPower.icon          = GUI.iconNormal  (shpPower)
      ggPower.disabledIcon  = GUI.iconDisabled(shpPower)
      ggPower.tooltip = "Toggle DSP"

      component = new BoxPanel(Orientation.Vertical) {
        contents += folder.component
        contents += Separator()
        contents += VStrut(2)
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += ggPower
          contents += HStrut(16)
          contents += new Label("Playing:")
          contents += HStrut(4)
          contents += playing.component
        }
        contents += VStrut(2)
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      transport.dispose()
      folder   .dispose()
      playing  .dispose()
    }
  }
}
