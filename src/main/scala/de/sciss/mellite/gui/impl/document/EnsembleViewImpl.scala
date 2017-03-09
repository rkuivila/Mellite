/*
 *  EnsembleViewImpl.scala
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

package de.sciss.mellite
package gui
package impl
package document

import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.{BooleanCheckBoxView, View, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.swingplus.Separator
import de.sciss.synth.proc.{Ensemble, SoundProcesses, Transport, Workspace}

import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Component, Label, Orientation, Swing, ToggleButton}
import Swing._

object EnsembleViewImpl {
  def apply[S <: Sys[S]](ensObj: Ensemble[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S], undoManager: UndoManager): Impl[S] = {
    val ens     = ensObj
    val folder  = FolderView(ens.folder)
    val folder1 = new FolderFrameImpl.ViewImpl[S](folder)
    folder1.init()
    val playing = BooleanCheckBoxView(ens.playing, "Playing State")
    val transport = Transport[S](Mellite.auralSystem)
    transport.addObject(ensObj)
    val res = new Impl(tx.newHandle(ensObj), transport, folder1, playing)
    res.init()
    res
  }

  final class Impl[S <: Sys[S]](ensembleH: stm.Source[S#Tx, Ensemble[S]], val transport: Transport[S],
                                        val view: FolderFrameImpl.ViewImpl[S], playing: View[S])
                                       (implicit val undoManager: UndoManager, val workspace: Workspace[S],
                                        val cursor: stm.Cursor[S])
    extends ComponentHolder[Component] with EnsembleView[S] { impl =>

    def ensemble(implicit tx: S#Tx): Ensemble[S] = ensembleH()

    def folderView: FolderView[S] = view.peer

    private[this] var obsTransp: Disposable[S#Tx] = _
    private[this] var ggPower: ToggleButton = _

    def init()(implicit tx: S#Tx): this.type = {
      deferTx {
        guiInit()
      }
      obsTransp = transport.react { implicit tx => {
        case Transport.Play(_, _) =>
          deferTx {
            ggPower.selected = true
          }
        case Transport.Stop(_, _) =>
          deferTx {
            ggPower.selected = false
          }
        case _ =>
      }}
      this
    }

    def guiInit(): Unit = {
      ggPower = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val sel = selected
            SoundProcesses.atomic[S, Unit] { implicit tx =>
              transport.stop()
              transport.seek(0L)
              if (sel) transport.play()
            } (impl.cursor)
        }
      }
      val shpPower = raphael.Shapes.Power _
      ggPower.icon          = GUI.iconNormal  (shpPower)
      ggPower.disabledIcon  = GUI.iconDisabled(shpPower)
      ggPower.tooltip = "Toggle DSP"

      component = new BoxPanel(Orientation.Vertical) {
        contents += view.component
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
      obsTransp .dispose()
      transport .dispose()
      view      .dispose()
      playing   .dispose()
    }
  }
}
