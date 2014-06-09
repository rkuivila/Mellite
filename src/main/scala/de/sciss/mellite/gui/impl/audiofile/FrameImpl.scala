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
package audiofile

import de.sciss.lucre.stm
import de.sciss.synth.proc
import proc.{AudioGraphemeElem, Obj, AuralSystem}
import de.sciss.desktop.Window
import de.sciss.file._
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._
import proc.Implicits._
import scala.swing.Component

object FrameImpl {
  def apply[S <: Sys[S]](doc: Workspace[S], obj: Obj.T[S, AudioGraphemeElem])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): AudioFileFrame[S] = {
    implicit val aural = Mellite.auralSystem
    val afv       = AudioFileView(doc, obj)
    val name      = obj.attr.name
    val file      = obj.elem.peer.value.artifact
    val view      = new Impl(doc, afv, name, file)
    deferTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](val document: Workspace[S], afv: AudioFileView[S], name: String, _file: File)
                                       (implicit cursor: stm.Cursor[S])
    extends AudioFileFrame[S] with WindowHolder[Window] {

    def component: Component = contents.component

    def contents: AudioFileView[S] = afv

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      deferTx(window.dispose())
    }

    private def disposeData()(implicit tx: S#Tx): Unit =
      afv.dispose()

    private def frameClosing(): Unit =
      cursor.step { implicit tx =>
        disposeData()
      }

    def guiInit(): Unit = {
      val fileName = _file.base
      window = new WindowImpl {
        component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
        title       = if (name == fileName) name else s"$name - $fileName"
        file        = Some(_file)
        contents    = afv.component
        reactions += {
          case Window.Closing(_) => frameClosing()
        }
        pack()
        // centerOnScreen()
        GUI.placeWindow(this, 1f, 0.75f, 24)
        front()
      }
    }
  }
}