/*
 *  FrameImpl.scala
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
package impl
package audiofile

import de.sciss.lucre.stm
import de.sciss.synth.proc.AuralSystem
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import de.sciss.file._
import de.sciss.lucre.synth.Sys

object FrameImpl {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.AudioGrapheme[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], aural: AuralSystem): AudioFileFrame[S] = {
    val afv       = AudioFileView(doc, elem)
    val name      = elem.name.value
    val file      = elem.entity.value.artifact
    val view      = new Impl(doc, afv, name, file)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](val document: Document[S], afv: AudioFileView[S], name: String, _file: File)
                                       (implicit cursor: stm.Cursor[S])
    extends AudioFileFrame[S] with ComponentHolder[Window] {

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      guiFromTx(comp.dispose())
    }

    private def disposeData()(implicit tx: S#Tx): Unit =
      afv.dispose()

    private def frameClosing(): Unit =
      cursor.step { implicit tx =>
        disposeData()
      }

    def guiInit(): Unit = {
      val fileName = _file.base
      comp = new WindowImpl {
        def handler = Mellite.windowHandler
        def style   = Window.Regular
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