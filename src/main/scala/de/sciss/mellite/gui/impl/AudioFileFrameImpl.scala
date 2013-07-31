package de.sciss
package mellite
package gui
package impl

import de.sciss.lucre.stm
import de.sciss.synth.proc.{AuralSystem, Sys}
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window
import de.sciss.file._

object AudioFileFrameImpl {
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