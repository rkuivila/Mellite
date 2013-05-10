package de.sciss
package mellite
package gui
package impl

import scala.swing.{FlowPanel, Button, Component, BorderPanel}
import synth.proc

object DocumentCursorsFrameImpl {
  type S = proc.Confluent
  type D = S#D

  def apply(document: ConfluentDocument)(implicit tx: S#Tx): DocumentCursorsFrame = {
    val view = new Impl(document)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class CursorView(elem: Cursors[S, D])

  private final class Impl(val document: ConfluentDocument)
    extends DocumentCursorsFrame with ComponentHolder[desktop.Window] {

    def guiInit() {
      val ggAdd = Button("+") {
        println("Add")
      }
      ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      val ggDelete: Button = Button("\u2212") {
        println("Delete")
      }
      ggDelete.enabled = false
      ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val ggView: Button = Button("View") {
        println("View")
      }
      ggView.enabled = false
      ggView.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val folderButPanel = new FlowPanel(ggAdd, ggDelete, ggView)

      lazy val folderPanel = new BorderPanel {
        // add(folderView.component, BorderPanel.Position.Center)
        add(folderButPanel, BorderPanel.Position.South )
      }
      comp = new desktop.impl.WindowImpl {
        def style       = desktop.Window.Regular
        def handler     = Mellite.windowHandler

        title           = document.folder.nameWithoutExtension
        file            = Some(document.folder)
        closeOperation  = desktop.Window.CloseIgnore
        contents        = folderPanel

        pack()
        // centerOnScreen()
        front()
        // add(folderPanel, BorderPanel.Position.Center)
      }
    }
  }
}