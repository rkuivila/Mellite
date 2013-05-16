package de.sciss
package mellite
package gui
package impl

import synth.proc.Sys
import lucre.stm
import java.io.File
import desktop.Window
import desktop.impl.WindowImpl
import scalaswingcontrib.group.GroupPanel
import scala.swing.{Button, Alignment, Label}
import language.reflectiveCalls

object RecursionFrameImpl {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Recursion[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): RecursionFrame[S] = {
    val name      = elem.name.value
    val recursion = elem.entity
    val deployed  = recursion.deployed.entity.artifact.value
    val product   = recursion.product.value
    val spec      = recursion.productSpec
    val view      = new Impl(doc, elem, name, deployed)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](val document: Document[S], elem: Element.Recursion[S], name: String, deployed: File)
                                       (implicit _cursor: stm.Cursor[S])
    extends RecursionFrame[S] with ComponentHolder[Window] {

    def dispose()(implicit tx: S#Tx) {
      disposeData()
      guiFromTx(comp.dispose())
    }

    private def disposeData()(implicit tx: S#Tx) {
      // afv.dispose()
    }

    private def frameClosing() {
      _cursor.step { implicit tx =>
        disposeData()
      }
    }

    def guiInit() {
      // val fileName = deployed.nameWithoutExtension

      val box = new GroupPanel {
        val lbDeployed    = new Label("Deployed Artifact:", ElementView.AudioGrapheme.icon, Alignment.Right)
        val ggDeployed    = new Label(deployed.name)
        val viewDeployed  = Button("View") {
          _cursor.step { implicit tx =>
            AudioFileFrame(document, elem.entity.deployed)
          }
        }
        viewDeployed.peer.putClientProperty("JButton.buttonType", "roundRect")
        theHorizontalLayout is Sequential(lbDeployed, ggDeployed, viewDeployed)
        theVerticalLayout   is Parallel(Baseline)(lbDeployed, ggDeployed, viewDeployed)
      }

      comp = new WindowImpl {
        def handler = Mellite.windowHandler
        def style   = Window.Regular
        // component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
        title       = name
        file        = Some(deployed)
        contents    = box
        reactions += {
          case Window.Closing(_) => frameClosing()
        }
        resizable   = false
        pack()
        GUI.centerOnScreen(this)
        front()
      }
    }
  }
}