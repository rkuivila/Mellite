package de.sciss
package mellite
package gui
package impl

import de.sciss.synth.proc.{Server, Sys}
import lucre.stm
import java.io.File
import de.sciss.desktop.{DialogSource, Window}
import desktop.impl.WindowImpl
import scalaswingcontrib.group.GroupPanel
import scala.swing.{BorderPanel, FlowPanel, ProgressBar, Button, Alignment, Label}
import language.reflectiveCalls
import de.sciss.synth.proc
import de.sciss.processor.Processor
import scala.util.{Success, Failure}
import javax.swing.JPanel

object RecursionFrameImpl {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Recursion[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): RecursionFrame[S] = {
    val name      = elem.name.value
    val recursion = elem.entity
    val deployed  = recursion.deployed.entity.artifact.value
    val product   = recursion.product.value
    val spec      = recursion.productSpec
    val view      = new Impl(doc, tx.newHandle(recursion), name, deployed)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](val document: Document[S], elem: stm.Source[S#Tx, Recursion[S]],
                                        name: String, deployed: File)(implicit _cursor: stm.Cursor[S])
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

      var funStopProcess = () => ()

      val ggProgress    = new ProgressBar
      val ggStopProcess = Button("Abort") {
        funStopProcess()
      }
      GUI.fixWidth(ggProgress, 256)
      GUI.round(ggStopProcess)

      val lbDeployed    = new Label("Deployed Artifact:", ElementView.AudioGrapheme.icon, Alignment.Right)
      val ggDeployed    = new Label(deployed.name)

      val panelProgress = new FlowPanel(ggProgress, ggStopProcess) {
        preferredSize = preferredSize
        minimumSize   = preferredSize
        maximumSize   = preferredSize
      }

      def performMatch() {
        val (groupH, gain, span, channels, spec) = _cursor.step { implicit tx =>
          import proc.ProcGroup.serializer
          val e         = elem()
          val _groupH   = tx.newHandle(e.group)
          val _gain     = e.gain
          val _span     = e.span
          val _spec     = e.deployed.entity.value.spec
          val _channels = e.channels
          (_groupH, _gain, _span, _channels, _spec)
        }
        val file              = File.createTempFile("bounce", ".w64")
        val server            = Server.Config()
        ActionBounceTimeline.specToServerConfig(file, spec, server)

        val pSet    = ActionBounceTimeline.PerformSettings(groupH, server, gain, span, channels)
        val p       = ActionBounceTimeline.perform(document, pSet)
        monitor("Match", p) { _ =>
          GUI.defer {
            println("Match bounce done.")
            processStopped()
            file.delete()
          }
        }
      }

      def processStopped() {
        matchDeployed.enabled = true
        ggProgress.visible    = false
        ggStopProcess.visible = false
      }

      def monitor[A](title: String, p: Processor[A, _])(onSuccess: A => Unit) {
        matchDeployed.enabled = false
        ggProgress.visible    = true
        ggStopProcess.visible = true
        ggStopProcess.requestFocus()

        p.addListener {
          case prog @ Processor.Progress(_, _) => GUI.defer {
            ggProgress.value = prog.toInt
          }
        }
        p.onComplete {
          case Success(value) => onSuccess(value)
          case Failure(Processor.Aborted()) =>
            GUI.defer(processStopped())
          case Failure(e: Exception) => // XXX TODO: Desktop should allow Throwable for DialogSource.Exception
            GUI.defer {
              processStopped()
              DialogSource.Exception(e -> title).show(Some(comp))
            }
          case Failure(e) =>
            GUI.defer(processStopped())
            e.printStackTrace()
        }
      }

      lazy val viewDeployed: Button = Button("View") {
        _cursor.step { implicit tx =>
          AudioFileFrame(document, elem().deployed)
        }
      }
      lazy val matchDeployed: Button = Button("Match") {
        performMatch()
      }
      GUI.round(viewDeployed, matchDeployed)

      val box = new GroupPanel {
        theHorizontalLayout is Sequential(lbDeployed, ggDeployed, viewDeployed, matchDeployed)
        theVerticalLayout   is Parallel(Baseline)(lbDeployed, ggDeployed, viewDeployed, matchDeployed)
        linkHorizontalSize(viewDeployed, matchDeployed)
      }

      processStopped()

      comp = new WindowImpl {
        def handler = Mellite.windowHandler
        def style   = Window.Regular
        // component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
        title       = name
        file        = Some(deployed)
        contents    = new BorderPanel {
          add(box          , BorderPanel.Position.Center)
          add(panelProgress, BorderPanel.Position.South)
        }
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