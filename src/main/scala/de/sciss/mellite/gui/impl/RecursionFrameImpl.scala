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
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.io.AudioFile
import concurrent.blocking

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

  private final class FileComparison(a: File, b: File) extends ProcessorImpl[Boolean, FileComparison] {
    protected def body(): Boolean = blocking {
      val inA = AudioFile.openRead(a)
      try {
        val inB = AudioFile.openRead(b)
        try {
          val numCh     = inA.numChannels
          val numFrames = inA.numFrames
          if (numCh           != inB.numChannels ||
              numFrames       != inB.numFrames   ||
              inA.sampleRate  != inB.sampleRate) return false

          val bufSz   = 8192
          val bufA    = inA.buffer(bufSz)
          val bufB    = inB.buffer(bufSz)
          var remain  = numFrames
          while (remain > 0) {
            val chunk = math.min(bufSz, remain).toInt
            inA.read(bufA, 0, chunk)
            inB.read(bufB, 0, chunk)
            for (ch <- 0 until numCh) {
              val ca = bufA(ch)
              val cb = bufB(ch)
              for (i <- 0 until chunk) {
                if (ca(i) != cb(i)) return false
              }
            }
            remain -= chunk
            checkAborted()
          }
          true

        } finally {
          inB.close()
        }
      } finally {
        inA.close()
      }
    }
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
        val (groupH, gain, span, channels, audio) = _cursor.step { implicit tx =>
          import proc.ProcGroup.serializer
          val e         = elem()
          val _groupH   = tx.newHandle(e.group)
          val _gain     = e.gain
          val _span     = e.span
          val _audio    = e.deployed.entity.value
          val _channels = e.channels
          (_groupH, _gain, _span, _channels, _audio)
        }
        val file              = File.createTempFile("bounce", ".w64")
        val server            = Server.Config()
        ActionBounceTimeline.specToServerConfig(file, audio.spec, server)

        val pSet    = ActionBounceTimeline.PerformSettings(groupH, server, gain, span, channels)
        val pBounce = ActionBounceTimeline.perform(document, pSet)
        monitor("Match", pBounce) { _ =>
          val pCompare = new FileComparison(audio.artifact, file)
          pCompare.start()
          monitor("Compare", pCompare) { res =>
            processStopped()
            println(if (res) "Is up to date" else "Has changed")
            // println(s"Old file: ${audio.artifact}, new file $file")
          }
        }
      }

      def processStopped() {
        matchDeployed.enabled = true
        ggProgress.visible    = false
        ggStopProcess.visible = false
        funStopProcess        = () => ()
      }

      // `onSuccess` is called on EDT!
      def monitor[A](title: String, p: Processor[A, _])(onSuccess: A => Unit) {
        matchDeployed.enabled = false
        ggProgress.visible    = true
        ggStopProcess.visible = true
        ggStopProcess.requestFocus()
        funStopProcess        = () => p.abort()

        p.addListener {
          case prog @ Processor.Progress(_, _) => GUI.defer {
            ggProgress.value = prog.toInt
          }
        }
        p.onComplete {
          case Success(value) => GUI.defer(onSuccess(value))
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