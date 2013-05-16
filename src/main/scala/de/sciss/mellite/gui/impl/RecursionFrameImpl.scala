package de.sciss
package mellite
package gui
package impl

import de.sciss.synth.proc.{Artifact, Server, Sys}
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
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import concurrent.blocking
import java.awt.{ComponentOrientation, Color}
import scala.annotation.tailrec
import de.sciss.lucre.stm.Disposable

object RecursionFrameImpl {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Recursion[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): RecursionFrame[S] = {
    val name      = elem.name.value
    val recursion = elem.entity
    val deployed  = recursion.deployed.entity.artifact.value
    val product   = recursion.product.value
    val spec      = recursion.productSpec

    val view: Impl[S] = new Impl[S] {
      val document  = doc
      val recH      = tx.newHandle(recursion)
      val _name     = name
      val _depFile  = deployed
      val _prodFile = product
      val _spec     = spec
      val _cursor   = cursor
      val observer  = recursion.changed.react { implicit tx => {
        case _ => println("Recursion update!")
      }}
    }
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

  private abstract class Impl[S <: Sys[S]]
    extends RecursionFrame[S] with ComponentHolder[Window] {

    protected def recH      : stm.Source[S#Tx, Recursion[S]]
    protected def _name     : String
    protected def _depFile  : File
    protected def _prodFile : File
    protected def _spec     : AudioFileSpec
    protected implicit def _cursor   : stm.Cursor[S]
    protected def observer  : Disposable[S#Tx]

    final def dispose()(implicit tx: S#Tx) {
      disposeData()
      guiFromTx(comp.dispose())
    }

    private def disposeData()(implicit tx: S#Tx) {
      observer.dispose()
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

      val lbDeployed    = new Label("Deployed Artifact:"   , ElementView.AudioGrapheme.icon, Alignment.Left)
      lbDeployed.peer.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT)
      val ggDeployed    = new Label(_depFile.name)
      val lbProduct     = new Label("Most Recent Artifact:", ElementView.AudioGrapheme.icon, Alignment.Left)
      lbProduct.peer.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT)
      val ggProduct     = new Label(_prodFile .name)

      val panelProgress = new FlowPanel(ggProgress, ggStopProcess) {
        preferredSize = preferredSize
        minimumSize   = preferredSize
        maximumSize   = preferredSize
      }

      def performProductUpdate() {
        val b         = _prodFile.parent
        val (n0,ext)  = _prodFile.splitExtension
        val i         = n0.lastIndexOf('_')
        val n         = if (i < 0) n0 else n0.substring(0, i)

        @tailrec def loopFile(i: Int): File = {
          val f = b / f"${n}_$i%04d.$ext"
          if (!f.exists()) f else loopFile(i + 1)
        }

        val newFile   = loopFile(1)
        performBounce(newFile) {
          _cursor.step { implicit tx =>
            val product = recH().product
            (/* product.location.modifiableOption, */ product.modifiableOption) match {
              case (/* Some(locM), */ Some(artM)) =>
                val newChild  = Artifact.relativize(artM.location.directory, newFile)
                artM.child_=(newChild)

              case _ =>
                println("Woop. Product artifact is not modifiable !?")
            }
          }
        }
      }

      def performBounce(file: File)(success: => Unit) {
        val (groupH, gain, span, channels, audio) = _cursor.step { implicit tx =>
          import proc.ProcGroup.serializer
          val e         = recH()
          val _groupH   = tx.newHandle(e.group)
          val _gain     = e.gain
          val _span     = e.span
          val _audio    = e.deployed.entity.value
          val _channels = e.channels
          (_groupH, _gain, _span, _channels, _audio)
        }
        val server            = Server.Config()
        ActionBounceTimeline.specToServerConfig(file, audio.spec, server)

        val pSet    = ActionBounceTimeline.PerformSettings(groupH, server, gain, span, channels)
        val pBounce = ActionBounceTimeline.perform(document, pSet)
        monitor("Bounce", pBounce) { _ => success }
      }

      def performMatch() {
        val file = File.createTempFile("bounce", "." + _spec.fileType.extension)
        performBounce(file) {
          val pCompare = new FileComparison(_depFile, file)
          pCompare.start()
          monitor("Compare", pCompare) { res =>
            processStopped()
            // println(if (res) "Is up to date" else "Has changed")
            lbDeployed.foreground = if (res) new Color(0x00, 0xB0, 0x0) else Color.red
            // println(s"Old file: ${audio.artifact}, new file $file")
          }
        }
      }

      def updateDeployedEnabled = _depFile != _prodFile

      def updateGadgets(enabled: Boolean) {
        matchDeployed.enabled   = enabled
        updateDeployed.enabled  = enabled && updateDeployedEnabled
        updateProduct.enabled   = enabled
        ggProgress.visible      = !enabled
        ggStopProcess.visible   = !enabled
      }

      def processStopped() {
        updateGadgets(enabled = true)
        funStopProcess          = () => ()
      }

      // `onSuccess` is called on EDT!
      def monitor[A](title: String, p: Processor[A, _])(onSuccess: A => Unit) {
        updateGadgets(enabled = false)
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
          AudioFileFrame(document, recH().deployed)
        }
      }
      lazy val matchDeployed: Button = Button("Match") {
        performMatch()
      }
      lazy val updateDeployed: Button = Button("Update \u2713") {
        println("Update deployed")
      }
      lazy val viewProduct: Button = Button("View") {
        IO.revealInFinder(_prodFile)
      }
      lazy val dummyProduct = new Label(null)
      lazy val updateProduct: Button = Button("Update \u2697") {
        performProductUpdate()
      }
      GUI.round(viewDeployed, matchDeployed, updateDeployed, viewProduct, updateProduct)

      updateDeployed.enabled = updateDeployedEnabled

      val box = new GroupPanel {
        theHorizontalLayout is Sequential(
          Parallel(lbDeployed    , lbProduct    ),
          Parallel(ggDeployed    , ggProduct    ),
          Parallel(viewDeployed  , viewProduct  ),
          Parallel(matchDeployed , dummyProduct ),
          Parallel(updateDeployed, updateProduct)
        )
        theVerticalLayout   is Sequential(
          Parallel(Baseline)(lbDeployed, ggDeployed, viewDeployed, matchDeployed, updateDeployed),
          Parallel(Baseline)(lbProduct , ggProduct , viewProduct , dummyProduct , updateProduct )
        )
        linkHorizontalSize(viewDeployed, matchDeployed, updateDeployed, viewProduct, dummyProduct, updateProduct)
        linkHorizontalSize(lbDeployed, lbProduct)
      }

      processStopped()

      comp = new WindowImpl {
        def handler = Mellite.windowHandler
        def style   = Window.Regular
        // component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
        title       = _name
        file        = Some(_depFile)
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