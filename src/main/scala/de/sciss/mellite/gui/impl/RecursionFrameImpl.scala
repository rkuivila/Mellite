package de.sciss
package mellite
package gui
package impl

import de.sciss.synth.proc.{AuralSystem, Artifact, Server, Sys}
import lucre.stm
import java.io.File
import de.sciss.desktop.{DialogSource, Window}
import desktop.impl.WindowImpl
import scalaswingcontrib.group.GroupPanel
import scala.swing.{Component, BorderPanel, FlowPanel, ProgressBar, Button, Alignment, Label}
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
import de.sciss.file._

object RecursionFrameImpl {
  private final case class View(name: String, deployed: File, product: File) {
    def sameFiles: Boolean = deployed != product
  }

  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Recursion[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], aural: AuralSystem): RecursionFrame[S] = {
    new Impl[S] {
      val document  = doc
      val recH      = tx.newHandle(elem.entity)
      val _spec     = elem.entity.productSpec
      val _cursor   = cursor
      val _aural    = aural

      private def mkView()(implicit tx: S#Tx): View = {
        val name      = elem.name.value
        val rec       = recH()
        val deployed  = rec.deployed.entity.artifact.value
        val product   = rec.product.value
        val _depFile  = deployed
        val _prodFile = product
        View(name, deployed = _depFile, product = _prodFile)
      }

      private var _view = mkView()

      def view: View = _view

      val observer  = elem.changed.react { implicit tx => {
        case _ =>
          // println(s"Observed: $x")
          val v = mkView()
          guiFromTx {
            _view = v
            guiUpdate()
          }
      }}

      guiFromTx {
        guiInit()
      }
    }
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
    protected def view      : View
    protected def _spec     : AudioFileSpec
    protected implicit def _cursor   : stm.Cursor[S]
    protected implicit def _aural    : AuralSystem
    protected def observer  : Disposable[S#Tx]

    final def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      guiFromTx(comp.dispose())
    }

    private def disposeData()(implicit tx: S#Tx): Unit =
      observer.dispose()

    private def frameClosing(): Unit =
      _cursor.step { implicit tx =>
        disposeData()
      }

    private var ggDeployed: Label = _
    private var ggProduct : Label = _
    private var updateDeployed: Button = _
    private var frame     : Frame = _
    private var currentProc = Option.empty[Processor[Any, _]]

    final protected def guiUpdate(): Unit = {
      ggDeployed.text = view.deployed.name
      ggProduct .text = view.product .name
      // println(s"view.deployed = ${view.deployed}, product = ${view.product}")
      frame.setTitle(view.name)
      frame.file_=     (Some(view.deployed))
      val enabled = currentProc.isEmpty
      updateDeployed.enabled  = enabled && view.sameFiles
    }

    final protected def guiInit(): Unit = {
      // val fileName = deployed.nameWithoutExtension

      val ggProgress    = new ProgressBar
      val ggStopProcess = Button("Abort") {
        currentProc.foreach(_.abort())
      }
      GUI.fixWidth(ggProgress, 480)
      GUI.round   (ggStopProcess)

      val lbDeployed    = new Label("Deployed Artifact:"   , ElementView.AudioGrapheme.icon, Alignment.Right)
      lbDeployed.peer.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT)
      ggDeployed        = new Label("---") // don't use `null`, it will stick to a width of zero
      GUI.fixWidth(ggDeployed, 128)
      val lbProduct     = new Label("Most Recent Artifact:", ElementView.AudioGrapheme.icon, Alignment.Right)
      lbProduct.peer.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT)
      ggProduct         = new Label("---")
      GUI.fixWidth(ggProduct, 128)

      val panelProgress = new FlowPanel(ggProgress, ggStopProcess) {
        preferredSize = preferredSize
        minimumSize   = preferredSize
        maximumSize   = preferredSize
      }

      def performProductUpdate(): Unit = {
        val b         = view.product.parent
        val (n0,ext)  = view.product.baseAndExt
        val i         = n0.lastIndexOf('_')
        val n         = if (i < 0) n0 else n0.substring(0, i)

        @tailrec def loopFile(i: Int): File = {
          val f = b / f"${n}_$i%04d.$ext"
          if (!f.exists()) f else loopFile(i + 1)
        }

        val ftOpt = _cursor.step { implicit tx =>
          recH().transform.map(_.entity.value) match {
            case Some(ft: Code.FileTransform) => Some(ft)
            case _ => None
          }
        }
        val newFile     = loopFile(1)
        val bounceFile  = if (ftOpt.isDefined) File.createTempFile("bounce", "." + _spec.fileType.extension) else newFile

        def embed(): Unit = {
          processStopped()
          _cursor.step { implicit tx =>
            val product = recH().product
            product.modifiableOption match {
              case (/* Some(locM), */ Some(artM)) =>
                val newChild  = Artifact.relativize(artM.location.directory, newFile)
                artM.child_=(newChild)

              case _ =>
                println("Woop. Product artifact is not modifiable !?")
            }
          }
        }

        performBounce(bounceFile) {
          ftOpt match {
            case Some(ft) =>
              ft.execute((bounceFile, newFile, { codeProcess =>
                monitor("Transform", codeProcess) { _ =>
                  embed()
                }
              }))
            case _ => embed()
          }
        }
      }

      def performBounce(file: File)(success: => Unit): Unit = {
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

      def performMatch(): Unit = {
        val file = File.createTempFile("bounce", "." + _spec.fileType.extension)
        performBounce(file) {
          val pCompare = new FileComparison(view.deployed, file)
          pCompare.start()
          monitor("Compare", pCompare) { res =>
            processStopped()
            // println(if (res) "Is up to date" else "Has changed")
            lbDeployed.foreground = if (res) new Color(0x00, 0xB0, 0x0) else Color.red
            // println(s"Old file: ${audio.artifact}, new file $file")
          }
        }
      }

      def updateGadgets(enabled: Boolean): Unit = {
        matchDeployed .enabled  = enabled
        updateDeployed.enabled  = enabled && view.sameFiles
        updateProduct .enabled  = enabled
        ggProgress    .visible  = !enabled
        ggStopProcess .visible  = !enabled
      }

      def processStopped(): Unit = {
        updateGadgets(enabled = true)
        currentProc = None
      }

      // `onSuccess` is called on EDT!
      def monitor[A](title: String, p: Processor[A, _])(onSuccess: A => Unit): Unit = {
        updateGadgets(enabled = false)
        ggStopProcess.requestFocus()
        currentProc = Some(p)

        p.addListener {
          case prog @ Processor.Progress(_, _) => defer {
            ggProgress.value = prog.toInt
          }
        }
        p.onComplete {
          case Success(value) => defer(onSuccess(value))
          case Failure(Processor.Aborted()) =>
            defer(processStopped())
          case Failure(e: Exception) => // XXX TODO: Desktop should allow Throwable for DialogSource.Exception
            defer {
              processStopped()
              DialogSource.Exception(e -> title).show(Some(comp))
            }
          case Failure(e) =>
            defer(processStopped())
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
      updateDeployed = Button("Update \u2713") {
        _cursor.step { implicit tx =>
          recH().iterate()
        }
      }
      lazy val viewProduct: Button = Button("View") {
        IO.revealInFinder(view.product)
      }
      lazy val dummyProduct = new Label(null)
      lazy val updateProduct: Button = Button("Update \u2697") {
        performProductUpdate()
      }
      GUI.round(viewDeployed, matchDeployed, updateDeployed, viewProduct, updateProduct)

      updateDeployed.enabled = view.sameFiles

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

      val panel = new BorderPanel {
        add(box, BorderPanel.Position.Center)
        add(panelProgress, BorderPanel.Position.South)
      }
      frame = new Frame(panel)
      guiUpdate()
      processStopped()
      GUI.centerOnScreen(frame)
      frame.front()
      comp  = frame
    }

    private class Frame(c: Component) extends WindowImpl {
      def handler = Mellite.windowHandler
      def style   = Window.Regular
      // component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
      contents    = c
      reactions += {
        case Window.Closing(_) => frameClosing()
      }
      resizable   = false
      pack()

      def setTitle(n: String): Unit = title_=(n)
      // def setFile (f: File  ): Unit = file_=(Some(f))
    }
  }
}