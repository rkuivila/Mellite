/*
 *  RecursionFrameImpl.scala
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

package de.sciss
package mellite
package gui
package impl

import de.sciss.lucre.artifact.Artifact
import de.sciss.synth.proc.{Code, Obj, AuralSystem}
import lucre.stm
import java.io.File
import de.sciss.desktop.{Desktop, DialogSource}
import scala.swing.{Component, BorderPanel, FlowPanel, ProgressBar, Button, Alignment, Label}
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
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.swingplus.GroupPanel
import de.sciss.lucre.swing.{defer, deferTx}
import proc.Implicits._
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.model.impl.ModelImpl

object RecursionFrameImpl {
  private final case class ViewData(name: String, deployed: File, product: File) {
    def sameFiles: Boolean = deployed != product
  }

  def apply[S <: Sys[S]](obj: Obj.T[S, Recursion.Elem])
                        (implicit tx: S#Tx, _workspace: Workspace[S],
                         cursor: stm.Cursor[S], compiler: Code.Compiler): RecursionFrame[S] = {
    val view = new ViewImpl[S] {
      val recH      = tx.newHandle(obj)
      val _spec     = obj.elem.peer.productSpec
      val _aural    = Mellite.auralSystem

      private def mkView()(implicit tx: S#Tx): ViewData = {
        val name      = obj.attr.name
        val rec       = recH()
        val deployed  = rec.elem.peer.deployed.elem.peer.artifact.value
        val product   = rec.elem.peer.product.value
        val _depFile  = deployed
        val _prodFile = product
        ViewData(name, deployed = _depFile, product = _prodFile)
      }

      private var _viewData = mkView()

      def viewData: ViewData = _viewData

      val observer  = obj.elem.changed.react { implicit tx => {
        case _ =>
          // println(s"Observed: $x")
          val v = mkView()
          deferTx {
            _viewData = v
            guiUpdate()
          }
      }}
      deferTx {
        guiInit()
      }
    }

    val res = new FrameImpl(view)
    res.init()
    res
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

  private final class FrameImpl[S <: Sys[S]](val view: ViewImpl[S])
    extends WindowImpl[S] with RecursionFrame[S] {

    override protected def initGUI(): Unit =
      view.addListener { case viewData =>
        title = viewData.name
        windowFile = Some(viewData.deployed)
      }
  }

  private abstract class ViewImpl[S <: Sys[S]](implicit val workspace: Workspace[S], val cursor: stm.Cursor[S],
                                               compiler: Code.Compiler)
    extends ViewHasWorkspace[S]
    with ComponentHolder[Component]
    with ModelImpl[ViewData] {

    protected def recH      : stm.Source[S#Tx, Recursion.Obj[S]]
    protected def viewData  : ViewData
    protected def _spec     : AudioFileSpec
    protected implicit def _aural    : AuralSystem
    protected def observer  : Disposable[S#Tx]

    // def component: Component = window.c

    final def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      // deferTx(window.dispose())
    }

    private def disposeData()(implicit tx: S#Tx): Unit =
      observer.dispose()

    def frameClosing(): Unit =
      cursor.step { implicit tx =>
        disposeData()
      }

    private var ggDeployed: Label = _
    private var ggProduct : Label = _
    private var updateDeployed: Button = _
    private var currentProc = Option.empty[Processor[Any, _]]

    final protected def guiUpdate(): Unit = {
      ggDeployed.text = viewData.deployed.name
      ggProduct .text = viewData.product .name

      val enabled = currentProc.isEmpty
      updateDeployed.enabled  = enabled && viewData.sameFiles

      dispatch(viewData)
    }

    final protected def guiInit(): Unit = {
      // val fileName = deployed.nameWithoutExtension

      val ggProgress    = new ProgressBar
      val ggStopProcess = Button("Abort") {
        currentProc.foreach(_.abort())
      }
      desktop.Util.fixWidth(ggProgress, 480)
      GUI.round   (ggStopProcess)

      val lbDeployed    = new Label("Deployed Artifact:"   , ObjView.AudioGrapheme.icon, Alignment.Right)
      lbDeployed.peer.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT)
      ggDeployed        = new Label("---") // don't use `null`, it will stick to a width of zero
      desktop.Util.fixWidth(ggDeployed, 128)
      val lbProduct     = new Label("Most Recent Artifact:", ObjView.AudioGrapheme.icon, Alignment.Right)
      lbProduct.peer.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT)
      ggProduct         = new Label("---")
      desktop.Util.fixWidth(ggProduct, 128)

      val panelProgress = new FlowPanel(ggProgress, ggStopProcess) {
        preferredSize = preferredSize
        minimumSize   = preferredSize
        maximumSize   = preferredSize
      }

      def performProductUpdate(): Unit = {
        val b         = viewData.product.parent
        val (n0,ext)  = viewData.product.baseAndExt
        val i         = n0.lastIndexOf('_')
        val n         = if (i < 0) n0 else n0.substring(0, i)

        @tailrec def loopFile(i: Int): File = {
          val f = b / f"${n}_$i%04d.$ext"
          if (!f.exists()) f else loopFile(i + 1)
        }

        val ftOpt = cursor.step { implicit tx =>
          recH().elem.peer.transform.map(_.elem.peer.value) match {
            case Some(ft: Code.FileTransform) => Some(ft)
            case _ => None
          }
        }
        val newFile     = loopFile(1)
        val bounceFile  = if (ftOpt.isDefined) File.createTempFile("bounce", s".${_spec.fileType.extension}") else newFile

        def embed(): Unit = {
          processStopped()
          cursor.step { implicit tx =>
            val product = recH().elem.peer.product
            product.modifiableOption match {
              case (/* Some(locM), */ Some(artM)) =>
                val newChild  = Artifact.relativize(artM.location.directory, newFile)
                artM.child_=(newChild)

              case _ =>
                println("Warning: Product artifact is not modifiable !?")
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
        val (groupH, gain, span, channels, audio) = cursor.step { implicit tx =>
          // import proc.ProcGroup.serializer
          val e         = recH().elem.peer
          val _groupH   = tx.newHandle(e.group)
          val _gain     = e.gain
          val _span     = e.span
          val _audio    = e.deployed.elem.peer.value
          val _channels = e.channels
          (_groupH, _gain, _span, _channels, _audio)
        }
        val server            = Server.Config()
        ActionBounceTimeline.specToServerConfig(file, audio.spec, server)

        val pSet    = ActionBounceTimeline.PerformSettings(groupH, server, gain, span, channels)
        val pBounce = ActionBounceTimeline.perform(workspace, pSet)
        monitor("Bounce", pBounce) { _ => success }
      }

      def performMatch(): Unit = {
        val file = File.createTempFile("bounce", "." + _spec.fileType.extension)
        performBounce(file) {
          val pCompare = new FileComparison(viewData.deployed, file)
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
        updateDeployed.enabled  = enabled && viewData.sameFiles
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
          case Failure(e) =>
            defer {
              processStopped()
              DialogSource.Exception(e -> title).show(None) // XXX TODO: Some(window))
            }
        }
      }

      lazy val viewDeployed: Button = Button("View") {
        cursor.step { implicit tx =>
          AudioFileFrame(recH().elem.peer.deployed)
        }
      }
      lazy val matchDeployed: Button = Button("Match") {
        performMatch()
      }
      updateDeployed = Button("Update \u2713") {
        cursor.step { implicit tx =>
          recH().elem.peer.iterate()
        }
      }
      lazy val viewProduct: Button = Button("View") {
        Desktop.revealFile(viewData.product)
      }
      lazy val dummyProduct = new Label(null)
      lazy val updateProduct: Button = Button("Update \u2697") {
        performProductUpdate()
      }
      GUI.round(viewDeployed, matchDeployed, updateDeployed, viewProduct, updateProduct)

      updateDeployed.enabled = viewData.sameFiles

      val box = new GroupPanel {
        horizontal = Seq(
          Par(lbDeployed    , lbProduct    ),
          Par(ggDeployed    , ggProduct    ),
          Par(viewDeployed  , viewProduct  ),
          Par(matchDeployed , dummyProduct ),
          Par(updateDeployed, updateProduct)
        )
        vertical = Seq(
          Par(Baseline)(lbDeployed, ggDeployed, viewDeployed, matchDeployed, updateDeployed),
          Par(Baseline)(lbProduct , ggProduct , viewProduct , dummyProduct , updateProduct )
        )
        linkHorizontalSize(viewDeployed, matchDeployed, updateDeployed, viewProduct, dummyProduct, updateProduct)
        linkHorizontalSize(lbDeployed, lbProduct)
      }

      val panel = new BorderPanel {
        add(box, BorderPanel.Position.Center)
        add(panelProgress, BorderPanel.Position.South)
      }

      // val frame = new Frame(this, panel)
      guiUpdate()
      processStopped()
      // GUI.centerOnScreen(frame)
      // frame.front()
      // window = frame

      component = panel
    }
  }

  //  private class Frame[S <: Sys[S]](impl: Impl[S], val c: Component) extends WindowImpl {
  //    contents    = c
  //    reactions += {
  //      case Window.Closing(_) => impl.frameClosing()
  //    }
  //    resizable   = false
  //    pack()
  //
  //    def setTitle(n: String): Unit = title_=(n)
  //    // def setFile (f: File  ): Unit = file_=(Some(f))
  //  }
}