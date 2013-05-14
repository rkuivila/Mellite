package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.{Grapheme, Sys}
import de.sciss.lucre.stm
import Element.AudioGrapheme
import scala.swing.{Button, BoxPanel, Orientation, Swing, BorderPanel, Component}
import java.awt.Color
import de.sciss.audiowidgets.Transport
import Swing._
import de.sciss.span.Span
import de.sciss.mellite.impl.TimelineModelImpl
import de.sciss.sonogram
import javax.swing.{TransferHandler, ImageIcon}
import javax.swing.TransferHandler.TransferSupport

object AudioFileViewImpl {
  def apply[S <: Sys[S]](document: Document[S], element: AudioGrapheme[S])(implicit tx: S#Tx): AudioFileView[S] = {
    val res = new Impl(document: Document[S], tx.newHandle(element))
    val f   = element.entity.value // .artifact // store.resolve(element.entity.value.artifact)
    guiFromTx(res.guiInit(f))
    res
  }

  private final class Impl[S <: Sys[S]](val document: Document[S], holder: stm.Source[S#Tx, AudioGrapheme[S]])
    extends AudioFileView[S] {
    impl =>

    var component: Component = _

    private var _sono: sonogram.Overview = _

    def dispose()(implicit tx: S#Tx) {
      guiFromTx {
        SonogramManager.release(_sono)
      }
    }

    def guiInit(snapshot: Grapheme.Value.Audio) {
      // println("AudioFileView guiInit")
      _sono = SonogramManager.acquire(snapshot.artifact)
      // import SonogramManager.executionContext
      //      sono.onComplete {
      //        case x => println(s"<view> $x")
      //      }
      val tlm       = new TimelineModelImpl(Span(0L, _sono.inputSpec.numFrames), _sono.inputSpec.sampleRate)
      val sonoView  = new AudioFileViewJ(_sono, tlm)

      val timeDisp  = TimeDisplay(tlm)

      import Transport._
      val transport = Transport.makeButtonStrip(Seq(
        GoToBegin   {},
        Rewind      {},
        Stop        {},
        Play        {},
        FastForward {},
        Loop        {}
      ))
      transport.button(Stop).foreach(_.selected = true)

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          HStrut(4),
          new AudioFileDnD.Button(document, holder, snapshot, tlm),
          new BusSinkButton[S](impl),
          HGlue,
          HStrut(4),
          timeDisp.component,
          HStrut(8),
          transport,
          HStrut(4)
        )
      }

      val pane = new BorderPanel {
        layoutManager.setVgap(2)
        add(transportPane,      BorderPanel.Position.North )
        add(sonoView.component, BorderPanel.Position.Center)
      }

      component = pane
    }

    def element(implicit tx: S#Tx): AudioGrapheme[S] = holder()
  }

  private final class BusSinkButton[S <: Sys[S]](view: Impl[S]) extends Button("Drop bus") {
    icon        = new ImageIcon(Mellite.getClass.getResource("dropicon16.png"))
    GUI.fixWidth(this)
    foreground  = Color.gray
    focusable   = false

    private var item = Option.empty[stm.Source[S#Tx, Element.Int[S]]]

    peer.setTransferHandler(new TransferHandler {
      // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      override def canImport(support: TransferSupport): Boolean = {
        if (support.isDataFlavorSupported(FolderView.selectionFlavor) &&
           ((support.getSourceDropActions & TransferHandler.COPY) != 0)) {
          support.setDropAction(TransferHandler.COPY)
          true
        } else false
      }

      override def importData(support: TransferSupport): Boolean = {
        val t     = support.getTransferable
        val data  = t.getTransferData(FolderView.selectionFlavor).asInstanceOf[FolderView.SelectionDnDData[S]]
        if (data.document == view.document) {
          val ints = data.selection.collect {
            case (_, ev: ElementView.Int[S]) => (ev.name, ev.element)
          }
          ints.headOption match {
            case Some((name, it)) =>
              item        = Some(it)
              text        = name
              foreground  = null
              repaint()
              true
            case _ => false
          }
        } else {
          false
        }
      }
    })
  }
}