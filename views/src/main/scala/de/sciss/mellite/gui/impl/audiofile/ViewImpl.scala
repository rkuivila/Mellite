/*
 *  ViewImpl.scala
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

package de.sciss.mellite
package gui
package impl
package audiofile

import de.sciss.synth.proc.{Timeline, Transport, ArtifactLocation, Obj, AudioGraphemeElem, AuralSystem, Grapheme, ExprImplicits}
import de.sciss.lucre.stm
import scala.swing.{Button, BoxPanel, Orientation, Swing, BorderPanel, Component}
import java.awt.Color
import Swing._
import de.sciss.span.Span
import de.sciss.sonogram
import javax.swing.{TransferHandler, ImageIcon}
import javax.swing.TransferHandler.TransferSupport
import de.sciss.audiowidgets.impl.TimelineModelImpl
import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder

object ViewImpl {
  def apply[S <: Sys[S]](obj0: Obj.T[S, AudioGraphemeElem])
                        (implicit tx: S#Tx, _workspace: Workspace[S], _cursor: stm.Cursor[S],
                         aural: AuralSystem): AudioFileView[S] = {
    val f             = obj0.elem.peer.value // .artifact // store.resolve(element.entity.value.artifact)
    // val sampleRate    = f.spec.sampleRate
    type I            = _workspace.I
    implicit val itx  = _workspace.inMemoryBridge(tx)
    val group         = Timeline[I] // proc.ProcGroup.Modifiable[I]
    // val groupObj      = Obj(ProcGroupElem(group))
    val srRatio       = f.spec.sampleRate / Timeline.SampleRate
    // val fullSpanFile  = Span(0L, f.spec.numFrames)
    val numFramesTL   = (f.spec.numFrames / srRatio).toLong
    val fullSpanTL    = Span(0L, numFramesTL)

    // ---- we go through a bit of a mess here to convert S -> I ----
    val graphemeV     = f // elem.entity.value
    val imp = ExprImplicits[I]
    import imp._
    val artifact      = obj0.elem.peer.artifact
    val artifDir      = artifact.location.directory
    val iLoc          = ArtifactLocation.Modifiable[I](artifDir)
    val iArtifact     = iLoc.add(artifact.value)
    val iGrapheme     = Grapheme.Expr.Audio[I](iArtifact, graphemeV.spec, graphemeV.offset, graphemeV.gain)
    ProcActions.insertAudioRegion[I](group, time = Span(0L, numFramesTL),
      /* track = 0, */ grapheme = iGrapheme, gOffset = 0L, bus = None)

    import _workspace.inMemoryCursor
    // val transport     = Transport[I, I](group, sampleRate = sampleRate)
    val transport = Transport[I](aural)

    import _workspace.inMemoryBridge
    val res: Impl[S, I] = new Impl[S, I] {
      val timelineModel = new TimelineModelImpl(fullSpanTL, Timeline.SampleRate)
      val workspace     = _workspace
      val cursor        = _cursor
      val holder        = tx.newHandle(obj0)
      val transportView: TransportView[I] = TransportView[I](transport, timelineModel, hasMillis = true, hasLoop = true)
    }

    deferTx {
      res.guiInit(f)
    } (tx)
    res
  }

  private abstract class Impl[S <: Sys[S], I <: Sys[I]](implicit inMemoryBridge: S#Tx => I#Tx)
    extends AudioFileView[S] with ComponentHolder[Component] { impl =>

    protected def holder       : stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]]
    protected def transportView: TransportView[I]
    protected def timelineModel: TimelineModel

    private var _sonogram: sonogram.Overview = _

    def dispose()(implicit tx: S#Tx): Unit = {
      val itx: I#Tx = tx
      transportView.transport.dispose()(itx)
      transportView.dispose()(itx)
      deferTx {
        SonogramManager.release(_sonogram)
      }
    }

    def guiInit(snapshot: Grapheme.Value.Audio): Unit = {
      // println("AudioFileView guiInit")
      _sonogram = SonogramManager.acquire(snapshot.artifact)
      // import SonogramManager.executionContext
      //      sono.onComplete {
      //        case x => println(s"<view> $x")
      //      }
      val sonogramView = new ViewJ(_sonogram, timelineModel)

      val ggDragRegion = new DnD.Button(holder, snapshot, timelineModel)

      val topPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          HStrut(4),
          ggDragRegion,
          new BusSinkButton[S](impl, ggDragRegion),
          HGlue,
          HStrut(4),
          transportView.component,
          HStrut(4)
        )
      }

      val pane = new BorderPanel {
        layoutManager.setVgap(2)
        add(topPane,                BorderPanel.Position.North )
        add(sonogramView.component, BorderPanel.Position.Center)
      }

      component = pane
    }

    def obj(implicit tx: S#Tx): Obj.T[S, AudioGraphemeElem] = holder()
  }

  private final class BusSinkButton[S <: Sys[S]](view: AudioFileView[S], export: DnD.Button[S])
    extends Button("Drop bus") {

    icon        = new ImageIcon(Mellite.getClass.getResource("dropicon16.png"))
    // this doesn't have any effect?
    // GUI.fixWidth(this)
    foreground  = Color.gray
    focusable   = false

    // private var item = Option.empty[stm.Source[S#Tx, Element.Int[S]]]

    private val trns = new TransferHandler {
      // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      override def canImport(support: TransferSupport): Boolean =
        if (support.isDataFlavorSupported(FolderView.SelectionFlavor) &&
           ((support.getSourceDropActions & TransferHandler.COPY) != 0)) {
          support.setDropAction(TransferHandler.COPY)
          true
        } else false

      override def importData(support: TransferSupport): Boolean = {
        val t     = support.getTransferable
        val data  = t.getTransferData(FolderView.SelectionFlavor).asInstanceOf[FolderView.SelectionDnDData[S]]
        (data.workspace == view.workspace) && {
          data.selection.exists { nodeView =>
            nodeView.renderData match {
              case ev: ObjView.Int[S] =>
                export.bus  = Some(ev.obj)
                text        = ev.name
                foreground  = null
                repaint()
                true

              case _ => false
            }
          }
        }
      }
    }
    peer.setTransferHandler(trns)
  }
}