/*
 *  PanelImpl.scala
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
package realtime

import de.sciss.lucre.stm.Cursor
import de.sciss.synth.proc.{ProcGroup, Obj, ProcElem, ProcKeys, Elem, Proc, Param, ProcTransport}
import de.sciss.lucre.bitemp.BiGroup
import java.awt.{RenderingHints, Graphics2D, Color}
import collection.immutable.{IndexedSeq => Vec}
import prefuse.{Display, Visualization}
import prefuse.action.layout.graph.ForceDirectedLayout
import prefuse.action.{RepaintAction, ActionList}
import prefuse.activity.Activity
import prefuse.controls.{ZoomToFitControl, PanControl, WheelZoomControl, ZoomControl}
import javax.swing.event.{AncestorEvent, AncestorListener}
import prefuse.data
import prefuse.visual.VisualItem
import prefuse.action.assignment.ColorAction
import prefuse.util.ColorLib
import prefuse.render.DefaultRendererFactory
import prefuse.util.force.{AbstractForce, ForceItem}
import scala.swing.Component
import de.sciss.span.SpanLike
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder

object PanelImpl {
  def apply[S <: Sys[S]](transport: ProcTransport[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): InstantGroupPanel[S] = {

    //      require( EventQueue.isDispatchThread, "VisualInstantPresentation.apply must be called on EDT" )
    //      require( Txn.findCurrent.isEmpty, "VisualInstantPresentation.apply must be called outside transaction" )

    val vis = new Impl(transport, cursor)
    val map = tx.newInMemoryIDMap[Map[SpanLike, List[VisualProc[S]]]]
    val all = transport.iterator.toIndexedSeq

    //    def playStop(b: Boolean)(implicit tx: S#Tx): Unit =
    //      deferTx(vis.playing = b)

    def advance(time: Long, added: Vec[(SpanLike, BiGroup.TimedElem[S, Obj.T[S, ProcElem]])],
                removed: Vec[(SpanLike, BiGroup.TimedElem[S, Obj.T[S, ProcElem]])],
                params : Vec[(SpanLike, BiGroup.TimedElem[S, Obj.T[S, ProcElem]], Map[String, Param])])
               (implicit tx: S#Tx): Unit = {
      val vpRem = removed.flatMap {
        case (span, timed) =>
          map.get(timed.id).flatMap { vpm =>
            map.remove(timed.id)
            vpm.get(span).flatMap {
              case vp :: tail =>
                if (tail.nonEmpty) {
                  map.put(timed.id, vpm + (span -> tail))
                }
                Some(vp)
              case _ =>
                None
            }
          }
      }
      val hasRem  = vpRem.nonEmpty
      val vpAdd   = added.map {
        case (span, timed) =>
          val id    = timed.id
          val proc  = timed.value
          val n     = proc.attr.expr[String](ProcKeys.attrName).fold("<unnamed>")(_.value)
          // val n = proc.name.value
          //            val par  = proc.par.entriesAt( time )
          val par = Map.empty[String, Double]
          val vp = new VisualProc(n, par, cursor.position, tx.newHandle(proc))
          map.get(id) match {
            case Some(vpm) =>
              map.remove(id)
              map.put(id, vpm + (span -> (vp :: vpm.getOrElse(span, Nil))))
            case _ =>
              map.put(id, Map(span -> (vp :: Nil)))
          }
          vp
      }
      val hasAdd = vpAdd.nonEmpty

      val vpMod = params.flatMap {
        case (span, timed, ch) =>
          map.get(timed.id).flatMap(_.getOrElse(span, Nil).headOption).map(_ -> ch)
      }
      val hasMod = vpMod.nonEmpty

      if (hasAdd || hasRem || hasMod) deferTx {
        if (hasAdd) vis.add    (vpAdd: _*)
        if (hasRem) vis.remove (vpRem: _*)
        if (hasMod) vis.updated(vpMod: _*)
      }
    }

      deferTx( vis.guiInit() )
      advance( transport.time, all, Vec.empty, Vec.empty )   // after init!
      vis
   }

  //   private final class VisualProc( val name: String )

  private val ACTION_COLOR = "color"
  private val ACTION_LAYOUT = "layout"
  private val GROUP_GRAPH = "graph"
  private val GROUP_NODES = "graph.nodes"
  //   private val GROUP_EDGES    = "graph.edges"
  private val LAYOUT_TIME = 50

  //   private val colrPlay       = new Color( 0, 0x80, 0 )
  //   private val colrStop       = Color.black

  private final class Impl[S <: Sys[S]](transport: ProcTransport[S], cursor: Cursor[S])
    extends InstantGroupPanel[S] with ComponentHolder[Component] {

    private var playingVar = false
    //      private var vps      = Set.empty[ VisualProc ]
    private var nodeMap = Map.empty[VisualProc[S], data.Node]

    private val g = {
      val res = new data.Graph
      //         res.addColumn( VisualItem.LABEL, classOf[ String ])
      res.addColumn(VisualProc.COLUMN_DATA, classOf[VisualProc[S]])
      res
    }
    private var pVis: Visualization = _
    private var display: Display = _

    def guiInit(): Unit = {
      pVis = new Visualization()
      pVis.addGraph(GROUP_GRAPH, g)

      display = new Display(pVis) {
        override protected def setRenderingHints(g: Graphics2D): Unit = {
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING      , RenderingHints.VALUE_ANTIALIAS_ON         )
          g.setRenderingHint(RenderingHints.KEY_RENDERING         , RenderingHints.VALUE_RENDER_QUALITY       )
          // XXX somehow this has now effect:
          g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS , RenderingHints.VALUE_FRACTIONALMETRICS_ON )
          g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL    , RenderingHints.VALUE_STROKE_PURE          )
        }
      }
      display.setBackground(Color.black)

      val lay = new ForceDirectedLayout(GROUP_GRAPH)
      val fs = lay.getForceSimulator

      // a somewhat weird force that keeps unconnected vertices
      // within some bounds :)
      fs.addForce(new AbstractForce {
        private val x = 0f
        private val y = 0f
        private val r = 150f
        private val grav = 0.4f

        protected def getParameterNames: Array[String] = Array[String]()

        override def isItemForce = true

        override def getForce(item: ForceItem): Unit = {
          val n   = item.location
          val dx  = x - n(0)
          val dy  = y - n(1)
          val d   = math.sqrt(dx * dx + dy * dy).toFloat
          val dr  = r - d
          val v   = grav * item.mass / (dr * dr)
          if (d == 0.0) return
          item.force(0) += v * dx / d
          item.force(1) += v * dy / d

          //               println( "" + (dx/d) + "," + (dy/d) + "," + dr + "," + v )
        }
      })

      //         val lbRend = new LabelRenderer( VisualItem.LABEL )
      val lbRend = new NodeRenderer(VisualProc.COLUMN_DATA)
      val rf = new DefaultRendererFactory(lbRend)
      pVis.setRendererFactory(rf)

      // colors
      val actionNodeStroke  = new ColorAction(GROUP_NODES, VisualItem.STROKECOLOR, ColorLib.rgb(255, 255, 255))
      val actionNodeFill    = new ColorAction(GROUP_NODES, VisualItem.FILLCOLOR  , ColorLib.rgb(  0,   0,   0))
      val actionTextColor   = new ColorAction(GROUP_NODES, VisualItem.TEXTCOLOR  , ColorLib.rgb(255, 255, 255))

      // quick repaint
      val actionColor = new ActionList()
      actionColor.add(actionTextColor)
      actionColor.add(actionNodeStroke)
      actionColor.add(actionNodeFill)
      pVis.putAction(ACTION_COLOR, actionColor)

      val actionLayout = new ActionList(Activity.INFINITY, LAYOUT_TIME)
      actionLayout.add(lay)
      actionLayout.add(new RepaintAction())
      pVis.putAction(ACTION_LAYOUT, actionLayout)
      pVis.alwaysRunAfter(ACTION_COLOR, ACTION_LAYOUT)

      // ------------------------------------------------

      // initialize the display
      display.setSize(600, 600)
      val origin = new java.awt.Point(0, 0)
      display.zoom(origin, 2.0)
      display.panTo(origin)
      display.addControlListener(new ZoomControl())
      display.addControlListener(new WheelZoomControl())
      display.addControlListener(new ZoomToFitControl())
      display.addControlListener(new PanControl())
      //         display.addControlListener( new DragControl() )
      display.addControlListener(new VisualProcControl(cursor))
      display.setHighQuality(true)

      display.setForeground(Color.WHITE)
      display.setBackground(Color.BLACK)

      display.addAncestorListener(new AncestorListener {
        def ancestorAdded(e: AncestorEvent): Unit =
          startAnimation()

        def ancestorRemoved(e: AncestorEvent): Unit =
          stopAnimation()

        def ancestorMoved(e: AncestorEvent) = ()
      })

      component = Component.wrap(display)
    }

    def add(vps: VisualProc[S]*): Unit =
      visDo {
        vps.foreach(add1)
      }

    def remove(vps: VisualProc[S]*): Unit =
      visDo {
        vps.foreach(rem1)
      }

    def updated(pairs: (VisualProc[S], Map[String, Param])*): Unit =
      visDo {
        pairs.foreach {
          case (vp, map) =>
            vp.par ++= map
        }
      }

    private def add1(vp: VisualProc[S]): Unit = {
      val pNode = g.addNode()
      //         val vi      = pVis.getVisualItem( GROUP_GRAPH, pNode )
      //         vi.setString( VisualItem.LABEL, vp.name )
      //         pNode.setString( VisualItem.LABEL, vp.name )
      //         val vi = pVis.getVisualItem( GROUP_NODES, pNode )
      //         if( vi != null ) vi.set( COLUMN_DATA, vp )
      pNode.set(VisualProc.COLUMN_DATA, vp)
      nodeMap += vp -> pNode
    }

    private def rem1(vp: VisualProc[S]): Unit =
      nodeMap.get(vp) match {
        case Some(n) =>
          g.removeNode(n)
          nodeMap -= vp
        case _ =>
      }

    def playing: Boolean = playingVar

    def playing_=(b: Boolean): Unit =
      if (playingVar != b) {
        playingVar = b
        //            display.setBackground( if( b ) colrPlay else colrStop )
        //            display.repaint()
      }

    private def stopAnimation(): Unit = {
      pVis.cancel(ACTION_COLOR)
      pVis.cancel(ACTION_LAYOUT)
    }

    private def startAnimation(): Unit =
      pVis.run(ACTION_COLOR)

    private def visDo(thunk: => Unit): Unit =
      pVis.synchronized {
        stopAnimation()
        try {
          thunk
        } finally {
          startAnimation()
        }
      }
  }
}
