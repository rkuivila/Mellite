package de.sciss.mellite
package gui
package impl

import prefuse.controls.ControlAdapter
import javax.swing.SwingUtilities
import java.awt.event.MouseEvent
import java.awt.geom.Point2D
import prefuse.Display
import prefuse.visual.{EdgeItem, NodeItem, AggregateItem, VisualItem}

object VisualProcControl {
//   private val csrHand     = Cursor.getPredefinedCursor( Cursor.HAND_CURSOR )
//   private val csrDefault  = Cursor.getDefaultCursor
}
class VisualProcControl /* ( vis: Visualization ) */ extends ControlAdapter {

   private var hoverItem: Option[ VisualItem ]  = None
   private var drag: Option[ Drag ]             = None

   private class Drag( /* val vi: VisualItem,*/ val lastPt: Point2D ) {
      var started = false
   }


   def setSmartFixed( vi: VisualItem, state: Boolean ) {
      if( state == true ) {
         vi.setFixed( true )
         return
      }
//      getData( vi ) match {
//         case Some( vProc ) if( vProc.proc.anatomy == ProcDiff ) => {
//            vi.setFixed( true )
//         }
//         case _ => {
            vi.setFixed( false )
//         }
//      }
   }

   private def getData( vi: VisualItem ) : Option[ VisualProc ] = {
      if( vi.canGet(     VisualProc.COLUMN_DATA, classOf[       VisualProc ])) {
         Option( vi.get( VisualProc.COLUMN_DATA ).asInstanceOf[ VisualProc ])
      } else None
   }

   override def itemEntered( vi: VisualItem, e: MouseEvent ) {
//      e.getComponent.setCursor( csrHand )
      hoverItem = Some( vi )
      vi match {
         case ni: NodeItem => {
            setFixed( ni, fixed = true )
         }
         case ei: EdgeItem => {
            setFixed( ei.getSourceItem, fixed = true )
            setFixed( ei.getTargetItem, fixed = true )
         }
         case _ =>
      }
   }

   private def getDisplay( e: MouseEvent ) = e.getComponent.asInstanceOf[ Display ]

   override def itemExited( vi: VisualItem, e: MouseEvent ) {
      hoverItem = None
      vi match {
         case ni: NodeItem => {
            setFixed( ni, fixed = false )
         }
         case ei: EdgeItem => {
            setFixed( ei.getSourceItem, fixed = false )
            setFixed( ei.getTargetItem, fixed = false )
         }
         case _ =>
      }
//      e.getComponent.setCursor( csrDefault )
   }

   override def itemPressed( vi: VisualItem, e: MouseEvent ) {
      val d          = getDisplay( e )
      val displayPt  = d.getAbsoluteCoordinate( e.getPoint, null )
      getData( vi ).foreach { vp =>
         if( e.getClickCount == 2 ) println( "Jo chuck" )
      }
      if( !SwingUtilities.isLeftMouseButton( e ) || e.isShiftDown ) return
//      if( e.isAltDown() ) {
//         vi match {
//
//         }
//      }
      val dr   = new Drag( displayPt )
      drag     = Some( dr )
      if( vi.isInstanceOf[ AggregateItem ]) setFixed( vi, fixed = true )
   }

   override def itemReleased( vi: VisualItem, e: MouseEvent ) {
//      vis.getRenderer( vi ) match {
//         case pr: NuagesProcRenderer => {
//            val data = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
//            if( data != null ) {
//               val d          = getDisplay( e )
//               val displayPt  = d.getAbsoluteCoordinate( e.getPoint, null )
//               data.update( pr.getShape( vi ))
//               data.itemReleased( vi, e, displayPt )
//            }
//         }
//         case _ =>
//      }
      drag.foreach( dr => {
         setFixed( vi, fixed = false )
         drag = None
      })
   }

   override def itemDragged( vi: VisualItem, e: MouseEvent ) {
      val d          = getDisplay( e )
      val newPt      = d.getAbsoluteCoordinate( e.getPoint, null )
//      vis.getRenderer( vi ) match {
//         case pr: NuagesProcRenderer => {
//            val data       = vi.get( COL_NUAGES ).asInstanceOf[ VisualData ]
//            if( data != null ) {
//               data.update( pr.getShape( vi ))
//               data.itemDragged( vi, e, newPt )
//            }
//         }
//         case _ =>
//      }
      drag.foreach( dr => {
         if( !dr.started ) dr.started = true
         val dx = newPt.getX - dr.lastPt.getX
         val dy = newPt.getY - dr.lastPt.getY
         move( vi, dx, dy )
         dr.lastPt.setLocation( newPt )
      })
   }

   // recursive over aggregate items
   private def setFixed( vi: VisualItem, fixed: Boolean ) {
      vi match {
         case ai: AggregateItem => {
            val iter = ai.items()
            while( iter.hasNext ) {
               val vi2 = iter.next.asInstanceOf[ VisualItem ]
               setFixed( vi2, fixed )
            }
         }
         case _ => setSmartFixed( vi, fixed )
      }
   }

   // recursive over aggregate items
   private def move( vi: VisualItem, dx: Double, dy: Double ) {
      vi match {
         case ai: AggregateItem => {
            val iter = ai.items()
            while( iter.hasNext ) {
               val vi2 = iter.next.asInstanceOf[ VisualItem ]
               move( vi2, dx, dy )
            }
         }
         case _ => {
            val x = vi.getX
            val y = vi.getY
            vi.setStartX( x )
            vi.setStartY( y )
            vi.setX( x + dx )
            vi.setY( y + dy )
            vi.setEndX( x + dx )
            vi.setEndY( y + dy )
         }
      }
   }
}