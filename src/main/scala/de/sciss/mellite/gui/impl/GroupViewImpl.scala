package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import swing.Component
import collection.immutable.{IndexedSeq => IIdxSeq}

object GroupViewImpl {
  def apply[S <: Sys[S]](group: Group[S])(implicit tx: S#Tx): GroupView[S] = {
    val view      = new Impl(group)
    val elemNames = group.elements.iterator.toIndexedSeq.map(_.name.value.getOrElse("<unnamed>"))
    guiFromTx {
       view.guiInit(elemNames)
    }
    view
  }

  private final class Impl[S <: Sys[S]](group: Group[S]) extends GroupView[S] {
    view =>

    @volatile private var comp: Component = _

    def component: Component = {
       requireEDT()
       val res = comp
       if (res == null) sys.error("Called component before GUI was initialized")
       res
    }

    def guiInit(initElemNames: IIdxSeq[String]) {
       requireEDT()
       require( comp == null, "Initialization called twice" )
//       ggList = new swing.ListView {
//          peer.setModel( mList )
//          listenTo( selection )
//          reactions += {
//             case l: ListSelectionChanged[ _ ] => notifyViewObservers( l.range )
//          }
//       }

       comp = ??? // new ScrollPane( ggList )
    }
  }
}