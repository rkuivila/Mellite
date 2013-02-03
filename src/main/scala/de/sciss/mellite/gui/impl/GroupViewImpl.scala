package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import swing.Component
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.expr.Expr
import de.sciss.swingtree.tree.{ExternalTreeModel, Tree}
import de.sciss.lucre.stm

object GroupViewImpl {
  def apply[S <: Sys[S]](group: Group[S])(implicit tx: S#Tx): GroupView[S] = {
    val view      = new Impl(group)
    val it: de.sciss.lucre.data.Iterator[S#Tx, Element[S]] = group.elements.iterator
    it.map {
      case Element.Int(ex) => ???
      case _ => ???
    } .toIndexedSeq
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private sealed trait ElementView[S <: Sys[S]] {
    def name: String
    def elem: stm.Source[S#Tx, Element[S]]
  }

  private final class StringExprView[S <: Sys[S]](var name: String,
                                                  val elem: stm.Source[S#Tx, Element[S] {type A = Expr.Var[S, String]}])
    extends ElementView[S] {

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

    def guiInit() {
      requireEDT()
      require( comp == null, "Initialization called twice" )
//       ggList = new swing.ListView {
//          peer.setModel( mList )
//          listenTo( selection )
//          reactions += {
//             case l: ListSelectionChanged[ _ ] => notifyViewObservers( l.range )
//          }
//       }

//      val m = ExternalTreeModel(roots)(expand)
//      val t = new Tree[ElementView]()

      comp = ??? // new ScrollPane( ggList )
    }
  }
}