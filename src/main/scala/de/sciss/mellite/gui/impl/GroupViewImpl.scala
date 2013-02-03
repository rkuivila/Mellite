package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import swing.{Swing, Orientation, BoxPanel, Label, ScrollPane, Component}
import collection.immutable.{IndexedSeq => IIdxSeq}
import scalaswingcontrib.tree.{ExternalTreeModel, Tree}
import Swing._

object GroupViewImpl {
  def apply[S <: Sys[S]](root: Elements[S])(implicit tx: S#Tx): GroupView[S] = {
    val view      = new Impl[S] // (group)
    val rootIter  = root.iterator
    val rootView  = new ElementView.Root(rootIter.map(elemView(_)(tx)).toIndexedSeq)
//    val elemViews = group.elements.iterator.map(elemView)

//println(s"root view = '$rootView', with children ${rootView.children}")

    guiFromTx {
      view.guiInit(rootView)
    }
    view
  }

  private def elemView[S <: Sys[S]](elem: Element[S])(implicit tx: S#Tx): ElementView[S] = {
    val name = elem.name.value
    elem match {
      case Element.Int(ex) =>
        val value = ex.value
        new ElementView.Int(name, value)
      case Element.Double(ex) =>
        val value = ex.value
        new ElementView.Double(name, value)
      case Element.String(ex) =>
        val value = ex.value
        new ElementView.String(name, value)
      case Element.Group(g) =>
        val children = g.iterator.map(elemView(_)(tx)).toIndexedSeq
        new ElementView.Group(name, children)
    }
  }

  private final val cmpBlank  = new Label
  private final val cmpLabel  = new Label
  private object cmpString extends BoxPanel( Orientation.Horizontal ) {
    val key = new Label {
      preferredSize = (100, 24)
    }
    val value = new Label {
      preferredSize = (100, 24)
    }
    contents += key
    contents += HStrut(8)
    contents += value
  }

  private object ElementView {
    import scala.{Int => _Int, Double => _Double}
    import java.lang.{String => _String}
//    import mellite.{Group => _Group}

    final class String[S <: Sys[S]](var name: Option[_String], var value: _String)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        cmpString.key.text    = name.getOrElse("<untitled>")
        cmpString.value.text  = value
        cmpString
      }
    }

    final class Int[S <: Sys[S]](var name: Option[_String], var value: _Int)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank // XXX TODO
    }

    final class Double[S <: Sys[S]](var name: Option[_String], var value: _Double)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank // XXX TODO
    }

    sealed trait GroupLike[S <: Sys[S]] extends ElementView[S] {
      def children: IIdxSeq[ElementView[S]]
    }

    final class Group[S <: Sys[S]](var name: Option[_String], var children: IIdxSeq[ElementView[S]])
      extends GroupLike[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        cmpLabel.text = name.getOrElse("<untitled>")
        cmpLabel
      }
    }

    final class Root[S <: Sys[S]](var children: IIdxSeq[ElementView[S]])
      extends GroupLike[S] {
      def name = None
      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank
    }
  }
  private sealed trait ElementView[S <: Sys[S]] {
    def name: Option[String]
//    def elem: stm.Source[S#Tx, Element[S]]
    def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component
  }

  private final class Renderer[S <: Sys[S]] extends Tree.Renderer[ElementView[S]] {
    def componentFor(owner: Tree[_], value: ElementView[S], cellInfo: Tree.Renderer.CellInfo): Component = {
      value.componentFor(owner, cellInfo)
    }
  }

  private final class Impl[S <: Sys[S]] extends GroupView[S] {
    view =>

    @volatile private var comp: Component = _

    def component: Component = {
      requireEDT()
      val res = comp
      if (res == null) sys.error("Called component before GUI was initialized")
      res
    }

    def guiInit(root: ElementView.Root[S]) {
      requireEDT()
      require(comp == null, "Initialization called twice")
//       ggList = new swing.ListView {
//          peer.setModel( mList )
//          listenTo( selection )
//          reactions += {
//             case l: ListSelectionChanged[ _ ] => notifyViewObservers( l.range )
//          }
//       }

      val m = ExternalTreeModel[ElementView[S]](root.children: _*) {
        case g: ElementView.GroupLike[S] => g.children
        case _ => Nil
      }
      val t = new Tree(m)
      t.showsRootHandles = true
      t.renderer = new Renderer[S]
//      t.expandAll()

      comp = new ScrollPane(t)
    }
  }
}