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

    def elemAdded(path: Tree.Path[ElementView.GroupLike[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
      val v = elemView(elem)
      guiFromTx {
        // parentView.children = parentView.children.patch(idx, IIdxSeq(elemView), 0)
        view.model.insertUnder(path, v, idx)
      }
    }

    root.changed.reactTx[Elements.Update[S]] { implicit tx => upd =>
      upd.changes.foreach {
        case Elements.Added(  idx, elem) => elemAdded(Tree.Path(rootView), idx, elem)
        case Elements.Removed(idx, elem) =>

//        case Elements.Element(elem, elemUpd) =>
        case _ =>
      }
//      case Element.Update(elem, changes) =>
//      case Elements.Update(root0, changes) =>
//
//      case _ =>
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

    final class String[S <: Sys[S]](var name: _String, var value: _String)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        cmpString.key.text    = name
        cmpString.value.text  = value
        cmpString
      }
    }

    final class Int[S <: Sys[S]](var name: _String, var value: _Int)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank // XXX TODO
    }

    final class Double[S <: Sys[S]](var name: _String, var value: _Double)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank // XXX TODO
    }

    sealed trait GroupLike[S <: Sys[S]] extends ElementView[S] {
      var children: IIdxSeq[ElementView[S]]
    }

    final class Group[S <: Sys[S]](var name: _String, var children: IIdxSeq[ElementView[S]])
      extends GroupLike[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        cmpLabel.text = name
        cmpLabel
      }
    }

    final class Root[S <: Sys[S]](var children: IIdxSeq[ElementView[S]])
      extends GroupLike[S] {
      def name = "<root>"
      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank
    }
  }
  private sealed trait ElementView[S <: Sys[S]] {
    def name: String
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
    private var _model: ExternalTreeModel[ElementView[S]] = _
    def model = _model

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

      _model = ExternalTreeModel[ElementView[S]](root.children: _*) {
        case g: ElementView.GroupLike[S] => g.children
        case _ => Nil
      } makeInsertableWith { (path, elem, idx) =>
        path.lastOption.getOrElse(root) match {
          case g: ElementView.GroupLike[S] if g.children.size >= idx =>
            println(s"Expanding ${g} at ${idx} with ${elem}")
            g.children = g.children.patch(idx, IIdxSeq(elem), 0)
            true
          case _ => false
        }
      }

      val t = new Tree(_model)
      t.showsRootHandles = true
      t.renderer = new Renderer[S]
      t.expandAll()

      comp = new ScrollPane(t)
    }
  }
}