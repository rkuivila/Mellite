package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import swing.{Swing, Orientation, BoxPanel, Label, ScrollPane, Component}
import collection.immutable.{IndexedSeq => IIdxSeq}
import scalaswingcontrib.tree.{ExternalTreeModel, Tree}
import Swing._
import java.awt.{Dimension, Color}
import javax.swing.tree.DefaultTreeCellRenderer

object GroupViewImpl {
  def apply[S <: Sys[S]](root: Elements[S])(implicit tx: S#Tx): GroupView[S] = {
    val view      = new Impl[S] // (group)
    val rootIter  = root.iterator
    val rootView  = new ElementView.Root(rootIter.map(elemView(_)(tx)).toIndexedSeq)
    //println(s"rootView.children = ${rootView.children}")
    //    val elemViews = group.elements.iterator.map(elemView)

    //println(s"root view = '$rootView', with children ${rootView.children}")

    guiFromTx {
      view.guiInit(rootView)
    }

    def elemAdded(path: Tree.Path[ElementView.GroupLike[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
      println(s"elemAdded = $path $idx $elem")
      val v = elemView(elem)
      guiFromTx {
        // parentView.children = paren  tView.children.patch(idx, IIdxSeq(elemView), 0)
        view.model.insertUnder(path, v, idx)
      }
    }

    root.changed.reactTx[Elements.Update[S]] {
      implicit tx => upd =>
        println(s"List update. toSeq = ${upd.list.iterator.toIndexedSeq}")
        upd.changes.foreach {
          case Elements.Added  (idx, elem)      => elemAdded(Tree.Path.empty, idx, elem)
          case Elements.Removed(idx, elem)      => ???
          case Elements.Element(elem, elemUpd)  => ???
          //        case _ =>
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
  private final val cmpGroupJ = new DefaultTreeCellRenderer
  private final val cmpGroup  = Component.wrap(cmpGroupJ)

  private object cmpString extends BoxPanel(Orientation.Horizontal) {
//    val sz: Dimension = (100, 24)
    val key = new DefaultTreeCellRenderer
//    {
//      override def getPreferredSize: Dimension = {
//        val d = super.getPreferredSize
//        if (d != null) d.width = 100
//println(s"Aqui $d")
//        d
//      }
//    }
    key.setLeafIcon(null)
    val value = new DefaultTreeCellRenderer
//    {
//      override def getPreferredSize = sz
//    }
//    value.setPreferredSize(sz)
//    value.setMinimumSize  (sz)
    value.setLeafIcon(null)
    background = null
    contents += Component.wrap(key)
    contents += HStrut(8)
    contents += Component.wrap(value)
  }

  private object ElementView {
    import scala.{Int => _Int, Double => _Double}
    import java.lang.{String => _String}
    //    import mellite.{Group => _Group}

    private val colrSelection = new Color(0xD0, 0xD0, 0xFF, 0xFF)

    final class String[S <: Sys[S]](var name: _String, var value: _String)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        cmpString.key.getTreeCellRendererComponent(tree.peer, name, info.isSelected, false, true, info.row, info.hasFocus)
//        cmpString.key.text    = name
        cmpString.value.getTreeCellRendererComponent(tree.peer, value, info.isSelected, false, true, info.row, info.hasFocus)
//        if (info.isSelected) {
//          cmpString.background = colrSelection
//        } else {
//          cmpString.background = null
//        }
        cmpString
      }
      def prefix = "String"
    }

    final class Int[S <: Sys[S]](var name: _String, var value: _Int)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank // XXX TODO
      def prefix = "Int"
    }

    final class Double[S <: Sys[S]](var name: _String, var value: _Double)
      extends ElementView[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank // XXX TODO
      def prefix = "Double"
    }

    sealed trait GroupLike[S <: Sys[S]] extends ElementView[S] {
      var children: IIdxSeq[ElementView[S]]
    }

    final class Group[S <: Sys[S]](var name: _String, var children: IIdxSeq[ElementView[S]])
      extends GroupLike[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        // never show the leaf icon, always a folder icon. for empty folders, show the icon as if the folder is open
        cmpGroupJ.getTreeCellRendererComponent(tree.peer, name, info.isSelected, info.isExpanded || info.isLeaf,
          false /* info.isLeaf */, info.row, info.hasFocus)
        cmpGroup
      }

      def prefix = "Group"
    }

    final class Root[S <: Sys[S]](var children: IIdxSeq[ElementView[S]])
      extends GroupLike[S] {
      def name = "<root>"
      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank
      def prefix = "Root"
    }
  }
  private sealed trait ElementView[S <: Sys[S]] {
    def prefix: String
    def name: String
//    def elem: stm.Source[S#Tx, Element[S]]
    def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component
    override def toString = s"ElementView.$prefix(name = $name)"
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
            g.children = g.children.patch(idx, IIdxSeq(elem), 0)
            //            println(s"Expanding ${g} at ${idx} with ${elem} - now children are ${g.children}")
            true
          case _ => false
        }
      }

      val t = new Tree(_model)
      //      t.listenTo(t)
      //      t.reactions += {
      //        case r => println(s"TREE OBSERVATION : ${r}")
      //      }
      t.showsRootHandles = true
      t.renderer = new Renderer[S]
      t.expandAll()

      comp = new ScrollPane(t)
    }
  }
}