package de.sciss
package mellite
package gui

import scala.swing.{Reactions, Component}
import de.sciss.treetable.j
import javax.swing.{table => jtab}
import javax.swing.{tree => jtree}
import collection.breakOut
import language.implicitConversions
import scalaswingcontrib.CellView
import javax.swing.{event => jse}
import javax.swing.tree.TreePath
import de.sciss.treetable.j.event.TreeColumnModelListener
import java.awt

object TreeTable {
  private trait JTreeTableMixin { def tableWrapper: TreeTable[_, _] }

  val Path = collection.immutable.IndexedSeq
  type Path[+A] = collection.immutable.IndexedSeq[A]

  implicit private[gui] def pathToTreePath(p: Path[Any]): jtree.TreePath = {
    // reePath must be non null and not empty... SUCKERS
    if (p.isEmpty) null else {
      val array: Array[AnyRef] = p.map(_.asInstanceOf[AnyRef])(breakOut)
      new jtree.TreePath(array)
    }
  }

  implicit private[gui] def treePathToPath[A](tp: jtree.TreePath): Path[A] = {
    if (tp == null) Path.empty
    else tp.getPath.map(_.asInstanceOf[A])(breakOut) // .toIndexedSeq
  }
}
class TreeTable[A, Col <: TreeColumnModel[A]](treeModel0: TreeModel[A], treeColumnModel0: Col,
                                              tableColumnModel0: jtab.TableColumnModel)
  extends Component /* with Scrollable.Wrapper */ with CellView[A] {

  me =>

  import TreeTable.{Path, pathToTreePath, treePathToPath}

  def this(treeModel0: TreeModel[A], treeColumnModel0: Col) {
    this(treeModel0, treeColumnModel0, null)
  }

  private var _treeModel        = treeModel0
  private var _treeColumnModel  = treeColumnModel0
  private var _tableColumnModel = tableColumnModel0
  private var _renderer: TreeTableCellRenderer[A] = _

  def treeModel: TreeModel[A]                 = _treeModel
  def treeColumnModel: Col                    = _treeColumnModel
  def tableColumnModel: jtab.TableColumnModel = _tableColumnModel

  def renderer = _renderer
  def renderer_=(r: TreeTableCellRenderer[A]) {
    val rp = r match {
      case w: TreeTableCellRenderer.Wrapped[_] => w.peer
      case _ => new j.TreeTableCellRenderer {
        def getTreeTableCellRendererComponent(treeTable: j.TreeTable, value: Any, selected: Boolean, hasFocus: Boolean,
                                              row: Int, column: Int): awt.Component =
          getTreeTableCellRendererComponent(treeTable, value, selected = selected, hasFocus = hasFocus,
            row = row, column = column, expanded = false, leaf = false)

        def getTreeTableCellRendererComponent(treeTable: j.TreeTable, value: Any, selected: Boolean, hasFocus: Boolean,
                                              row: Int, column: Int, expanded: Boolean, leaf: Boolean): awt.Component = {
          val state = TreeTableCellRenderer.State(selected = selected, focused = hasFocus, expanded = expanded, leaf = leaf)
          r.getRendererComponent(me, value.asInstanceOf[A], row = row, column = column, state).peer
        }
      }
    }
    _renderer = r
    peer.setDefaultRenderer(classOf[AnyRef], rp)
  }

  def editable   = ??? : Boolean      // crap
  def cellValues = ??? : Iterator[A]  // crap

  private def wrapTreeModel(_peer: TreeModel[A]): jtree.TreeModel = new {
    val peer = _peer
  } with jtree.TreeModel {
    jmodel =>

    // val peer = _treeModel

    def getRoot: AnyRef = peer.root.asInstanceOf[AnyRef]
    def getChild(parent: Any, index: Int): AnyRef = peer.getChild(parent.asInstanceOf[A], index).asInstanceOf[AnyRef]
    def getChildCount(parent: Any): Int = peer.getChildCount(parent.asInstanceOf[A])
    def isLeaf(node: Any): Boolean = peer.isLeaf(node.asInstanceOf[A])

    def valueForPathChanged(path: TreePath, newValue: Any) {
      peer.valueForPathChanged(path, newValue.asInstanceOf[A])  // XXX TODO: is newValue really an `A`?
    }

    def getIndexOfChild(parent: Any, child: Any): Int =
      peer.getIndexOfChild(parent.asInstanceOf[A], child.asInstanceOf[A])

    private val sync = new AnyRef
    private var listeners = Vector.empty[jse.TreeModelListener]

    private val reaction: Reactions.Reaction = {
      case te: TreeNodesChanged[_] =>
        val evt = te.toJava(jmodel)
        listeners.foreach { l => l.treeNodesChanged(evt) }

      case te: TreeNodesInserted[_] =>
        val evt = te.toJava(jmodel)
        listeners.foreach { l => l.treeNodesInserted(evt) }

      case te: TreeNodesRemoved[_] =>
        val evt = te.toJava(jmodel)
        listeners.foreach { l => l.treeNodesRemoved(evt) }

      case te: TreeStructureChanged[_] =>
        val evt = te.toJava(jmodel)
        listeners.foreach { l => l.treeStructureChanged(evt) }
    }

    def addTreeModelListener(l: jse.TreeModelListener) {
      sync.synchronized {
        val start = listeners.isEmpty
        listeners :+= l
        if (start) peer.reactions += reaction
      }
    }

    def removeTreeModelListener(l: jse.TreeModelListener) {
      sync.synchronized {
        val idx = listeners.indexOf(l)
        if (idx >= 0) {
          listeners = listeners.patch(idx, Vector.empty, 1)
          if (listeners.isEmpty) peer.reactions -= reaction
        }
      }
    }
  }

  private def wrapTreeColumnModel(_peer: Col): j.TreeColumnModel = new {
    val peer = _peer
  } with j.TreeColumnModel {
    // val peer = _treeColumnModel

    def getHierarchicalColumn = peer.hierarchicalColumn
    def getColumnClass(column: Int): Class[_] = peer.getColumnClass(column)
    def isCellEditable(node: Any, column: Int): Boolean = peer.isCellEditable(node.asInstanceOf[A], column)
    def getColumnCount: Int = peer.columnCount
    def getColumnName(column: Int): String = peer.getColumnName(column)
    def getValueAt(node: Any, column: Int): AnyRef = peer.getValueAt(node.asInstanceOf[A], column).asInstanceOf[AnyRef]
    def setValueAt(value: Any, node: Any, column: Int) { peer.setValueAt(value, node.asInstanceOf[A], column) }

    private val sync      = new AnyRef
    private var listeners = Vector.empty[TreeColumnModelListener]

    private val reaction: Reactions.Reaction = {
      case TreeColumnChanged(_, path, column) =>
        val evt = new j.event.TreeColumnModelEvent(this, path, column)
        listeners.foreach { l =>
          l.treeColumnChanged(evt)
        }
    }

    def addTreeColumnModelListener(l: TreeColumnModelListener) {
      sync.synchronized {
        val start = listeners.isEmpty
        listeners :+= l
        if (start) peer.reactions += reaction
      }
    }

    def removeTreeColumnModelListener(l: TreeColumnModelListener) {
      sync.synchronized {
        val idx = listeners.indexOf(l)
        if (idx >= 0) {
          listeners = listeners.patch(idx, Vector.empty, 1)
          if (listeners.isEmpty) peer.reactions -= reaction
        }
      }
    }
  }

  override lazy val peer: j.TreeTable =
    new j.TreeTable(wrapTreeModel(treeModel0), wrapTreeColumnModel(treeColumnModel0), _tableColumnModel)
      with TreeTable.JTreeTableMixin with SuperMixin {

      def tableWrapper = TreeTable.this

      //    override def getCellRenderer(r: Int, c: Int) = new TableCellRenderer {
      //      def getTableCellRendererComponent(table: JTable, value: AnyRef, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) =
      //        Table.this.rendererComponent(isSelected, hasFocus, row, column).peer
      //    }
      //    override def getCellEditor(r: Int, c: Int) = editor(r, c)
      //    override def getValueAt(r: Int, c: Int) = Table.this.apply(r,c).asInstanceOf[AnyRef]
    }

  def showsRootHandles        : Boolean =  peer.getShowsRootHandles
  def showsRootHandles_=(value: Boolean) { peer.setShowsRootHandles(value) }

  def expandPath(path: Path[A]) { peer.expandPath(path) }

  def hierarchicalColumn: Int = peer.getHierarchicalColumn

  object selection extends CellSelection {
    object paths extends SelectionSet[Path[A]]({
      val p = peer.getSelectionPaths
      if (p == null) Seq.empty else p.map(treePathToPath)(breakOut)
    }) {
      def -=(p: Path[A]) = { peer.removeSelectionPath(p); this }
      def +=(p: Path[A]) = { peer.addSelectionPath(p); this }
      def --=(ps: Seq[Path[A]]) = { peer.removeSelectionPaths(ps.map(pathToTreePath).toArray); this }
      def ++=(ps: Seq[Path[A]]) = { peer.addSelectionPaths(ps.map(pathToTreePath).toArray); this }
      def leadSelection: Option[Path[A]] = Option(peer.getLeadSelectionPath)
    }

    def cellValues = ??? : Iterator[A]

    def isEmpty = size == 0
    def size = peer.getSelectionCount
  }
}