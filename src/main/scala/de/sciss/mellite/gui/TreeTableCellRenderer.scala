package de.sciss
package mellite
package gui

import treetable.j
import scala.swing.{Label, Component}
import java.awt

object TreeTableCellRenderer {
  final case class TreeState(expanded: Boolean, leaf: Boolean)
  final case class State(selected: Boolean, focused: Boolean, tree: Option[TreeState])

  object Default extends Label with Wrapped {
    override lazy val peer: j.DefaultTreeTableCellRenderer = new j.DefaultTreeTableCellRenderer

    def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int, state: State): Component = {
      state.tree match {
        case Some(TreeState(expanded, leaf)) =>
          peer.getTreeTableCellRendererComponent(treeTable.peer, value, state.selected, state.focused, row, column,
            expanded, leaf)
        case _ =>
          peer.getTreeTableCellRendererComponent(treeTable.peer, value, state.selected, state.focused, row, column)
      }
      this
    }
  }

  trait Wrapped extends TreeTableCellRenderer {
    def peer: j.TreeTableCellRenderer
  }
}
trait TreeTableCellRenderer {
  import TreeTableCellRenderer._

  def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int, state: State): Component
}