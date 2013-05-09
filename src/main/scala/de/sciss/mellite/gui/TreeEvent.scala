package de.sciss.mellite.gui

import javax.swing.{event => jse}
import scala.swing.event.Event
import collection.breakOut

sealed trait TreeEvent[A] extends Event {
  def model: TreeModel[A]
  def parentPath: TreeTable.Path[A]
  def children: Seq[(Int, A)]

  final private[gui] def toJava(source: Any): jse.TreeModelEvent = {
    import TreeTable.pathToTreePath
    val (idxSeq, nodesSeq) = children.unzip
    val indices = idxSeq.toArray
    val nodes: Array[AnyRef] = nodesSeq.map(_.asInstanceOf[AnyRef])(breakOut)
    new jse.TreeModelEvent(source, parentPath, indices, nodes)
  }
}

final case class TreeNodesChanged[A](model: TreeModel[A], parentPath: TreeTable.Path[A], children: (Int, A)*)
  extends TreeEvent[A]

final case class TreeNodesInserted[A](model: TreeModel[A], parentPath: TreeTable.Path[A], children: (Int, A)*)
  extends TreeEvent[A]

final case class TreeNodesRemoved[A](model: TreeModel[A], parentPath: TreeTable.Path[A], children: (Int, A)*)
  extends TreeEvent[A]

final case class TreeStructureChanged[A](model: TreeModel[A], parentPath: TreeTable.Path[A], children: (Int, A)*)
  extends TreeEvent[A]
