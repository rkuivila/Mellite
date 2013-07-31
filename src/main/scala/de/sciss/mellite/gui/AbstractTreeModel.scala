package de.sciss.mellite.gui

import scala.annotation.tailrec
import collection.immutable.{IndexedSeq => Vec}

trait AbstractTreeModel[A] extends TreeModel[A] {
  import TreeTable.Path

  def getParent(node: A): Option[A]

  private def pathToRoot(node: A): TreeTable.Path[A] = {
    @tailrec def loop(partial: Path[A], n: A): Path[A] = {
      val res = n +: partial
      getParent(n) match {
        case Some(parent) => loop(res, parent)
        case _ => res
      }
    }
    loop(Path.empty, node)
  }

  final protected def fireNodesChanged    (nodes: A*): Unit = fire(nodes)(TreeNodesChanged    [A])
  final protected def fireNodesInserted   (nodes: A*): Unit = fire(nodes)(TreeNodesInserted   [A])
  final protected def fireNodesRemoved    (nodes: A*): Unit = fire(nodes)(TreeNodesRemoved    [A])
  final protected def fireStructureChanged(nodes: A*): Unit = fire(nodes)(TreeStructureChanged[A])

  private def fire(nodes: Seq[A])(fun: (TreeModel[A], Path[A], Seq[(Int, A)]) => TreeModelEvent[A]): Unit = {
    var pred  = Map.empty[A, Path[A]]
    var paths = Map.empty[Path[A], Vec[(Int, A)]] withDefaultValue Vector.empty
    nodes.foreach { n =>
      val (path, idx) = getParent(n) match {
        case Some(parent) =>
          val path = pred.getOrElse(parent, {
            val res = pathToRoot(parent)
            pred += parent -> res
            res
          })
          path -> getIndexOfChild(parent, n)
        case _ =>
          Path.empty -> 0
      }
      paths += path -> (paths(path) :+ (idx, n))
    }

    paths.foreach { case (parentPath, indexed) =>
      publish(fun(this, parentPath, indexed))
    }
  }
}