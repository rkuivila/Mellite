package de.sciss.mellite
package gui
package impl

import scalaswingcontrib.tree.{Tree, TreeModel}
import javax.swing.{tree => jst}
import javax.swing.{event => jse}
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import reflect.ClassTag
import Tree.Path
import collection.mutable

object TreeModelImpl {
  private case object hiddenRoot
}

/** A simple tree model like `ExternalTreeModel` but assuming that the caller takes care of updating the
  * external data.
  *
  * The caller must follow these rules
  * - for insertion, first insert value to external model, then call insertUnder
  * - for deletion, first call remove, then remove value from external model
  *
  * @param rootItems  function that provides the root level children
  * @param children   function that expands any internal branching node to its children
  */
final class TreeModelImpl[A](rootItems: => IIdxSeq[A], children: A => IIdxSeq[A]) extends TreeModel[A] {
  import TreeModelImpl._

  def roots: Seq[A] = rootItems

  object peer extends jst.TreeModel {
    private val treeModelListenerList = mutable.ListBuffer[jse.TreeModelListener]()

    def getChildrenOf(parent: Any) = parent match {
      case `hiddenRoot` => roots
      case a            => children(a.asInstanceOf[A])
    }

    def getChild(parent: Any, index: Int): AnyRef = {
      val ch = getChildrenOf(parent)
      if (index >= 0 && index < ch.size)
        ch(index).asInstanceOf[AnyRef]
      else
        sys.error("No child of \"" + parent + "\" found at index " + index)
    }
    def getChildCount(parent: Any): Int = getChildrenOf(parent).size
    def getIndexOfChild(parent: Any, child: Any): Int = getChildrenOf(parent) indexOf child
    def getRoot: AnyRef = hiddenRoot
    def isLeaf(node: Any): Boolean = getChildrenOf(node).isEmpty

//    private[tree] def copyListenersFrom(otherPeer: ExternalTreeModel[A]#ExternalTreeModelPeer) {
//      otherPeer.treeModelListeners foreach addTreeModelListener
//    }

    def treeModelListeners: Seq[jse.TreeModelListener] = treeModelListenerList

    def addTreeModelListener(tml: jse.TreeModelListener) {
      treeModelListenerList += tml
    }

    def removeTreeModelListener(tml: jse.TreeModelListener) {
      treeModelListenerList -= tml
    }

    def valueForPathChanged(path: jst.TreePath, newValue: Any) {
      update(treePathToPath(path), newValue.asInstanceOf[A])
    }

    private def createEvent(parentPath: jst.TreePath, newValue: Any) = {
      val index = getChildrenOf(parentPath.getPath.last) indexOf newValue
      createEventWithIndex(parentPath, newValue, index)
    }

    private def createEventWithIndex(parentPath: jst.TreePath, newValue: Any, index: Int) = {
      new jse.TreeModelEvent(this, parentPath, Array(index), Array(newValue.asInstanceOf[AnyRef]))
    }

    def fireTreeStructureChanged(parentPath: jst.TreePath, newValue: Any) {
      treeModelListenerList foreach { _ treeStructureChanged createEvent(parentPath, newValue) }
    }

    def fireNodesChanged(parentPath: jst.TreePath, newValue: Any) {
      treeModelListenerList foreach { _ treeNodesChanged createEvent(parentPath, newValue) }
    }

    def fireNodesInserted(parentPath: jst.TreePath, newValue: Any, index: Int) {
      def createEvent = createEventWithIndex(parentPath, newValue, index)
      treeModelListenerList foreach { _ treeNodesInserted createEvent }
    }

    def fireNodesRemoved(parentPath: jst.TreePath, removedValue: Any, index: Int) {
      def createEvent = createEventWithIndex(parentPath, removedValue, index)
      treeModelListenerList foreach { _ treeNodesRemoved createEvent }
    }
  }

  def getChildrenOf(parentPath: Path[A]): Seq[A] = if (parentPath.isEmpty) roots else children(parentPath.last)

  def filter(p: A => Boolean): TreeModel[A] = new TreeModelImpl(rootItems.filter(p), children.andThen(_.filter(p)))

  /** This method is currently not supported and will throw an exception. */
  def map[B](f: A => B) = throw new UnsupportedOperationException("toInternalModel")

  /** This method always returns `true`. */
  def isExternalModel = true
  /** This method is currently not supported and will throw an exception. */
  def toInternalModel = throw new UnsupportedOperationException("toInternalModel")

  def pathToTreePath(path: Path[A]): jst.TreePath = {
    val array = (hiddenRoot +: path).map(_.asInstanceOf[AnyRef]).toArray(ClassTag.Object)
    new jst.TreePath(array)
  }

  def treePathToPath(tp: jst.TreePath): Path[A] = {
    if (tp == null) null
    else {
      (tp.getPath.map(_.asInstanceOf[A])(breakOut): Path[A]).tail
    }
  }

  /** Replaces one value with another, mutating the underlying structure.
    * If a way to modify the external tree structure has not been provided with makeUpdatableWith(), then
    * an exception will be thrown.
    */
  def update(path: Path[A], newValue: A) {
    if (path.isEmpty) throw new IllegalArgumentException("Cannot update an empty path")

    val existing = path.last

    val replacingWithDifferentReference = existing.isInstanceOf[AnyRef] &&
                                         (existing.asInstanceOf[AnyRef] ne newValue.asInstanceOf[AnyRef])

    if (replacingWithDifferentReference) {
      // If the result is actually replacing the node with a different reference object, then
      // fire "tree structure changed".
      peer.fireTreeStructureChanged(pathToTreePath(path.init), newValue)
    } else {
      // If the result is a value type or is a modification of the same node reference, then
      // just fire "nodes changed".
      peer.fireNodesChanged        (pathToTreePath(path.init), newValue)
    }
  }

  def remove(pathToRemove: Path[A]): Boolean = {
    if (pathToRemove.isEmpty) return false

    val parentPath = pathToRemove.init
    val index = siblingsUnder(parentPath) indexOf pathToRemove.last
    if (index == -1) return false

    peer.fireNodesRemoved(pathToTreePath(parentPath), pathToRemove.last, index)
    true
  }

  def insertUnder(parentPath: Path[A], newValue: A, index: Int): Boolean = {
    val actualIndex = siblingsUnder(parentPath) indexOf newValue
    if (actualIndex == -1) return false

    peer.fireNodesInserted(pathToTreePath(parentPath), newValue, actualIndex)
    true
  }
}