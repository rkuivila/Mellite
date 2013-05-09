package de.sciss
package mellite
package gui

import scala.swing.Publisher
import scala.swing.event.Event

//object TreeModel {
//  def wrap[A](peer: jtree.TreeModel): TreeModel[A] = {
//    val _peer = peer
//    new TreeModel[A] {
//      val peer = _peer
//    }
//  }
//}
trait TreeModel[A] extends Publisher {
  def root: A

  def getChildCount(parent: A): Int
  def getChild(parent: A, index: Int): A
  def isLeaf(node: A): Boolean

  // val peer: jtree.TreeModel

  def valueForPathChanged(path: TreeTable.Path[A], newValue: A): Unit

  def getIndexOfChild(parent: A, child: A): Int

  // final case class NodesChanged(parentPath: TreeTable.Path[A], children: (Int, A)*) extends Event
}