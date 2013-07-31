package de.sciss
package mellite
package gui

import scala.reflect.ClassTag
import scala.swing.Publisher
import TreeTable.Path
import scala.annotation.tailrec
import collection.immutable.{IndexedSeq => Vec}

object TreeColumnModel {
  abstract class Column[A, T](val name: String)(implicit val ct: ClassTag[T]) {
    def apply(node: A): T
    def update(node: A, value: T): Unit
    def isEditable(node: A): Boolean
  }

  trait TupleLike[A] extends TreeColumnModel[A] {
    protected def columns: Vec[Column[A, _]]

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

    final def getColumnName (column: Int): String   = columns(column).name
    final def getColumnClass(column: Int): Class[_] = columns(column).ct.runtimeClass

   	final def columnCount: Int = columns.length

    def getValueAt(node: A, column: Int): Any = columns(column)(node)

    def setValueAt(value: Any, node: A, column: Int): Unit = {
      columns(column).asInstanceOf[Column[A, Any]](node) = value
      val path = pathToRoot(node)
      publish(TreeColumnChanged(this, path, column))
    }

   	def isCellEditable(node: A, column: Int): Boolean = columns(column).isEditable(node)

    def hierarchicalColumn = 0
  }

  abstract class Tuple2[A, T1, T2](val _1: Column[A, T1], val _2: Column[A, T2])
    extends TupleLike[A] {

    protected val columns = Vec(_1, _2)
  }

  abstract class Tuple3[A, T1, T2, T3](val _1: Column[A, T1], val _2: Column[A, T2], val _3: Column[A, T3])
    extends TupleLike[A] {

    protected val columns = Vec(_1, _2, _3)
  }
}
trait TreeColumnModel[A] extends Publisher {
  // def peer: j.TreeColumnModel
  
  def getColumnName (column: Int): String
  def getColumnClass(column: Int): Class[_]
 	
 	def columnCount: Int
 	
 	def getValueAt(node: A, column: Int): Any
 	def setValueAt(value: Any, node: A, column: Int): Unit
 	
 	def isCellEditable(node: A, column: Int): Boolean

  def hierarchicalColumn: Int
}