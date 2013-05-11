package de.sciss
package mellite
package gui
package impl

import scala.swing.{ScrollPane, FlowPanel, Button, BorderPanel}
import synth.proc
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.lucre.{confluent, stm}
import java.util.Date

object DocumentCursorsFrameImpl {
  type S = proc.Confluent
  type D = S#D

  def apply(document: ConfluentDocument)(implicit tx: D#Tx): DocumentCursorsFrame = {
    def createView(parent: Option[CursorView], elem: Cursors[S, D])(implicit tx: D#Tx): CursorView = {
      import document._
      val name    = elem.name.value
      val created = confluent.Sys.Acc.info(elem.seminal        ).timeStamp
      val updated = confluent.Sys.Acc.info(elem.cursor.position).timeStamp
      new CursorView(elem = elem, parent = parent, children = Vector.empty,
        name = name, created = created, updated = updated)
    }

    val rootView  = createView(parent = None, elem = document.cursors)
    val view = new Impl(document, rootView)(tx.system)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class CursorView(val elem: Cursors[S, D], val parent: Option[CursorView],
                                 var children: IIdxSeq[CursorView], var name: String,
                                 val created: Long, var updated: Long)

  private final class Impl(val document: ConfluentDocument, _root: CursorView)(implicit cursor: stm.Cursor[D])
    extends DocumentCursorsFrame with ComponentHolder[desktop.Window] {

    type Node = CursorView

    private class ElementTreeModel extends AbstractTreeModel[Node] {
      lazy val root: Node = _root // ! must be lazy. suckers....

      def getChildCount(parent: Node): Int = parent.children.size
      def getChild(parent: Node, index: Int): Node = parent.children(index)
      def isLeaf(node: Node): Boolean = false
      def getIndexOfChild(parent: Node, child: Node): Int = parent.children.indexOf(child)
      def getParent(node: Node): Option[Node] = node.parent

      def valueForPathChanged(path: TreeTable.Path[Node], newValue: Node) {
        println(s"valueForPathChanged($path, $newValue)")
      }

      def elemAdded(parent: Node, idx: Int, view: Node) {
        // if (DEBUG) println(s"model.elemAdded($parent, $idx, $view)")
        require(idx >= 0 && idx <= parent.children.size)
        parent.children = parent.children.patch(idx, Vector(view), 0)
        fireNodesInserted(view)
      }

      def elemRemoved(parent: Node, idx: Int) {
        // if (DEBUG) println(s"model.elemRemoved($parent, $idx)")
        require(idx >= 0 && idx < parent.children.size)
        val v = parent.children(idx)
        // this is frickin insane. the tree UI still accesses the model based on the previous assumption
        // about the number of children, it seems. therefore, we must not update children before
        // returning from fireNodesRemoved.
        fireNodesRemoved(v)
        parent.children  = parent.children.patch(idx, Vector.empty, 1)
      }

      def elemUpdated(view: Node) {
        // if (DEBUG) println(s"model.elemUpdated($view)")
        fireNodesChanged(view)
      }
    }

    private var _model: ElementTreeModel  = _
    private var t: TreeTable[Node, TreeColumnModel[Node]] = _

    def guiInit() {
      requireEDT()
      require(comp == null, "Initialization called twice")

      _model = new ElementTreeModel

      val colName = new TreeColumnModel.Column[Node, String]("Name") {
        def apply(node: Node): String = node.name

        def update(node: Node, value: String) {
          if (value != node.name) {
            cursor.step { implicit tx =>
              val expr = ExprImplicits[D]
              import expr._
              node.elem.name_=(value)
            }
          }
        }

        def isEditable(node: Node) = true
      }

      val colCreated = new TreeColumnModel.Column[Node, Date]("Created") {
        def apply(node: Node): Date = new Date(node.created)
        def update(node: Node, value: Date) {}
        def isEditable(node: Node) = false
      }

      val colUpdated = new TreeColumnModel.Column[Node, Date]("Updated") {
        def apply(node: Node): Date = new Date(node.updated)
        def update(node: Node, value: Date) {}
        def isEditable(node: Node) = false
      }

      val tcm = new TreeColumnModel.Tuple3[Node, String, Date, Date](colName, colCreated, colUpdated) {
        def getParent(node: Node): Option[Node] = node.parent
      }

      t = new TreeTable(_model, tcm)
      t.showsRootHandles    = true
      t.autoCreateRowSorter = true

      val ggAdd = Button("+") {
        println("Add")
      }
      ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      val ggDelete: Button = Button("\u2212") {
        println("Delete")
      }
      ggDelete.enabled = false
      ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val ggView: Button = Button("View") {
        println("View")
      }
      ggView.enabled = false
      ggView.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val folderButPanel = new FlowPanel(ggAdd, ggDelete, ggView)

      val scroll    = new ScrollPane(t)
      scroll.border = null

      comp = new desktop.impl.WindowImpl {
        def style       = desktop.Window.Regular
        def handler     = Mellite.windowHandler

        title           = document.folder.nameWithoutExtension
        file            = Some(document.folder)
        closeOperation  = desktop.Window.CloseIgnore
        contents        = new BorderPanel {
          add(scroll,         BorderPanel.Position.Center)
          add(folderButPanel, BorderPanel.Position.South )
        }

        pack()
        // centerOnScreen()
        front()
        // add(folderPanel, BorderPanel.Position.Center)
      }
    }
  }
}