package de.sciss
package mellite
package gui
package impl

import scala.swing._
import synth.proc
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.lucre.{confluent, stm}
import java.util.{Locale, Date}
import de.sciss.mellite.gui.TreeTableSelectionChanged
import scala.Some
import java.text.SimpleDateFormat
import de.sciss.lucre.event.Change

object DocumentCursorsFrameImpl {
  type S = proc.Confluent
  type D = S#D

  def apply(document: ConfluentDocument)(implicit tx: D#Tx): DocumentCursorsFrame = {
    val root      = document.cursors
    val rootView  = createView(document, parent = None, elem = root)
    val view      = new Impl(document, rootView)(tx.system)

    val obs = root.changed.react { implicit tx => upd =>
      view.elemUpdated(rootView, upd.changes)
    }
    // XXX TODO: remember obs for disposal

    guiFromTx {
      view.guiInit()
    }
    view
  }

  private def createView(document: ConfluentDocument, parent: Option[CursorView], elem: Cursors[S, D])
                        (implicit tx: D#Tx): CursorView = {
    import document._
    val name    = elem.name.value
    val created = confluent.Sys.Acc.info(elem.seminal        ).timeStamp
    val updated = confluent.Sys.Acc.info(elem.cursor.position).timeStamp
    new CursorView(elem = elem, parent = parent, children = Vector.empty,
      name = name, created = created, updated = updated)
  }

  private final class CursorView(val elem: Cursors[S, D], val parent: Option[CursorView],
                                 var children: IIdxSeq[CursorView], var name: String,
                                 val created: Long, var updated: Long)

  private final class Impl(val document: ConfluentDocument, _root: CursorView)(implicit cursor: stm.Cursor[D])
    extends DocumentCursorsFrame with ComponentHolder[desktop.Window] {

    type Node = CursorView

    private var mapViews = Map.empty[Cursors[S, D], Node]

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

    private def actionAdd(parent: Node) {
      val format  = new SimpleDateFormat("yyyy MM dd MM | HH:mm:ss", Locale.US) // don't bother user with alpha characters
      val ggValue = new FormattedTextField(format)
      ggValue.peer.setValue(new Date(parent.updated))
      val nameOpt = GUIUtil.keyValueDialog(value = ggValue, title = "Add New Cursor",
        defaultName = "branch", window = Some(comp))
      (nameOpt, ggValue.peer.getValue) match {
        case (Some(name), seminalDate: Date) =>
          val parentElem = parent.elem
          parentElem.cursor.step { implicit tx =>
            implicit val dtx = proc.Confluent.durable(tx)
            val seminal = tx.inputAccess.takeUntil(seminalDate.getTime)
            // lucre.event.showLog = true
            parentElem.addChild(seminal)
            // lucre.event.showLog = false
          }
        case _ =>
      }
    }

    private def elemRemoved(parent: Node, child: Cursors[S, D])(implicit tx: D#Tx) {
      mapViews.get(child).foreach { cv =>
        val idx = parent.children.indexOf(cv)
        cv.children.reverse.foreach { cc =>
          elemRemoved(cv, cc.elem)
        }
        mapViews -= child
        guiFromTx {
          _model.elemRemoved(parent, idx)
        }
      }
    }

    private def elemAdded(parent: Node, child: Cursors[S, D])(implicit tx: D#Tx) {
      val cv  = createView(document, parent = Some(parent), elem = child)
      val idx = parent.children.size
      mapViews += child -> cv
      guiFromTx {
        _model.elemAdded(parent, idx, cv)
      }
    }

    def elemUpdated(v: Node, upd: IIdxSeq[Cursors.Change[S, D]])(implicit tx: D#Tx) {
      upd.foreach {
        case Cursors.ChildAdded  (child) => elemAdded  (v, child)
        case Cursors.ChildRemoved(child) => elemRemoved(v, child)
        case Cursors.Renamed(Change(_, newName))  => guiFromTx {
          v.name = newName
          _model.elemUpdated(v)
        }
        case Cursors.ChildUpdate(Cursors.Update(source, childUpd)) => // recursion
          mapViews.get(source).foreach { cv =>
            elemUpdated(cv, childUpd)
          }
      }
    }

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
      t.autoCreateRowSorter = true  // XXX TODO: hmmm, not sufficient for sorters. what to do?
      t.renderer = new TreeTableCellRenderer {
        private val dateFormat = new SimpleDateFormat("E d MMM yy | HH:mm:ss", Locale.US)

        private val component = TreeTableCellRenderer.Default
        def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int,
                                 state: TreeTableCellRenderer.State): Component = {
          val value1 = value match {
            case d: Date  => dateFormat.format(d)
            case _        => value
          }
          val res = component.getRendererComponent(treeTable, value1, row = row, column = column, state = state)
          res // component
        }
      }
      val tabCM = t.peer.getColumnModel
      tabCM.getColumn(0).setPreferredWidth(128)
      tabCM.getColumn(1).setPreferredWidth(184)
      tabCM.getColumn(2).setPreferredWidth(184)

      val ggAdd = Button("+") {
        t.selection.paths.headOption.foreach { path =>
          val v = path.last
          actionAdd(parent = v)
        }
      }
      ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")
      ggAdd.enabled = false

      val ggDelete: Button = Button("\u2212") {
        println("Delete")
      }
      ggDelete.enabled = false
      ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val ggView: Button = Button("View") {
        t.selection.paths.foreach { path =>
          val elem = path.last.elem
          implicit val cursor = elem.cursor
          cursor.step { implicit tx =>
            DocumentElementsFrame(document)
          }
        }
      }
      ggView.enabled = false
      ggView.peer.putClientProperty("JButton.buttonType", "roundRect")

      t.listenTo(t.selection)
      t.reactions += {
        case e: TreeTableSelectionChanged[_, _] =>  // this crappy untyped event doesn't help us at all
          val selSize = t.selection.paths.size
          ggAdd   .enabled  = selSize == 1
          // ggDelete.enabled  = selSize > 0
          ggView  .enabled  = selSize == 1 // > 0
      }

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