/*
 *  FreesoundRetrievalObjView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor
import java.net.URI
import java.util.Date
import javax.swing.Icon

import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{Desktop, FileDialog, OptionPane, PathField, Preferences, UndoManager}
import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.freesound.impl.FreesoundImpl
import de.sciss.freesound.lucre.{PreviewsCache, Retrieval, RetrievalView, TextSearchObj}
import de.sciss.freesound.swing.SoundTableView
import de.sciss.freesound.{Auth, Client, Freesound, Sound, TextSearch}
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, TxnLike}
import de.sciss.lucre.swing._
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ListObjViewImpl.NonEditable
import de.sciss.mellite.gui.impl.document.FolderFrameImpl
import de.sciss.processor.Processor
import de.sciss.swingplus.GroupPanel
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Workspace
import de.sciss.{desktop, freesound}

import scala.collection.breakOut
import scala.concurrent.Future
import scala.concurrent.stm.Ref
import scala.swing.{Action, Alignment, Button, Component, Label, SequentialContainer, TabbedPane, TextField}
import scala.util.control.NonFatal
import scala.util.{Success, Try}

object FreesoundRetrievalObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = Retrieval[~]
  val icon: Icon        = ObjViewImpl.raphaelIcon(freesound.swing.Shapes.Retrieval)
  val prefix            = "Retrieval"
  def humanName: String = s"Freesound $prefix"
  def tpe               = Retrieval
  def category: String  = ObjView.categComposition
  def hasMakeDialog     = true

  private[this] final val ClientId  = "WxJZb6eY0rqYVYqzkkfP"

  private[this] lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def mkListView[S <: Sys[S]](obj: Retrieval[S])(implicit tx: S#Tx): FreesoundRetrievalObjView[S] with ListObjView[S] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  type Config[S <: stm.Sys[S]] = ObjViewImpl.PrimitiveConfig[File]

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (ok: Config[S] => Unit)
                                 (implicit cursor: stm.Cursor[S]): Unit = {
    val ggValue   = new PathField
    ggValue.mode  = FileDialog.Folder
    ggValue.title = "Select Download Folder"
    val res = ObjViewImpl.primitiveConfig[S, File](window, tpe = humanName, ggValue = ggValue,
      prepare = ggValue.valueOption)
    res.foreach(ok)
  }

  def makeObj[S <: Sys[S]](c: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val (name, locValue) = c
    val search  = TextSearchObj    .newConst[S](TextSearch(""))
    val loc     = ArtifactLocation .newConst[S](locValue)
    val obj     = Retrieval[S](search, loc)
    if (!name.isEmpty) obj.name = name
    obj :: Nil
  }

  private[this] val _previewsCache = Ref(Option.empty[PreviewsCache])

  private implicit lazy val _client: Client = {
    val se: String = ak.flatMap { n =>
      (0 until 64 by 8).map(i => (((n >>> i) & 0xFF) + '0').toChar)
    } (collection.breakOut)
    Client(ClientId, se)
  }

  private implicit def previewCache(implicit tx: TxnLike): PreviewsCache = {
    import TxnLike.peer
    _previewsCache().getOrElse {
      val res = PreviewsCache(dir = Mellite.cacheDir)
      _previewsCache() = Some(res)
      res
    }
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, E[S]])
    extends FreesoundRetrievalObjView   [S]
      with ListObjView                  [S]
      with ObjViewImpl.Impl             [S]
      with ListObjViewImpl.EmptyRenderer[S]
      with NonEditable                  [S] {

    override def obj(implicit tx: S#Tx): E[S] = objH()

    def factory = FreesoundRetrievalObjView

    def isViewable = true

    // currently this just opens a code editor. in the future we should
    // add a scans map editor, and a convenience button for the attributes
    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val r             = obj
      val tsInit        = r.textSearch.value
      import Mellite.auralSystem
      val rv            = RetrievalView[S](searchInit = tsInit, soundInit = Nil)
      implicit val undo = new UndoManagerImpl
      val fv            = FolderView[S](r.downloads)
      val downloadsView = new FolderFrameImpl.ViewImpl[S](fv).init()
      val name          = AttrCellView.name[S](r)
      val w = new WindowImpl[S](name) {
        val view: View[S] = new EditorImpl[S](rv, downloadsView, tx.newHandle(r.downloadLocation)).init()
      }
      w.init()
      Some(w)
    }
  }

  private sealed trait DownloadMode { def downloadFile: File; def out: File }
  private final case class Direct (out: File) extends DownloadMode {
    def downloadFile: File = out
  }
  private final case class Convert(temp: File, out: File) extends DownloadMode {
    def downloadFile: File = temp
  }
  private final case class Download(sound: Sound, mode: DownloadMode)

  private implicit object AuthPrefsType extends Preferences.Type[Auth] {
    def toString(value: Auth): String = s"${value.accessToken};${value.refreshToken};${value.expires.getTime}"

    def valueOf(string: String): Option[Auth] = {
      val arr = string.split(";")
      if (arr.length == 3) {
        val access    = arr(0)
        val refresh   = arr(1)
        val expiresS  = arr(2)
        Try {
          Auth(accessToken = access, refreshToken = refresh, expires = new Date(expiresS.toLong))
        } .toOption

      } else None
    }
  }

  private def prefsFreesoundAuth: Preferences.Entry[Auth] = Mellite.userPrefs[Auth]("freesound-auth")

  // future fails with `Processor.Aborted` if user declines

  private def findAuth(window: Option[desktop.Window]): Future[Auth] = {
    def andStore(fut: Future[Auth]): Future[Auth] = fut.andThen {
      case Success(newAuth) => prefsFreesoundAuth.put(newAuth)
    }

    prefsFreesoundAuth.get match {
      case Some(oldAuth) =>
        val inHalfAnHour  = new Date(System.currentTimeMillis() + (30L * 60 * 1000))
        val willExpire    = oldAuth.expires.compareTo(inHalfAnHour) < 0
        if (willExpire) {
          implicit val _oldAuth = oldAuth
          val fut = Freesound.refreshAuth()
          andStore(fut)

        } else {
          Future.successful(oldAuth)
        }

      case None =>
        val codeURL = s"https://www.freesound.org/apiv2/oauth2/authorize/?client_id=${_client.id}&response_type=code"
        val codeHTML =
          s"""<html>
             |<body>
             |<p>
             |Mellite has not yet been authorized to download<br>
             |files via your Freesound user account. If you<br>
             |have not yet created a Freesound user account,<br>
             |this is the first step you need to do. Next open<br>
             |the following URL, authorize Mellite and paste<br>
             |the result code back into the answer box in this<br>
             |dialog below:
             |</p>
             |</body>
             |""".stripMargin
        val lbInfo = new Label(codeHTML)

        val lbLink = new Label("Link:", null, Alignment.Trailing)
        val ggLink = new TextField(codeURL, 24)
        ggLink.caret.position = 0
        ggLink.editable = false
        val ggOpen = Button("Browse") {
          Desktop.browseURI(new URI(codeURL))
        }

        val lbCode = new Label("Result Code:", null, Alignment.Trailing)
        val ggCode = new TextField(24)
        val ggPaste = Button("Paste") {
          val cb = Toolkit.getDefaultToolkit.getSystemClipboard
          try {
            val str = cb.getData(DataFlavor.stringFlavor).asInstanceOf[String]
            ggCode.text = str
          } catch {
            case NonFatal(_) =>
          }
        }

        val pane = new GroupPanel {
          horizontal = Par(lbInfo, Seq(
            Par(lbLink, lbCode), Par(Seq(ggLink, ggOpen), Seq(ggCode, ggPaste))
          ))
          vertical = Seq(
            lbInfo, Par(GroupPanel.Alignment.Baseline)(lbLink, ggLink, ggOpen),
            Par(GroupPanel.Alignment.Baseline)(lbCode, ggCode, ggPaste)
          )
        }

        val optPane = OptionPane.confirmation(message = pane, optionType = OptionPane.Options.OkCancel)
        val res = optPane.show(window)
        val code = ggCode.text.trim
        if (res === OptionPane.Result.Ok && code.nonEmpty) {
//          FreesoundImpl.DEBUG = true
          val fut = Freesound.getAuth(code)
          andStore(fut)

        } else {
          Future.failed[Auth](Processor.Aborted())
        }
    }
  }

  private final class EditorImpl[S <: Sys[S]](peer: RetrievalView[S], downloadsView: View[S],
                                              locH: stm.Source[S#Tx, ArtifactLocation[S]])
                                             (implicit val workspace: Workspace[S], val cursor: stm.Cursor[S],
                                              val undoManager: UndoManager)
    extends ViewHasWorkspace[S] with View.Editable[S] {

    def component: Component = peer.component

    def init()(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      this
    }

//    private def performDL(xs: List[Download]): Unit = {
//      xs match {
//        case head :: tail =>
//          Freesound.download(head.sound.id, head.mode.downloadFile)
//      }
////      Future.sequence()
//    }

    private def guiInit(): Unit = {
      val actionDL = Action("Download") {
        val sel = peer.soundTableView.selection
        val dir = cursor.step { implicit tx => locH().value }
        val dl: List[Download] = sel.flatMap { s =>
          val n0: String = s.fileName.collect {
            case c    if c.isLetterOrDigit => c
            case ' ' => '-'
            case '-' => '-'
            case '_' => '_'
          } (breakOut)

          val n1: String = n0.take(12)
          val needsConversion = s.fileType.isCompressed
          val ext = if (needsConversion) "aif" else s.fileType.toProperty
          val n   = s"${s.id}_$n1.$ext"
          val f   = dir / n
          val m: DownloadMode = if (needsConversion) {
            val temp = File.createTemp(suffix = s.fileType.toProperty)
            Convert(temp, f)
          } else Direct(f)
          if (f.exists()) None else Some(Download(s, m))
        } (breakOut)

        if (sel.nonEmpty && dl.isEmpty)
          println(s"${if (sel.size > 1) "Files have" else "File has"} already been downloaded.")

        val authFut = findAuth(desktop.Window.find(component))
        authFut.onComplete { res =>
          println(s"Authorization result: $res")
        }
      }
      actionDL.enabled = false
      val bot: SequentialContainer = peer.resultBottomComponent
      val ggDL = GUI.toolButton(actionDL, freesound.swing.Shapes.Download,
        tooltip = "Downloads selected sound to folder")
      bot.contents += ggDL
      peer.tabbedPane.pages += new TabbedPane.Page("Downloads", downloadsView.component, null)

      peer.soundTableView.addListener {
        case SoundTableView.Selection(sounds) =>
          actionDL.enabled = sounds.nonEmpty
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = peer.dispose()
  }

  private[this] final val ak = Array(
    2455899147606491166L, 2677468186055286084L, 3764232225906169915L, 4834682473675565318L, 5060424300801244677L
  )
}
trait FreesoundRetrievalObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): Retrieval[S]
}