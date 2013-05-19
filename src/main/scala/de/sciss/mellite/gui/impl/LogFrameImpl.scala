package de.sciss.mellite
package gui
package impl

import swing.{Component, ScrollPane, Swing}
import de.sciss.scalainterpreter.LogPane
import java.io.{PrintStream, OutputStream}
import javax.swing.BorderFactory
import swing.event.WindowClosing
import de.sciss.desktop.impl.WindowImpl
import de.sciss.desktop.Window

// lazy window - opens as soon as something goes to the console
private[gui] final class LogFrameImpl extends LogFrame with WindowImpl {
  frame =>

  //  peer.getRootPane.putClientProperty("Window.style", "small")

  def style = Window.Auxiliary

  def handler = Mellite.windowHandler

  component.peer.getRootPane.putClientProperty("Window.style", "small")

  val log = {
    val cfg = LogPane.Settings()
    cfg.rows = 24
    LogPane(cfg)
  }

  private val printLog = new PrintStream(log.outputStream)

  private val observer: OutputStream = new OutputStream {
    override def write(b: Array[Byte], off: Int, len: Int) {
      log.makeDefault() // detaches this observer
      System.setOut(printLog) // XXX TODO: should investigate why we need this as well, and incorporate it into LogPane
      System.setErr(printLog)
      log.outputStream.write(b, off, len)
      Swing.onEDT(frame.front()) // there we go
    }

    def write(b: Int) {
      write(Array(b.toByte), 0, 1)
    }
  }

  private val printObserver = new PrintStream(observer)

  def observe() {
    Console.setOut(observer)
    Console.setErr(observer)
    System.setOut(printObserver)
    System.setErr(printObserver)
  }

  observe()
  // closeOperation = Window.CloseIgnore
  reactions += {
    case WindowClosing(_) =>
      // frame.visible = false
      observe()
  }

  contents = new ScrollPane {
    contents = Component.wrap(log.component)
    border = BorderFactory.createEmptyBorder()
  }

  title = "Log"
  pack()

  import LogFrame._

  GUI.placeWindow(frame, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)
}