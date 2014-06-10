package de.sciss.mellite
package gui
package impl
package interpreter

import de.sciss.scalainterpreter.{InterpreterPane, Interpreter, CodePane}
import java.io.{IOException, FileInputStream, File}
import swing.Component
import de.sciss.desktop.Window

// careful... tripping over SI-3809 "illegal cyclic reference involving class Array"...
// actually SI-7481
private[gui] object InterpreterFrameImpl {
  val boom = Array(1, 2, 3)  // forcing scalac to recompile, so it doesn't crash

  private def readFile(file: File): String = {
    val fis = new FileInputStream(file)
    try {
      val arr = new Array[Byte](fis.available())
      fis.read(arr)
      new String(arr, "UTF-8")
    } finally {
      fis.close()
    }
  }

  def apply(): InterpreterFrame = {
    val codeCfg = CodePane.Config()

    val file = new File(/* new File( "" ).getAbsoluteFile.getParentFile, */ "interpreter.txt")
    if (file.isFile) try {
      codeCfg.text = readFile(file)
    } catch {
      case e: IOException => e.printStackTrace()
    }

    val intpCfg = Interpreter.Config()
    intpCfg.imports = List(
      "de.sciss.mellite._",
      "de.sciss.synth._",
      "Ops._",
      "concurrent.duration._",
      "gui.InterpreterFrame.Bindings._"
    )

    //      intpCfg.bindings = Seq( NamedParam( "replSupport", replSupport ))
    //         in.bind( "s", classOf[ Server ].getName, ntp )
    //         in.bind( "in", classOf[ Interpreter ].getName, in )

    //      intpCfg.out = Some( LogWindow.instance.log.writer )

    val intp = InterpreterPane(interpreterConfig = intpCfg, codePaneConfig = codeCfg)

    new InterpreterFrame {
      val component = new de.sciss.desktop.impl.WindowImpl {
        frame =>

        def handler = Application.windowHandler

        override def style = Window.Auxiliary

        title           = "Interpreter"
        contents        = Component.wrap(intp.component)
        closeOperation  = Window.CloseDispose
        pack()
        GUI.centerOnScreen(this)
        front()
      }
    }
  }
}
