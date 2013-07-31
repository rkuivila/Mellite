package de.sciss.mellite
package impl

import de.sciss.scalainterpreter.Interpreter

object InterpreterSingleton {
  private val sync = new AnyRef

  private var funs  = IndexedSeq.empty[Interpreter => Unit]
  private var inOpt = Option    .empty[Interpreter]

  //   object Result {
  //      var value : Any = ()
  //   }
  //
  //   def wrap( code: String ) : String = {
  //      "de.sciss.mellite.gui.impl.InterpreterSingleton.Result.value = {" + code + "}"
  //   }

  def apply(fun: Interpreter => Unit): Unit =
    sync.synchronized {
      inOpt match {
        case Some(in) =>
          fun(in)
        case _ =>
          makeOne
          funs :+= fun
      }
    }

  private lazy val makeOne = {
    val cfg = Interpreter.Config()
    cfg.imports ++= Seq(
      "de.sciss.synth",
      "synth._",
      "ugen._"
    )
    //      cfg.bindings += NamedParam( Result )
    Interpreter.async(cfg) { in =>
      sync.synchronized {
        inOpt = Some(in)
        val f = funs
        funs  = Vector.empty
        f.foreach(_.apply(in))
      }
    }
  }
}
