package de.sciss
package mellite
package impl

import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter}
import java.io.File
import scala.tools.nsc.interpreter.{Results, IMain}
import scala.tools.nsc
import scala.concurrent.{blocking, Future}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl

object TransformImpl {
  private final class Intp(cset: nsc.Settings)
    extends IMain(cset, new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)) {

    override protected def parentClassLoader = TransformImpl.getClass.getClassLoader
  }

  private lazy val intp = {
    val cset = new nsc.Settings()
    cset.classpath.value += File.pathSeparator + sys.props("java.class.path")
    val res = new Intp(cset)
    res.initializeSynchronous()
    res
  }

  //  def execute(in: File, out: File, code: String): Future[Unit] = {
  //    new GenCode(in, out, code)
  //    ???
  //  }

  // private final class

  object Bindings {
//    private[TransformImpl] val _in  = new ThreadLocal[File]
//    def in: File = _in.get()
//    private[TransformImpl] val _out = new ThreadLocal[File]
//    def out: File = _out.get()
    private[TransformImpl] val _thisProcess   = new ThreadLocal[GenCode]
    private[TransformImpl] val _thisThunk     = new ThreadLocal[() => Unit]
    def thisProcess(): GenCode = _thisProcess.get()
  }

  final class GenCode(val in: File, val out: File, fun: () => Unit)
    extends ProcessorImpl[Unit, GenCode] {

    protected def body() {
      blocking {
//        Bindings._in .set(in)
//        Bindings._out.set(out)
        Bindings._thisProcess.set(this)
        fun()
      }
    }
  }

  object Foo {
    def bar() { println("BAR" )}
  }

  object Wrapper {
    def apply(thunk: => Unit) {
      Bindings._thisThunk.set(() => thunk)
    }
  }

  def test3(in: File, out: File, code: String): Processor[Unit, _] with Processor.Prepared = {
    val i = intp
    val pre =
      """de.sciss.mellite.impl.TransformImpl.Wrapper {
        |  val __import__ = de.sciss.mellite.impl.TransformImpl.Bindings.thisProcess()
        |  import __import__._
        |
        |""".stripMargin

    val post =
      """
        |}
      """.stripMargin

    val res = i.interpret(pre + code + post)
    i.reset()
    res match {
      case Results.Success =>
        val fun = Bindings._thisThunk.get()
        new GenCode(in, out, fun)
      case Results.Error =>
        sys.error("Compilation failed")
      case Results.Incomplete =>
        sys.error("Code incomplete")
    }
  }

  def test2() {
    val i = intp
    for(j <- 0 until 3) {
      val user  = "foo.bar()"
      val synth =  """import de.sciss.mellite.impl.TransformImpl.{Foo => foo}
                   """.stripMargin + user
      val res = i.interpret(synth)
      println(s"res$j: $res")
      i.reset()
    }
  }

  def test() {
    val i = intp
    i.bind[Foo.type]("foo", Foo)
    val res0 = i.interpret("foo.bar(); val x = 33")
    println(s"res0: $res0")
    i.reset()
    val res1 = i.interpret("println(x)")
    println(s"res1: $res1")
    i.reset()
    val res2 = i.interpret("foo.bar()")
    println(s"res2: $res2")
  }
}