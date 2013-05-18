package de.sciss
package mellite
package impl

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import scala.annotation.switch
import scala.concurrent._
import scala.collection.mutable
import java.io.File
import de.sciss.processor.Processor
import scala.tools.nsc
import scala.tools.nsc.interpreter.{Results, IMain}
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter}
import de.sciss.processor.impl.ProcessorImpl
import collection.immutable.{Seq => ISeq}
import reflect.runtime.universe.{typeTag, TypeTag}
import scala.util.control.NonFatal
import scala.util.{Success, Failure}
import scala.concurrent.duration.Duration

object CodeImpl2 {

  // ---- internals ----

  object Wrapper {
    implicit object FileTransform
      extends Wrapper[(File, File, Processor[Any, _] => Unit), Future[Unit], Code.FileTransform] {

      def imports: ISeq[String] = ISeq(
        "de.sciss.synth.io.{AudioFile, AudioFileSpec, AudioFileType, SampleFormat, Frames}",
        "de.sciss.synth._",
        "de.sciss.mellite.RichFile"
      )

      def binding: Option[String] = Some("FileTransformContext")

      def wrap(args: (File, File, Processor[Any, _] => Unit))(fun: => Any): Future[Unit] = {
        val (in, out, procHandler) = args
        val proc = new FileTransformContext(in, out, () => fun)
        procHandler(proc)
        proc.start()
        proc
      }

      def blockTag = typeTag[Unit]
    }
  }
  trait Wrapper[In, Out, Repr] {
    def imports: ISeq[String]
    def binding: Option[String]
    // def wrap(in: In, funFut: Future[() => Any]): Out
    def wrap(in: In)(fun: => Any): Out
    def blockTag: TypeTag[_]
  }

  def execute[I, O, Repr <: Code { type In = I; type Out = O }](code: Repr, in: I)
                      (implicit w: Wrapper[I, O, Repr]): O = {
    w.wrap(in) {
      compileThunk(code.source, w)
    }
  }

  def compileBody[I, O, Repr <: Code { type In = I; type Out = O }](code: Repr)
                      (implicit w: Wrapper[I, O, Repr]): Future[Unit] = {
    future {
      blocking {
        compileThunk(code.source, w)
      }
    }
  }

  private final class Intp(cset: nsc.Settings)
    extends IMain(cset, new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)) {

    override protected def parentClassLoader = CodeImpl2.getClass.getClassLoader
  }

  private lazy val intp = {
    val cset = new nsc.Settings()
    cset.classpath.value += File.pathSeparator + sys.props("java.class.path")
    val res = new Intp(cset)
    res.initializeSynchronous()
    res
  }

  object Run {
    def apply[A](thunk: => A): A = thunk
  }

  sealed trait Context[A] {
    def __context__(): A
  }
  
  object FileTransformContext extends Context[FileTransformContext#Bindings] {
    private[CodeImpl2] val contextVar = new ThreadLocal[FileTransformContext#Bindings]
    def __context__(): FileTransformContext#Bindings = contextVar.get()
  }

  final class FileTransformContext(in: File, out: File, fun: () => Any)
    extends ProcessorImpl[Unit, FileTransformContext] {
    process =>

    type Bindings = Bindings.type
    object Bindings {
      def in : File = process.in
      def out: File = process.out
      def checkAborted() { process.checkAborted() }
      def progress(f: Double) {
        process.progress(f.toFloat)
        process.checkAborted()
      }
    }

    protected def body() {
      // blocking {
      val prom  = Promise[Unit]()
      val t = new Thread {
        override def run() {
          FileTransformContext.contextVar.set(Bindings)
          try {
            fun()
            prom.complete(Success())
          } catch {
            case e: Exception =>
              e.printStackTrace()
              prom.complete(Failure(e))
          }
        }
      }
      t.start()
      Await.result(prom.future, Duration.Inf)
      // }
    }
  }

  private val pkg = "de.sciss.mellite.impl.CodeImpl2"

  // note: synchronous
  private def compileThunk(code: String, w: Wrapper[_, _, _]): Any = {
    val i = intp

    val impS  = w.imports.map(i => s"  import $i\n").mkString
    val bindS = w.binding.map(i =>
     s"""  val __context__ = $pkg.$i.__context__
        |  import __context__._
      """.stripMargin
    ).getOrElse("")
    val aTpe  = w.blockTag.tpe.toString
    val synth =
     s"""$pkg.Run[$aTpe] {
        |$impS
        |$bindS
        |
      """.stripMargin + code + "\n}"

    val res = i.interpret(synth)
    // commented out to chase ClassNotFoundException
    // i.reset()
    res match {
      case Results.Success =>
        if (aTpe == "Unit") () else {
          val n = i.mostRecentVar
          i.valueOfTerm(n).getOrElse(sys.error(s"No value for term $n"))
        }

      case Results.Error      => throw Code.CompilationFailed()
      case Results.Incomplete => throw Code.CodeIncomplete()
    }
  }
}