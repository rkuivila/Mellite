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

object CodeImpl {
  private final val COOKIE  = 0x436F6465  // "Code"
  implicit object serializer extends ImmutableSerializer[Code] {
    def write(v: Code, out: DataOutput) {
      out.writeInt(COOKIE)
      out.writeInt(v.id)
      out.writeUTF(v.source)
    }

    def read(in: DataInput): Code = {
      val cookie = in.readInt()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      val id      = in.readInt()
      val source  = in.readUTF()
      import Code._
      (id: @switch) match {
        case FileTransform.id => FileTransform(source)
        case _ => sys.error(s"Unsupported context id $id")
      }
    }
  }

  private val sync    = new AnyRef
  private val codeMap = new mutable.WeakHashMap[String, () => Any]

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

      def wrap(args: (File, File, Processor[Any, _] => Unit), funFut: Future[() => Any]): Future[Unit] =
        funFut.flatMap { fun =>
          val (in, out, procHandler) = args
          val proc = new FileTransformContext(in, out, fun)
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
    def wrap(in: In, funFut: Future[() => Any]): Out
    def blockTag: TypeTag[_]
  }

  def compile[I, O, Repr <: Code { type In = I; type Out = O }](code: Repr)
                      (implicit w: Wrapper[I, O, Repr]): I => O = (in: I) => {
    val funFut = future {
      blocking {
        sync.synchronized {
          codeMap.getOrElseUpdate(code.source, compileThunk(code.source, w))
        }
      }
    }

    w.wrap(in, funFut) // .asInstanceOf[Future[() => B]])
  }

  private final class Intp(cset: nsc.Settings)
    extends IMain(cset, new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)) {

    override protected def parentClassLoader = CodeImpl.getClass.getClassLoader
  }

  private lazy val intp = {
    val cset = new nsc.Settings()
    cset.classpath.value += File.pathSeparator + sys.props("java.class.path")
    val res = new Intp(cset)
    res.initializeSynchronous()
    res
  }

  object Capture {
    private val vr = new ThreadLocal[() => Any]

    def apply[A](thunk: => A) {
      vr.set(() => thunk)
    }
    
    private[CodeImpl] def result: () => Any = vr.get()
  }

  sealed trait Context[A] {
    def __context__(): A
  }
  
  object FileTransformContext extends Context[FileTransformContext] {
    private[CodeImpl] val contextVar = new ThreadLocal[FileTransformContext]
    def __context__(): FileTransformContext = contextVar.get()
  }

  final class FileTransformContext(val in: File, val out: File, fun: () => Any)
    extends ProcessorImpl[Unit, FileTransformContext] {

    protected def body() {
      blocking {
        FileTransformContext.contextVar.set(this)
        fun()
      }
    }
  }

  // note: synchronous
  def compileThunk(code: String, w: Wrapper[_, _, _]): () => Any = {
    val i = intp

    val impS  = w.imports.map(i => s"  import $i\n").mkString
    val bindS = w.binding.map(i =>
     s"""  val __context__ = de.sciss.mellite.impl.CodeImpl.$i.__context__
        |  import __context__._
      """.stripMargin
    ).getOrElse("")
    val aTpe  = w.blockTag.tpe.toString
    val synth =
     s"""de.sciss.mellite.impl.CodeImpl.Capture[$aTpe] {
        |$impS
        |$bindS
        |
      """.stripMargin + code + "\n}"

    val res = i.interpret(synth)
    i.reset()
    res match {
      case Results.Success    => Capture.result // .asInstanceOf[() => A]
      case Results.Error      => throw Code.CompilationFailed()
      case Results.Incomplete => throw Code.CodeIncomplete()
    }
  }
}