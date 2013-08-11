package de.sciss
package mellite
package impl

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import scala.annotation.switch
import scala.concurrent._
import scala.collection.mutable
import java.io.{ObjectInputStream, ByteArrayInputStream, ObjectOutputStream, File}
import de.sciss.processor.Processor
import scala.tools.nsc
import scala.tools.nsc.interpreter.{Results, IMain}
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter}
import de.sciss.processor.impl.ProcessorImpl
import collection.immutable.{Seq => ISeq}
import reflect.runtime.universe.{typeTag, TypeTag}
import de.sciss.serial.impl.ByteArrayOutputStream

object CodeImpl {
  private final val COOKIE  = 0x436F6465  // "Code"
  implicit object serializer extends ImmutableSerializer[Code] {
    def write(v: Code, out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeInt(v.id)
      out.writeUTF(v.source)
    }

    def read(in: DataInput): Code = {
      val cookie = in.readInt()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      val id      = in.readInt()
      val source  = in.readUTF()
      Code.apply(id, source)
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
        "de.sciss.file._",
        "de.sciss.fscape.{FScapeJobs => fscape}",
        "de.sciss.mellite.TransformUtil._"
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
    // private val vr = new ThreadLocal[() => Any]
    private val vr = new ThreadLocal[Array[Byte]]

    def apply[A](thunk: => A): Unit = {
      val fun = () => thunk
      val baos  = new ByteArrayOutputStream()
      val oos   = new ObjectOutputStream(baos)
      oos.writeObject(fun)
      oos.close()
      // vr.set(() => thunk)
      vr.set(baos.toByteArray)
    }
    
    private[CodeImpl] def result: () => Any = {
      // vr.get()
      val bais  = new ByteArrayInputStream(vr.get())
      val ois   = new ObjectInputStream(bais)
      val fun   = ois.readObject().asInstanceOf[() => Any]
      ois.close()
      fun
    }
  }

  sealed trait Context[A] {
    def __context__(): A
  }
  
  object FileTransformContext extends Context[FileTransformContext#Bindings] {
    private[CodeImpl] val contextVar = new ThreadLocal[FileTransformContext#Bindings]
    def __context__(): FileTransformContext#Bindings = contextVar.get()
  }

  final class FileTransformContext(in: File, out: File, fun: () => Any)
    extends ProcessorImpl[Unit, FileTransformContext] {
    process =>

    type Bindings = Bindings.type
    object Bindings {
      def in : File = process.in
      def out: File = process.out
      def checkAborted(): Unit = process.checkAborted()
      def progress(f: Double): Unit = {
        process.progress(f.toFloat)
        process.checkAborted()
      }
    }

    protected def body(): Unit =
      blocking {
        //      val prom  = Promise[Unit]()
        //      val t = new Thread {
        //        override def run(): Unit = {
        println("---1")
        FileTransformContext.contextVar.set(Bindings)
        try {
          fun()
          println("---2")
          //            prom.complete(Success())
        } catch {
          case e: Exception =>
            e.printStackTrace()
            throw e
          //              prom.complete(Failure(e))
        }
        //        }
        //      }
        //      t.start()
        //      Await.result(prom.future, Duration.Inf)
      }
  }

  // note: synchronous
  private def compileThunk(code: String, w: Wrapper[_, _, _]): () => Any = {
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
    // commented out to chase ClassNotFoundException
    // i.reset()
    res match {
      case Results.Success    => Capture.result // .asInstanceOf[() => A]
      case Results.Error      => throw Code.CompilationFailed()
      case Results.Incomplete => throw Code.CodeIncomplete()
    }
  }
}