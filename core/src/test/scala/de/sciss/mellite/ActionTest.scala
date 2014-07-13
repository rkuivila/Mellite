package de.sciss.mellite

import de.sciss.mellite.impl.{JarUtil, CodeImpl}

import scala.concurrent.Future
import scala.util.Success

object ActionTest extends App {
  class MemoryClassLoader extends ClassLoader {
    private var map: Map[String, Array[Byte]] = Map.empty

    def += (entry: (String, Array[Byte])): Unit = {
      map += entry
    }

    def ++= (entries: Iterable[(String, Array[Byte])]): Unit = {
      map ++= entries
    }

    override protected def findClass(name: String): Class[_] =
      map.get(name).map { bytes =>
        println(s"defineClass($name, ...)")
        defineClass(name, bytes, 0, bytes.length)

      } .getOrElse(super.findClass(name)) // throws exception
  }

  val exampleSource =
    """val xs = List("hello", "world")
      |println(xs.map(_.capitalize).mkString(" "))
      |""".stripMargin

  iter("Foo").andThen {
    case Success(_) => iter("Bar").andThen {
      case Success(bytes) =>
        val map = JarUtil.unpack(bytes)
        val cl = new MemoryClassLoader
        cl ++= map
        val clName  = s"${CodeImpl.UserPackage}.Bar"
        println(s"Resolving class '$clName'...")
        val clazz = Class.forName(clName, true, cl)
        println("Instantiating...")
        val x     = clazz.newInstance().asInstanceOf[() => Unit]
        println("Invoking 'apply':")
        x()
    }
  }

  def iter(name: String): Future[Array[Byte]] = {
    val code = Code.Action(exampleSource)
    val fut = CodeImpl.compileToFunction[Code.Action](name, code)

    fut.onComplete { res =>
      println("Result:")
      println(res)
      res match {
        case Success(bytes) =>
          val map = JarUtil.unpack(bytes)
          map.keys.foreach(println)

        case _ =>
      }
    }

    fut // .map(_ => ())
  }
}
