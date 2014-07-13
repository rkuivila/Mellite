package de.sciss.mellite.impl

import java.io.{ByteArrayOutputStream, ByteArrayInputStream, BufferedInputStream}

import scala.annotation.tailrec
import scala.tools.nsc.io._

object JarUtil {
  // cf. http://stackoverflow.com/questions/1281229/how-to-use-jaroutputstream-to-create-a-jar-file
  def pack(base: AbstractFile): Array[Byte] = {
    import java.util.jar._

    val mf = new Manifest
    mf.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    val bs    = new java.io.ByteArrayOutputStream
    val out   = new JarOutputStream(bs, mf)

    def add(prefix: String, f: AbstractFile): Unit = {
      val name0 = prefix + f.name // f.getName
      val name  = if (f.isDirectory) name0 + "/" else name0
      val entry = new JarEntry(name)
      entry.setTime(f.lastModified /* () */)
      // if (f.isFile) entry.setSize(f.length())
      out.putNextEntry(entry)
      if (!f.isDirectory /* f.isFile */) {
        val in = new BufferedInputStream(f.input /* new FileInputStream(f) */)
        try {
          val buf = new Array[Byte](1024)
          @tailrec def loop(): Unit = {
            val count = in.read(buf)
            if (count >= 0) {
              out.write(buf, 0, count)
              loop()
            }
          }
          loop()
        } finally {
          in.close()
        }
      }
      out.closeEntry()
      if (f.isDirectory) f /* .listFiles */ .foreach(add(name, _))
    }

    base /* .listFiles() */.foreach(add("", _))
    out.close()
    bs.toByteArray
  }

  def unpack(bytes: Array[Byte]): Map[String, Array[Byte]] = {
    import java.util.jar._
    import scala.annotation.tailrec

    val in = new JarInputStream(new ByteArrayInputStream(bytes))
    val b  = Map.newBuilder[String, Array[Byte]]

    @tailrec def loop(): Unit = {
      val entry = in.getNextJarEntry
      if (entry != null) {
        if (!entry.isDirectory) {
          val name  = entry.getName

          // cf. http://stackoverflow.com/questions/8909743/jarentry-getsize-is-returning-1-when-the-jar-files-is-opened-as-inputstream-f
          val bs  = new ByteArrayOutputStream
          var i   = 0
          while (i >= 0) {
            i = in.read()
            if (i >= 0) bs.write(i)
          }
          val bytes = bs.toByteArray
          b += mkClassName(name) -> bytes
        }
        loop()
      }
    }
    loop()
    in.close()
    b.result()
  }

  /* Converts a jar entry name with slashes to a class name with dots
   * and dropping the `class` extension
   */
  private def mkClassName(path: String): String = {
    require(path.endsWith(".class"))
    path.substring(0, path.length - 6).replace("/", ".")
  }
}
