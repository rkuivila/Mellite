//package de.sciss.mellite
//
//import de.sciss.synth.proc.{Compiler, Code}
//
//object IntpTest extends App {
//  // TransformImpl.test2()
//
//  //  val process = TransformImpl.test3(new File("alpha"), new File("beta"),
//  //    """println(s"We're frickin here. input is $in, output is $out")
//  //      |
//  //    """.stripMargin
//  //  )
//  //  process.addListener {
//  //    case any => println(s"Observed: $any")
//  //  }
//  //  process.start()
//
//  implicit val compiler = Compiler()
//  val ft = Code.FileTransform("""println(s"input = $in, output = $out")""")
//  val res = ft.execute(new java.io.File("alpha"), new java.io.File("beta"), _.addListener { case x => println(s"Observed: $x") })
//  println(res)
//
//  val t: Thread = new Thread {
//    override def run(): Unit =
//      t.synchronized(t.wait())
//  }
//  t.start()
//
//  res.onComplete { futRes =>
//    println(s"Future result: $futRes")
//    t.synchronized(t.notifyAll())
//  }
//}