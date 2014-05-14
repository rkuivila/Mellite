//package de.sciss.mellite
//package sensor
//
//import de.sciss.osc
//import de.sciss.lucre.stm.TxnLike
//import impl.{SensorSystemImpl => Impl}
//
//object SensorSystem {
//  def apply(): SensorSystem = Impl()
//
//  def start(config: Config = defaultConfig)(implicit tx: TxnLike): SensorSystem = {
//    val res = apply()
//    res.start(config)
//    res
//  }
//
//  type Config = osc.Channel.Net.Config
//
//  def defaultConfig: Config = {
//    val builder = Prefs.defaultSensorProtocol match {
//      case osc.UDP => osc.UDP.Config()
//      case osc.TCP => osc.TCP.Config()
//    }
//    builder.localPort = Prefs.defaultSensorPort
//    // builder.localIsLoopback = true
//    builder.build
//  }
//
//  trait Client {
//    def started(c: Server)(implicit tx: TxnLike): Unit
//    def stopped()         (implicit tx: TxnLike): Unit
//  }
//
//  type Server = osc.Receiver.Undirected.Net
//}
//trait SensorSystem {
//  import SensorSystem.{Config, Client, Server}
//
//  def start(config: Config = SensorSystem.defaultConfig)(implicit tx: TxnLike): Unit
//
//  def stop()(implicit tx: TxnLike): Unit
//
//  def addClient   (c: Client)(implicit tx: TxnLike): Unit
//  def removeClient(c: Client)(implicit tx: TxnLike): Unit
//
//  def whenStarted(fun: Server => Unit)(implicit tx: TxnLike): Unit
//
//  def serverOption(implicit tx: TxnLike): Option[Server]
//}
