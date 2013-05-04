//package de.sciss.lucre.event
//package impl
//
//import collection.immutable.{IndexedSeq => IIdxSeq}
//
//trait MultiEventImpl[S <: Sys[S], A, A1, Repr] extends Node[S] {
//  self: Repr =>
//
//  protected def events: IIdxSeq[Event[S, A1, Repr]]
//  protected def reader: Reader[S, Repr]
//  protected def foldUpdate(sum: Option[A], inc: A1): Option[A]
//
//  object changed extends impl.EventImpl[S, A, Repr] with InvariantSelector[S]{
//    def node: Repr with Node[S] = self
//
//    def --->(r: Selector[S])(implicit tx: S#Tx) {
//      events.foreach(_ ---> r)
//    }
//
//    def -/->(r: Selector[S])(implicit tx: S#Tx) {
//      events.foreach(_ -/-> r)
//    }
//
//    override def toString = node.toString + ".changed"  // default toString invokes `slot`!
//    def slot: Int = throw new UnsupportedOperationException
//
//    def connect()( implicit tx: S#Tx ) {}
//    def disconnect()( implicit tx: S#Tx ) {}
//
//    protected def reader: Reader[S, Repr] = self.reader
//
//    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = {
//      events.foldLeft(Option.empty[A]) { case (res, e) =>
//        if (e.isSource(pull)) {
//          pull(e) match {
//            case Some(upd) => foldUpdate(res, upd)
//            case _ => res
//          }
//        } else res
//      }
//    }
//  }
//
//  final def select(slot: Int, invariant: Boolean): Event[S, Any, Any] = {
//    events.find(_.slot == slot) getOrElse sys.error(s"Invalid slot $slot")
//  }
//}