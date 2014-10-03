    val p = Attribute[Proc]("key")
    p.attr

    self.attr[Ensemble]("key").foreach { ens =>
      ens.playing = true
    }
    
    trait ActionBody {
      def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit
    }
        
    class Synthetic123 extends ActionBody {
      def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit = {
        import universe._
        source-expansion
      }
    }
    
    trait Universe[S <: Sys[S]] {
      def self: Action
      
      trait Obj {
        def attr: AttrMap
      }
      
      trait Action {
        
      }
      
      trait AttrMap {
        def apply[A](key: String): Option[A]
      }
    }