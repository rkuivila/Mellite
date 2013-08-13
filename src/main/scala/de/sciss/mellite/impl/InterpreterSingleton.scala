/*
 *  InterpreterSingleton.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package impl

import de.sciss.scalainterpreter.Interpreter

import collection.immutable.{IndexedSeq => Vec}

object InterpreterSingleton {
  private val sync = new AnyRef

  private var funs  = Vec   .empty[Interpreter => Unit]
  private var inOpt = Option.empty[Interpreter]

  //   object Result {
  //      var value : Any = ()
  //   }
  //
  //   def wrap( code: String ) : String = {
  //      "de.sciss.mellite.gui.impl.InterpreterSingleton.Result.value = {" + code + "}"
  //   }

  def apply(fun: Interpreter => Unit): Unit =
    sync.synchronized {
      inOpt match {
        case Some(in) =>
          fun(in)
        case _ =>
          makeOne
          funs :+= fun
      }
    }

  private lazy val makeOne = {
    val cfg = Interpreter.Config()
    cfg.imports ++= Seq(
      "de.sciss.synth",
      "synth._",
      "ugen._"
    )
    //      cfg.bindings += NamedParam( Result )
    Interpreter.async(cfg) { in =>
      sync.synchronized {
        inOpt = Some(in)
        val f = funs
        funs  = Vector.empty
        f.foreach(_.apply(in))
      }
    }
  }
}
