package de.sciss.mellite
package sensor

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr

trait Sensor[S <: Sys[S]] {
  def path : Expr.Var[S, String]
  def value: Expr    [S, Double]
}
