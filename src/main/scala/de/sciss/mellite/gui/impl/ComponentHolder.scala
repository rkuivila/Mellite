/*
 *  ComponentHolder.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

trait ComponentHolder[C] {
  final protected var comp: C = _

  final def component: C = {
    requireEDT()
    val res = comp
    if (res == null) sys.error("Called component before GUI was initialized")
    res
  }
}
