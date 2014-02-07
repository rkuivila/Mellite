/*
 *  Mellite.scala
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

package de.sciss
package mellite

import de.sciss.mellite.gui.{LogFrame, MainFrame, MenuBar}
import desktop.impl.SwingApplicationImpl
import synth.proc.AuralSystem

object Mellite extends SwingApplicationImpl("Mellite") {
  type Document = mellite.Document[_]

  // lucre.event    .showLog = true
  // lucre.confluent.showLog = true
  // synth.proc.showAuralLog     = true
  // synth.proc.showLog          = true
  // synth.proc.showTransportLog = true
  // showLog                     = true

  protected def menuFactory = MenuBar.instance

  private lazy val _aural = AuralSystem()

  implicit def auralSystem: AuralSystem = _aural

  override protected def init(): Unit = {
    LogFrame.instance
    new MainFrame
  }
}
