/*
 *  Mellite.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite

import de.sciss.mellite.gui.{DocumentViewHandler, LogFrame, MainFrame, MenuBar}
import de.sciss.desktop.impl.{SwingApplicationImpl, WindowHandlerImpl}
import de.sciss.desktop.WindowHandler
import synth.proc.AuralSystem
import de.sciss.lucre.event.Sys
import javax.swing.UIManager
import scala.util.control.NonFatal
import com.alee.laf.WebLookAndFeel

object Mellite extends SwingApplicationImpl("Mellite") {
  type Document = mellite.Document[_ <: Sys[_]]

  // lucre.event    .showLog = true
  // lucre.confluent.showLog = true
  // synth.proc.showAuralLog     = true
  // synth.proc.showLog          = true
  // synth.proc.showTransportLog = true
  // showLog                     = true

  override lazy val windowHandler: WindowHandler = new WindowHandlerImpl(this, menuFactory) {
    override lazy val usesInternalFrames = {
      false // XXX TODO: eventually a preferences entry
    }
  }

  protected def menuFactory = MenuBar.instance

  private lazy val _aural = AuralSystem()

  implicit def auralSystem: AuralSystem = _aural

  override protected def init(): Unit = {
    try {
      // WebLookAndFeel.install()
      UIManager.installLookAndFeel("Web Look And Feel", classOf[WebLookAndFeel].getName)
      UIManager.setLookAndFeel(Prefs.lookAndFeel.getOrElse(Prefs.defaultLookAndFeel).getClassName)
    } catch {
      case NonFatal(_) =>
    }

    UIManager.

    LogFrame           .instance    // init
    DocumentViewHandler.instance    // init
    new MainFrame
  }
}
