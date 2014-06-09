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
import de.sciss.synth.proc.{SensorSystem, AuralSystem}
import de.sciss.lucre.event.Sys
import javax.swing.UIManager
import scala.util.control.NonFatal
import com.alee.laf.checkbox.WebCheckBoxStyle
import com.alee.laf.progressbar.WebProgressBarStyle
import java.awt.Color

object Mellite extends SwingApplicationImpl("Mellite") {
  type Document = mellite.Workspace[_ <: Sys[_]]

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

    override lazy val usesNativeDecoration = Prefs.nativeWindowDecoration.getOrElse(true)
  }

  protected def menuFactory = MenuBar.instance

  private lazy val _aural   = AuralSystem ()
  private lazy val _sensor: SensorSystem = SensorSystem()

  implicit def auralSystem : AuralSystem  = _aural
  implicit def sensorSystem: SensorSystem = _sensor

  override protected def init(): Unit = {
    Application.init(this)

    // ---- type extensions ----

    mellite.initTypes()

    // ---- look and feel

    try {
      val web = "com.alee.laf.WebLookAndFeel"
      UIManager.installLookAndFeel("Web Look And Feel", web)
      UIManager.setLookAndFeel(Prefs.lookAndFeel.getOrElse(Prefs.defaultLookAndFeel).getClassName)
    } catch {
      case NonFatal(_) =>
    }
    // work-around for web-laf bug #118
    new javax.swing.JSpinner
    // some custom web-laf settings
    WebCheckBoxStyle   .animated            = false
    WebProgressBarStyle.progressTopColor    = Color.lightGray
    WebProgressBarStyle.progressBottomColor = Color.gray
    // XXX TODO: how to really turn of animation?
    WebProgressBarStyle.highlightWhite      = new Color(255, 255, 255, 0)
    WebProgressBarStyle.highlightDarkWhite  = new Color(255, 255, 255, 0)

    LogFrame           .instance    // init
    DocumentViewHandler.instance    // init
    new MainFrame
  }
}
