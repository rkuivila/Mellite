/*
 *  SonogramManager.scala
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

package de.sciss.mellite.gui

import de.sciss.sonogram
import java.io.File
import de.sciss.dsp.{Threading, ConstQ}

object SonogramManager {
  private lazy val _instance = {
    val cfg               = sonogram.OverviewManager.Config()
    val folder            = new File(new File(sys.props("user.home"), "mellite"), "cache")
    folder.mkdirs()
    val sizeLimit         = 2L << 10 << 10 << 100  // 20 GB
    cfg.caching           = Some(sonogram.OverviewManager.Caching(folder, sizeLimit))
    // currently a problem with JTransforms
    // cfg.executionContext  = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
    // cfg.executionContext  = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
    sonogram.OverviewManager(cfg)
  }

  def instance: sonogram.OverviewManager = _instance

  def acquire(file: File): sonogram.Overview = {
    val cq    = ConstQ.Config()
    cq.bandsPerOct  = 18
    cq.maxTimeRes   = 18
    cq.threading    = Threading.Single
    val job   = sonogram.OverviewManager.Job(file, cq)
    _instance.acquire(job)
  }
  def release(overview: sonogram.Overview): Unit = _instance.release(overview)

  implicit def executionContext = _instance.config.executionContext
}