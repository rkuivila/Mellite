package de.sciss.mellite.gui

import de.sciss.sonogram
import java.io.File

object SonogramManager {
  private lazy val _instance = {
    val cfg       = sonogram.OverviewManager.Config()
    val folder    = new File(new File(sys.props("user.home"), "mellite"), "cache")
    folder.mkdirs()
    val sizeLimit = 2L << 10 << 10 << 10  // 2 GB
    cfg.caching   = Some(sonogram.OverviewManager.Caching(folder, sizeLimit))
    sonogram.OverviewManager(cfg)
  }

  def instance: sonogram.OverviewManager = _instance

  def acquire(file: File): sonogram.Overview = _instance.acquire(sonogram.OverviewManager.Job(file))
  def release(overview: sonogram.Overview) { _instance.release(overview) }

  implicit def executionContext = _instance.config.executionContext
}