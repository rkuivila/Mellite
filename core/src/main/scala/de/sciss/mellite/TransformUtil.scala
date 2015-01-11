/*
 *  TransformUtil.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite

import de.sciss.fscape.FScapeJobs
import scala.concurrent.Await
import scala.concurrent.duration._
import java.io.File

object TransformUtil {
  implicit final class RichFScapeJob(val job: FScapeJobs.Doc) {
    def perform(): Unit = {
      val fut = FScape.perform(job)
      Await.result(fut, Duration.Inf)
    }
  }

  def tempFile(): File = File.createTempFile("mellite", ".tmp")

  def file(path: String) = new File(path)
}