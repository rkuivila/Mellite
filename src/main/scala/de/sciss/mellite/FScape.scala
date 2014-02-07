/*
 *  FScape.scala
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

import de.sciss.fscape.FScapeJobs
import scala.concurrent.{Promise, Future}

object FScape {
  private lazy val instance: Future[FScapeJobs] = {
    // import osc.Implicits._
    val res = FScapeJobs() // new FScapeJobs(transport = osc.TCP, addr = localhost -> FScapeJobs.DEFAULT_PORT, numThreads = 1)
    val p   = Promise[FScapeJobs]()
    res.connect(timeOut = 10.0) { success =>
      if (success) p.success(res) else p.failure(new Exception("Could not connect to FScape"))
    }
    p.future
  }

  def perform(job: FScapeJobs.Doc): Future[Unit] = {
    instance.flatMap { fsc =>
      val p = Promise[Unit]()
      fsc.process("mellite", job) { success =>
        if (success) p.success() else p.failure(new Exception("FScape failed to process job"))
      }
      p.future
    }
  }
}