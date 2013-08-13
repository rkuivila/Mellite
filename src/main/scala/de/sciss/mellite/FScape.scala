/*
 *  FScape.scala
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