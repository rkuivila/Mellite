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