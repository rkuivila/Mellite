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