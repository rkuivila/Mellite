//package de.sciss.mellite
//package gui
//package impl
//package document
//
//import de.sciss.synth.proc.Sys
//import de.sciss.file._
//
//object ElementViewActions {
//  def findAudioFile[S <: Sys[S]](root: ElementView.FolderLike[S], file: File): Option[ElementView.AudioGrapheme[S]] = {
//    requireEDT()
//
//    def loop(folder: ElementView.FolderLike[S]): Option[ElementView.AudioGrapheme[S]] =
//      folder.children.foreach {
//        case a: ElementView.AudioGrapheme[S] if a.value.artifact == file => a
//        case f: ElementView.FolderLike[S] => loop(f)
//        case _ =>
//      }
//
//    loop(root)
//  }
//}
