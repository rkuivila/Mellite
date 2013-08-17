![icon](icons/application.png)

# Mellite

## statement

Mellite is a graphical front end for [SoundProcesses](http://github.com/Sciss/SoundProcesses). It is (C)opyright 2012&ndash;2013 by Hanns Holger Rutz. All rights reserved. Mellite is released under the [GNU General Public License](http://github.com/Sciss/Mellite/blob/master/licenses/Mellite-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`.

## building

Mellite builds with sbt 0.12 and Scala 2.10. The dependencies should be downloaded automatically from maven central repository, except for snapshots during development.

Dependencies not found are all available from their respective [Github repositories](https://github.com/Sciss?tab=repositories), so in case you want to build a snapshot version, you may need to check out these projects and publish them yourself using `sbt publish-local`.
