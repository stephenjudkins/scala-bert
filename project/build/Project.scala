import sbt._

class Sbert(info: ProjectInfo) extends DefaultProject(info)
{
  val snapshots = "Scalatools Snapshots" at "http://scala-tools.org/repo-snapshots"
  val specs = "org.scala-tools.testing" % "specs" % "1.6.5" % "test"
  val check = "org.scala-tools.testing" % "scalacheck_2.8.0" % "1.7"
}
