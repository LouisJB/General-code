import sbt._

final class SbtConfig(info: ProjectInfo) extends DefaultProject(info)
{
  val snapshots = ScalaToolsSnapshots

  // dependancies...
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.0.RC2"

  val scalaTest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" % "test"  
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" // % "test" 


  // ***** repositories ***** 

  // required because Ivy doesn't pull repositories from poms
  val smackRepo = "m2-repository-smack" at "http://maven.reucon.com/public"

  val snapshotRepo = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  lazy val ruc = task { _ match {
    case Array(mainClassName)   => runTask(Some(mainClassName), runClasspath) dependsOn(compile, copyResources)
    case _                      => task { Some("Usage: ruc <className>") }
  } }

//  override def runJVMOptions = super.runJVMOptions ++ Seq("-Xmx512m")

  override def fork = Some(new ForkScalaRun {
      override def runJVMOptions = super.runJVMOptions ++ Seq("-Xmx4096m")
      override def scalaJars = Seq(buildLibraryJar.asFile, buildCompilerJar.asFile)
  })
}

