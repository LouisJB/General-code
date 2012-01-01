import sbt._
import Keys._

object ProjectSettings extends Build{

  val snapshots = ScalaToolsSnapshots

  // dependancies...
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.9.1"
  val scalaTest = "org.scalatest" % "scalatest" % "1.4.RC2" % "test"  
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.9.1.RC3" % "1.9" // % "test" 
  val scalaZ = "org.scalaz" % "scalaz-full_2.9.1" % "6.0.3" withSources()
  val colt = "colt" %  "colt" % "1.2.0" // withSources()
  val apacheMath = "org.apache.commons" % "commons-math" % "2.0"

  val dependencies = Seq(scalaTest, scalacheck, scalaSwing, scalaZ, colt, apacheMath)

  // ***** repositories ***** 
  // required because Ivy doesn't pull repositories from poms
  val smackRepo = "m2-repository-smack" at "http://maven.reucon.com/public"

  val snapshotRepo = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"


  lazy val defaultSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.9.1",
    libraryDependencies ++= dependencies,
    retrieveManaged := true
  )

  lazy val prj = Project(
    "Scala-stuff",
    file("."),
    settings = defaultSettings
  )

  /*
  lazy val ruc = task { _ match {
    case Array(mainClassName)   => runTask(Some(mainClassName), runClasspath) dependsOn(compile, copyResources)
    case _                      => task { Some("Usage: ruc <className>") }
  } }

//  override def runJVMOptions = super.runJVMOptions ++ Seq("-Xmx512m")

  override def fork = Some(new ForkScalaRun {
      override def runJVMOptions = super.runJVMOptions ++ Seq("-Xmx4096m")
      override def scalaJars = Seq(buildLibraryJar.asFile, buildCompilerJar.asFile)
  })
  */
}

