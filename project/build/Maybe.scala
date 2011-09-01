/*
 * Copyright 2010-2011 Christos KK Loverdos
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.Calendar
import sbt._

class Maybe(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = super.compileOptions ++
    Seq("-deprecation",
      "-Xmigration",
      "-Xcheckinit",
      "-optimise",
      "-explaintypes",
      "-unchecked",
      "-encoding", "utf8")
      .map(CompileOption(_))

  def extraResources = "LICENSE.txt"
  override def mainResources = super.mainResources +++ extraResources
  
  val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test->default"
  
  override def testOptions = 
    super.testOptions ++ 
    Seq(TestArgument(TestFrameworks.JUnit, "-q", "-v"))

//  val lib_scalatest = "org.scalatest"  %% "scalatest" % "1.4.1"  % "test"     withSources()
  val lib_scalatest =  buildScalaVersion match {
    case "2.8.1"   => "org.scalatest" % "scalatest_2.8.1"   % "1.5.1"     % "test" withSources()
    case "2.9.0"   => "org.scalatest" % "scalatest_2.9.0"   % "1.6.1"     % "test" withSources()
    case "2.9.0-1" => "org.scalatest" % "scalatest_2.9.0-1" % "1.6.1"     % "test" withSources()
    case "2.9.1"   => "org.scalatest" % "scalatest_2.9.1"   % "1.6.1"     % "test" withSources()
    case v         => error("Unsupported Scala version " + v)
  }

  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar= defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)
  val docsArtifact = Artifact.javadoc(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)

  override def packageAction = super.packageAction dependsOn test
  
  override def managedStyle = ManagedStyle.Maven

  override def pomExtra =
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>loverdos</id>
        <name>Christos KK Loverdos</name>
      </developer>
    </developers>;

  override def repositories =
    if (version.toString.endsWith("-SNAPSHOT")) super.repositories + ScalaToolsSnapshots
    else super.repositories
  
  // Set up publish repository (the tuple avoids SBT's ReflectiveRepositories detection)
  private lazy val ScalaToolsReleases_t  = ("Scala Tools Releases"  -> "http://nexus.scala-tools.org/content/repositories/releases/")
  private lazy val ScalaToolsSnapshots_t = ("Scala Tools Snapshots" -> "http://nexus.scala-tools.org/content/repositories/snapshots/")

  lazy val publishTo =
    if (version.toString.endsWith("-SNAPSHOT")) {
      println("====> publishing SNAPSHOT: " + version)
      ScalaToolsSnapshots_t._1 at ScalaToolsSnapshots_t._2
    }
    else {
      println("====> publishing RELEASE: " + version)
      ScalaToolsReleases_t._1 at ScalaToolsReleases_t._2
    }

  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  lazy val publishRemote = propertyOptional[Boolean](false, true)
  
  private lazy val localDestRepo = Resolver.file("maven-local", Path.userHome / ".m2" / "repository" asFile)
  override def defaultPublishRepository =
    if (!publishRemote.value) Some(localDestRepo)
    else super.defaultPublishRepository

  lazy val projectInceptionYear       = "2010"

  private lazy val docBottom =
    "Copyright (c) Christos KK Loverdos. " +
      projectInceptionYear + "-" + Calendar.getInstance().get(Calendar.YEAR) +
      ". All Rights Reserved."
}
