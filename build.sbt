import sbtrelease._
import ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

def gitHash(): String = sys.process.Process("git rev-parse HEAD").lineStream_!.head

val tagName = Def.setting {
  s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value
  else version.value}"
}
val tagOrHash = Def.setting {
  if (isSnapshot.value) gitHash() else tagName.value
}
val Scala212 = "2.12.6"

lazy val commonSettings = nocomma {
  scalaVersion := Scala212
  crossScalaVersions := Seq(Scala212)
  organization := "com.github.xuwei-k"
  homepage := Some(url("https://github.com/xuwei-k/scalaz-magnolia"))
  licenses := Seq("MIT License" -> url("https://opensource.org/licenses/mit-license"))
  pomExtra :=
    <developers>
      <developer>
        <id>xuwei-k</id>
        <name>Kenji Yoshida</name>
        <url>https://github.com/xuwei-k</url>
      </developer>
    </developers>
    <scm>
      <url>git@github.com:xuwei-k/scalaz-magnolia.git</url>
      <connection>scm:git:git@github.com:xuwei-k/scalaz-magnolia.git</connection>
      <tag>{tagOrHash.value}</tag>
    </scm>
  publishTo := sonatypePublishTo.value
  releaseTagName := tagName.value
  releaseCrossBuild := true
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    UpdateReadme.updateReadmeProcess,
    tagRelease,
    ReleaseStep(
      action = { state =>
        val extracted = Project extract state
        extracted.runAggregated(PgpKeys.publishSigned in Global in extracted.get(thisProjectRef), state)
      },
      enableCrossBuild = true
    ),
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    UpdateReadme.updateReadmeProcess,
    pushChanges
  )
}

lazy val scalazMagnolia = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure) // https://github.com/lucidsoftware/neo-sbt-scalafmt/issues/59
  .in(file("."))
  .settings(
    commonSettings,
    scalapropsCoreSettings
  )
  .settings(nocomma {
    name := UpdateReadme.scalazMagnoliaName
    description := "generate scalaz instances with magnolia"
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:experimental.macros",
      "-unchecked",
      "-Xlint:infer-any",
      "-Xlint:missing-interpolator",
      "-Xlint:nullary-override",
      "-Xlint:nullary-unit",
      "-Xlint:private-shadow",
      "-Xlint:stars-align",
      "-Xlint:type-parameter-shadow",
      "-Xlint:unsound-match",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture"
    )
    scalapropsVersion := "0.5.5"
    libraryDependencies ++= Seq(
      "com.propensive" %%% "magnolia" % "0.10.0",
      "org.scalaz" %%% "scalaz-core" % "7.2.26",
      "com.github.scalaprops" %%% "scalaprops-scalazlaws" % scalapropsVersion.value % "test",
      "com.github.scalaprops" %%% "scalaprops" % scalapropsVersion.value % "test",
      "com.github.scalaprops" %%% "scalaprops-magnolia" % "0.1.2" % "test"
    )
    scalacOptions in (Compile, doc) ++= {
      val tag = tagOrHash.value
      Seq(
        "-sourcepath",
        (baseDirectory in LocalRootProject).value.getAbsolutePath,
        "-doc-source-url",
        s"https://github.com/xuwei-k/scalaz-magnolia/tree/${tag}€{FILE_PATH}.scala"
      )
    }
  })
  .jsSettings(
    scalacOptions += {
      val a = (baseDirectory in LocalRootProject).value.toURI.toString
      val g = "https://raw.githubusercontent.com/xuwei-k/scalaz-magnolia/" + tagOrHash.value
      s"-P:scalajs:mapSourceURI:$a->$g/"
    }
  )

val jvm = scalazMagnolia.jvm.withId("jvm")
val js = scalazMagnolia.js.withId("js")

lazy val notPublish = nocomma {
  publishArtifact := false
  publish := {}
  publishLocal := {}
  PgpKeys.publishSigned := {}
  PgpKeys.publishLocalSigned := {}
}

commonSettings
notPublish
name := "root"
sources in Compile := Nil
sources in Test := Nil
