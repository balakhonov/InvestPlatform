
organization := "balakhonov.com"
name := "invest-platform"
version := "2.07"

lazy val `investplatform` = (project in file("."))
  .enablePlugins(PlayScala, sbtdocker.DockerPlugin, JavaAppPackaging)

resolvers += "Akka Snapshot Repository" at "https://repo.akka.io/snapshots/"

scalaVersion := "2.13.5"

// include local Maven repo
resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(evolutions, jdbc, ehcache, ws, specs2 % Test, guice)
libraryDependencies += "com.h2database" % "h2" % "1.4.198"
libraryDependencies += "net.codingwell" %% "scala-guice" % "5.0.2"
libraryDependencies ++= Dependencies.functionalDependencies
libraryDependencies ++= Dependencies.tinkoffInvestDependencies
libraryDependencies ++= Dependencies.databaseDependencies
// https://mvnrepository.com/artifact/me.tongfei/progressbar
libraryDependencies += "me.tongfei" % "progressbar" % "0.9.2"
// https://mvnrepository.com/artifact/org.apache.commons/commons-math3
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

docker / imageNames := Seq(
  // Sets the latest tag
  ImageName(s"cr.yandex/crp2go3gbfj6e6un4boe/invest:v${version.value}"),

  // Sets a name with a tag that contains the project version
  //  ImageName(
  //    namespace = Some(organization.value),
  //    repository = name.value,
  //    tag = Some("v" + version.value)
  //  )
)

// ssh balakhonov@178.154.226.9 "docker logs 0f68b7b907be"
docker / dockerfile := {
  val projectDir: File = baseDirectory.value

  new Dockerfile {
    from("openjdk:8-jre")
    label("container_name", "invest-platform")

    cmdRaw("""bash -c "/usr/share/invest-platform/bin/invest-platform -Dpidfile.path=/dev/null"""")

    copy(projectDir / "target/universal/stage", "/usr/share/invest-platform", chown = "daemon:daemon")
    copy(projectDir / "docker/conf/application.conf", "/etc/invest-platform/production.conf", chown = "daemon:daemon")
    copy(projectDir / "docker/cert/cacerts", "/etc/invest-platform/cacerts", chown = "daemon:daemon")

    expose(9200)
  }
}

docker / buildOptions := BuildOptions(
  cache = false,
  removeIntermediateContainers = BuildOptions.Remove.Always
)
      