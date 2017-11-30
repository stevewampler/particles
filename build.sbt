name := "particles"

version := "1.0"

scalaVersion := "2.12.4"

sbtVersion := "0.13.16"

// Required to retrieve Kafka 0.8.0-rc5 artifacts; once Kafka 0.8.0 is officially released
// you don't need to use the Apache Staging repo anymore.
resolvers += "Apache Staging" at "https://repository.apache.org/content/groups/staging/"

libraryDependencies ++= Seq(
  // The excludes of jms, jmxtools and jmxri are required as per https://issues.apache.org/jira/browse/KAFKA-974.
  // The exclude of slf4j-simple is because it overlaps with our use of logback with slf4j facade;  without the exclude
  // we get slf4j warnings and logback's configuration is not picked up.
//  "org.apache.kafka" % "kafka_2.10" % "0.8.1.1"
//    exclude("javax.jms", "jms")
//    exclude("com.sun.jdmk", "jmxtools")
//    exclude("com.sun.jmx", "jmxri")
//    exclude("org.slf4j", "slf4j-simple"),
  // Logback with slf4j facade
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "ch.qos.logback" % "logback-core" % "1.0.13",
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  "org.scala-lang.modules" %% "scala-swing" % "2.0.1",
  "com.typesafe.play" %% "play-json" % "2.6.7"
)
    