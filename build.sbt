name := "acs-commons"

organization := "cc.acs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

libraryDependencies ++=  Seq(
  "org.scalaz"            %% "scalaz-core"       %  "6.0.4",
  "commons-io"            %  "commons-io"        %  "2.0.1",
  "org.specs2"            %% "specs2"            %  "1.7.1"  %  "test",
  "org.jdom"              %  "jdom"              %  "1.1",
  "net.databinder"        %% "dispatch-http"     %  "0.8.7",
  "net.databinder"        %% "dispatch-core"     %  "0.8.7",
  "cc.acs"                %% "boxter-brown"      %  "0.1-SNAPSHOT"
)

// "cc.spray"              %% "spray-json"        %  "1.1.0",
// "org.codehaus.jettison" %  "jettison"          %  "1.3",
// "org.neo4j"             %  "neo4j"             %  "1.6",
// "javax.servlet"         %  "servlet-api"       %  "2.5"    %  "provided",

// set Ivy logging to be at the highest level
ivyLoggingLevel := UpdateLogging.Full

// set the prompt (for this build) to include the project id.
shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// set the prompt (for the current project) to include the username
shellPrompt := { state => System.getProperty("user.name") + "> " }

// disable printing timing information, but still print [success]
showTiming := false

// disable using the Scala version in output paths and artifacts
// crossPaths := false

// fork a new JVM for 'run' and 'test:run'
// fork := true

// add a JVM option to use when forking a JVM for 'run'
// javaOptions += "-Xmx2G"

// only use a single thread for building
parallelExecution := true

// only show warnings and errors on the screen for all tasks (the default is Info)
logLevel := Level.Info

// only store messages at info and above (the default is Debug)
//   this is the logging level for replaying logging with 'last'
persistLogLevel := Level.Debug

// only show 10 lines of stack traces
// traceLevel := 10

// only show stack traces up to the first sbt stack frame
traceLevel := 0

publishTo <<= (version) {version: String => {
  def repo(name: String) = name at "http://iesl.cs.umass.edu:8081/nexus/content/repositories/" + name
  val isSnapshot = version.trim.endsWith("SNAPSHOT")
  val repoName = if (isSnapshot) "snapshots" else "releases"
  Some(repo(repoName)) }}

credentials += {
  Seq("build.publish.user", "build.publish.password").map(k => Option(System.getProperty(k))) match {
    case Seq(Some(user), Some(pass)) => Credentials("Sonatype Nexus Repository Manager", "iesl.cs.umass.edu", user, pass)
    case _ => Credentials(Path.userHome / ".ivy2" / ".credentials")
  }}


seq(lsSettings :_*)
