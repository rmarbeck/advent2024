val scala3Version = "3.3.4"

lazy val advent2018 = (project
  .in(file("."))
  .settings(
    name := "advent of code 2024",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  ))
  .aggregate(projects*)
  .dependsOn(classPaths*)

lazy val dirs = new File(".").listFiles().filter(_.isDirectory).filter(_.name.contains("day"))
val subprojects = new CompositeProject {
  override def componentProjects: Seq[Project] = dirs.map { p =>
    Project(s"advent${p.getName}", p)
  }
}

val projects = subprojects.componentProjects.map(m => m : ProjectReference)
val classPaths = subprojects.componentProjects.map(m => m : ClasspathDependency)

