import sbt._
import Keys._

object SampleBuild extends Build {
    lazy val root = Project(id = "sample",
                            base = file(".")) aggregate(core, heap,numerical,touchdevelop,
                            scalapreproc,accessperm,partitioning,string,loops,tvla,
                            valuedrivenheap,gui,test)

    lazy val core = Project(id = "sample-core",
                           base = file("sample"))

    lazy val heap = Project(id = "sample-heap",
                           base = file("HeapAnalysis")) dependsOn(core)
    
    lazy val numerical = Project(id = "sample-numerical",
                           base = file("NumericalAnalysis")) dependsOn(core,heap)
    
    lazy val touchdevelop = Project(id = "sample-touchdevelop",
                           base = file("TouchDevelopPreprocessing")) dependsOn(core,heap,numerical)

    lazy val scalapreproc = Project(id = "sample-scala-preprocessing",
                           base = file("ScalaPreprocessing")) dependsOn(core)

    lazy val accessperm = Project(id = "sample-access-permissions",
                           base = file("AccessPermissionInference")) dependsOn(core,heap)
    
    lazy val partitioning = Project(id = "sample-partitioning",
                           base = file("Partitioning")) dependsOn(core,scalapreproc)
    
    lazy val string = Project(id = "sample-string",
                           base = file("String")) dependsOn(core)
    
    lazy val loops = Project(id = "sample-touchdevel-loops",
                           base = file("TouchDevelopLoops")) dependsOn(core,numerical,touchdevelop,heap)

    lazy val tvla = Project(id = "sample-tvla",
                           base = file("TVLA")) dependsOn(core,numerical)
    
    lazy val valuedrivenheap = Project(id = "sample-valuedriven-heap",
                           base = file("ValueDrivenHeapAnalysis")) dependsOn(core,numerical,scalapreproc)
    
    lazy val gui = Project(id = "sample-gui",
                           base = file("GUI")) dependsOn(core,numerical,heap,
                           touchdevelop,scalapreproc,accessperm,partitioning,
                           string,loops,tvla,valuedrivenheap)

    lazy val test = Project(id = "sample-test",
                           base = file("Test")) dependsOn(core,numerical,heap,
                           touchdevelop,scalapreproc,accessperm,partitioning,
                           string,loops,tvla,valuedrivenheap,gui)
}
