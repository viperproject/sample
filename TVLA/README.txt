Configuration of TVSHeap - TVLA Heap Domain
-------------------------------------------

[[[ IntelliJ configuration ]]]
My config is shown in the screenshots under doc/
Most important settings:
1. Include the directory containing tvla.properties as a source folder.
2. Create dependencies for all .jar files in the lib/ folder 
2. Module dependencies (e.g Sample core)

[[[ Flags to configure the analysis ]]]

1) TVLARunner.launchJVM
If set to true, it uses a JVM each time to execute TVLA.
This is more stable but much slower.
By default, this is false.

2) TVLARunner.cacheResults
If set to true, cache results of calls to TVLA.
Compensates for the inefficiency of the CFG iteration.
By default, this is true.

3) TVLALogger.enabledLog
If set to true, the domain generates a log of every call to TVLA.
Each call is output in a subdirectory of the working directory.
It is quite low-level and for debugging only.
By default, this is false.


[[[ Main+reset invocation method (default) ]]]
The libraries of the customized TVLA must be in the classpath.
They are contained in lib/.
The file tvla.properties must be available as a resource at runtime.
(Note: the directory containing this file must be marked as a source folder in IntelliJ. By default it is in test/resources, but this is not necessary.)


[[[JVM invocation method method  ]]]
2 environment variables must be specified for this to run!

1) TVLA_HOME should contain the top directory of the customized TVLA version.
   for me on linux this works with:
	export TVLA_HOME=/home/rf/eth/bachelorthesis/TVSHeap/tvla3custom/

   The directory tvla3custom is contained in the deliverables of the thesis.

2) PATH must contain the "bin" directory under the TVLA_HOME directory.
   The reason is that there are some scripts to execute the JVM for TVLA
   (shell script for linux, .bat file for windows).

   On linux this can be done with:
   	export PATH=$PATH:$TVLA_HOME/bin

   In windows the PATH can be edited similarly (new login may be required 
   for it to take effect).

   	
