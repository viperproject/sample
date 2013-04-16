In order to get the Loop Cost Analysis running, you need to

- install Apron

- install PUBS:

Url: https://costa.ls.fi.upm.es/svn/Systems/costa/trunk
Login: pierto.ferrara.public
Password: F6craBR8


This will checkout both costa and pubs (the solver). You need to
have installed csh and libppl9.

*** INSTALLING COSTA/PPL-0.11.2/SWI ON MAC 10.7

You need macports installed and working; I did this under tcsh

(1) run "sudo port install swi-prolog"

this should install from macports SWI-Prolog (Multi-threaded, 64 bits,
Version 5.10.5)

(2) extract /lib/ppl-0.11.2.tar.bz2 

(3) go to the newly created directory

(4) run "./configure --prefix=/usr/local --with-gmp-prefix=/opt/local --enable-interfaces=swi_prolog" (this could require to install m4)

(5) run "make"

(6) run "sudo make install"

(7) in my case, there was a permission issue with the directory
/usr/local/lib/ppl, which was not readable; I fixed it by running
"chmod 755 /usr/local/lib/ppl"

(8) install COSTA:
 1.  run make in the main directory, this will install costa and pubs,
and it will modify your .cshrc or .bashrc.
 2.  run make also in trunk/pubs/interfaces/shell to create a
shell-script that called pubs.
 3.  copy pubs_shell from trunk/pubs/interfaces/shell to Sample/trunk


- add the module TouchDevelopLoops to Sample (contains all my code)

- in class TouchAnalysis, in method analyze, add the following lines directly after val s = x.forwardSemantics[S](entryState):

        val analysis = new LoopCostAnalysis[S](x, s, new ParameterizedCostModel()) 
        analysis.run()
        for ((programPoint, answer) <- analysis.result) {
          println("loop at program point "+programPoint+": "+answer)
        }

- in class TouchCompiler, in method, compile file, replace the last line (the return value) by
	Augment.augment(compileString(source.getLines().mkString("\n"),pubID));

NOTE: in Linux, you have to run IntelliJ as administrator (sudo ./idea.sh) otherwise the analysis gets stuck.
