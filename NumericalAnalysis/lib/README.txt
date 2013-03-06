check out apron from svn://scm.gforge.inria.fr/svnroot/apron/apron/trunk apron
copy Makefile.copy.model to Makefile.copy

comment HAS_OCAML, OCAMLFIND, set all prefix paths to /usr, remove
comment for HAS_JAVA

make
sudo make install

possibly go inside japron and do make install

move the jars from /usr to your lib directory

go to /usr/lib and create symbolic link from libapron.so.0 to
libapron.so (same for boxD, octD ect.)


