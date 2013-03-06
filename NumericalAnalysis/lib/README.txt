check out apron:
svn co svn://scm.gforge.inria.fr/svnroot/apron/apron/trunk apron
copy the Makefile.config in the /lib directory to the main directory of apron

install libgmp-dev and libmpfr-dev and g++ and maybe gccaptg
set properly JNIINC to the directory that contains jni.h


make
sudo make install

possibly go inside japron and do make install

move the jars from /usr to your lib directory

go to /usr/lib and create symbolic link from libapron.so.0 to
libapron.so (same for boxD, octD ect.) MAYBE


