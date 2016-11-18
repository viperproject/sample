How to compile APRON on your system
===================================

APRON will most likely only work on UNIX-based systems.

---- General procedure ----

(1) Check out APRON:

 svn co svn://scm.gforge.inria.fr/svnroot/apron/apron/trunk apron

(2) Copy the Makefile.config in this directory to the main directory of APRON

(3) Install gmp,mpfr,ppl development packages as well as gcc/g++ and a jdk (see below)

(4) Set properly JNIINC to the directory that contains jni.h

(5) Run:

    make
    sudo make install
    cd japron
    make
    sudo make install

(6) Move the jars from /usr to your lib directory

---- Ubuntu specific instructions ----

You have to install libgmp-dev and libmpfr-dev and g++ and maybe gccaptg before compilation

After compilation and installation, go to /usr/lib and create symbolic link from libapron.so.0 to
libapron.so (same for boxD, octD ect.)

---- Red hat / Fedora specific instructions: ----

To compile APRON, you will need:

yum install mpfr-devel
yum install gmp-devel
yum install gcc
yum install gcc-c++
yum install ppl-devel
yum install java-1.6.0-ibm-devel

Make sure you use the Makefile.config in this directory and set the following JNIINC path:

JNIINC    = -I/usr/lib/jvm/java/include -I/usr/lib/jvm/java/include/linux

You will have to move some of the libraries to the lib64 directory. The full sequence of commands for me was:

make
make install
cd japron
make
make install
cd /usr
rm *.jar
cd /usr/lib
cp libap* libbox* libitv* liboct* libpol* libt1p* *.idl ../lib64/

