# Sample

[![Build Status](https://pmbuilds.inf.ethz.ch/buildStatus/icon?job=sample)](https://pmbuilds.inf.ethz.ch/job/sample/)

Sample (Static Analyzer of Multiple Programming Languages) is a generic static analyzer based on the based on the [abstract interpretation](http://en.wikipedia.org/wiki/Abstract_interpretation) theory.

*This readme is currently being updated and still incomplete.*

## Module Overview

 * **sample_core:** The core of sample containing the statements of the intermediate language and their semantics, some basic implementation of abstract states.
 
 * **sample_numerical:** Numerical analyses, both non-relational and relational.
 
 * **sample_apron:** Provides an interface to the numerical domains provided by the [Apron](http://apron.cri.ensmp.fr/library/) library.
 
 * **sample_silver:** The part of sample that is specific to [Viper](https://www.pm.inf.ethz.ch/research/viper.html), which includes translations form and to the intermediate language used by Viper and some implementations of Viper specific analyses.
 
 * **sample_qp:** The implementation of the [Permission Inference for Array Programs](https://doi.org/10.1007/978-3-319-96142-2_7).

## Getting Started

### Prerequisites

 * [Mercurial](https://www.mercurial-scm.org/)

### Installation


 * Clone the Sample repository (this repository).
 
   ```
   hg clone https://bitbucket.org/viperproject/sample
   ```
  
 * Clone the [Silver](https://bitbucket.org/viperproject/silver) repository.
 
   ```
   hg clone https://bitbucket.org/viperproject/silver
   ```

 * Clone the [Silicon](https://bitbucket.org/viperproject/silicon) repository.
 
    ```
    hg clone https://bitbucket.org/viperproject/silicon
    ```

 * Clone the [Carbon](https://bitbucket.org/viperproject/carbon) repository.
 
   ```
   hg clone https://bitbucket.org/viperproject/carbon
   ```
   
 * From within the directory where you installed Sample, create symbolic links to the directories where you installed Silver, Silicon, and Carbon.
 
   **Linux / Mac:**
   ```
   cd sample
   ln -s ../silver silver
   ln -s ../silicon silicon
   ln -s ../carbon carbon
   ```
   
   **Windows:**
   ```
   cd sample
   mklink /D silver ../silver
   mklink /D silicon ../silicon
   mklink /D carbon ../carbon
   ```

### Building and Using Apron

 * Libraries must be installed in a location that is accessible by `ld`. To separate these libraries from the package manager libraries, the default choice is `usr/local/lib` which is usually not scanned. It is therefore necessary to set the `LD_LIBRARY_PATH` environment variable before running Sample.

