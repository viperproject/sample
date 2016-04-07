


**Sample** (Static Analyzer of Multiple Programming LanguagEs) is a generic static analyzer
based on the [abstract interpretation](http://en.wikipedia.org/wiki/Abstract_interpretation) theory.
This analyzer is generic with respect to:

* the **heap analysis** that approximates all runtime heap structures
  (we have already implemented some common analyses, and the interface of [TVLA](http://www.cs.tau.ac.il/~tvla/)),
* the **semantic domain** that infers the property of interest
  (that can be about numerical information like [Apron](http://apron.cri.ensmp.fr/library/), string values, types, ...),
* the analyzed **language** since Sample works on an intermediate language called Simple
  (we have already developed the translation from [Scala](http://www.scala-lang.org/) and [TouchDevelop](https://www.touchdevelop.com/) to Simple).

Sample is the engine that infers specification in [Semper](http://www.pm.inf.ethz.ch/research/semper).

If you are new to Sample, take a look at the [Wiki](https://bitbucket.org/viperproject/sample/wiki) to get started.

**Build Status**

[![Build Status](https://pmbuilds.inf.ethz.ch/buildStatus/icon?job=sample)](https://pmbuilds.inf.ethz.ch/job/sample/)

