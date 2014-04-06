Backward Anlaysis: Usage Instructions
=====================================

The abstract error investigation process for TouchBoost alarms is controlled by
the  values of the option parameters in `TouchAnalysisParameters`. The default
values of the case class arguments are used unless overridden (e.g. by a test
suite, see below).

Enabling Backward Anaysis
-------------------------
The option `enableBackwardAnalysis` in `BackwardParams` enables the backward
analysis. It is set to true in the committed version.

Test suites
-----------
All tests can be run on the command line with "sbt test". For general
instructions see "Getting Started with Sample" on the wiki. Also note that the
old Swing GUI was never used for the backward analysis. It probably does not
even run.

There are 3 automated test suites that were used to run the backward analysis
on  example scripts. Different options were set since not all of the
functionality is appropriate for all scripts:

1. `BackwardLocalTest`: Analyzes scripts in "backward_tests/local". The
   analysis uses the Polyhedra domain and reports numerical errors (bound
   violations etc.). The backward analysis computes the refined entry state
   at the etry of the method that contains the alarm..

   The recommended way to experiment with scripts is to run them in this test
   suite! Run the class `BackwardLocalTest` with argument "-n [scriptname]" to
   analyze only script [scriptname]. Note that the file name is enough (without
   path), but it must be in the test directory somewhere.

   Interesting tests:
    - motivatingExample
    - motivatingExampl_unmodified
    - pic3_v4_presentation
    - join_imprecision
    - wideningAlarm
    - battery_check
    - inter1_3
    - presentation_ex1

2. `BackwardLocalNonNumericTest`: Analyzes scripts in
   "backward_tests/local_nonnumeric". This comprehends the "PLDI random" test
   scripts. I turned the reporting of numerical errors off and use the
   standard Octagon domain, so that the forward analysis alarms  are comparable
   to the ones in the manual PLDI evaluation.

   Interesting tests:
    - aanja
    - aaoweirj
    - aaib
    - aaeq
    - aaub
    - aauh
    

3. `BackwardInterprocTest`: Analyzes scripts in
   "backward_tests/interprocedural". Some scripts such as the interprocedural
   detection of false alarms require the backward analysis to go beyond the
   method with the alarm. This feature is quite experimental and does not always
   yield good results. It is therefore only enabled in this test suite. 
   (see the flag `backwardInterprocAnalysis` in TouchAnalysisParameters.
   
   Interesting tests:
    - interproc_false_alarm
    - two_public
    - private_method

NOTE: Again, to reproduce the result of a test script (as used in the report) or a
modification thereof, the script should be run using a test suite with the
argument "-n [scriptname]" where [scriptname] is replaced with the script's
file name (with or without suffix).

An additional test suite `InterpreterTestSuite` contains some tests for the concrete interpreter. It does not make use of any forward/backward analysis.

Analysing single script outside test suite
------------------------------------------
A local script can also be manually analyzed outside the test suites by running
`TouchApronRun` and passing its path as the argument. Note that this uses the
default options of TouchBoost (e.g. Octagon domain) and does not report
numerical bound violations. It is not recommended to run scripts this way, but
it should work if the desired TouchAnalysisParameters values are set by hand.


