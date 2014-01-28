package ch.ethz.inf.pm.sample.abstractdomain.vdha

object ValueDrivenHeapProperty {
  var materialize = true

  // If set to true, materialization will not add any self-loops to the
  // materialized definite node, and also no edges from the original
  // summary node to the materialized node
  // THIS IS JUST A TEMPORARY PROPERTY TO ENABLE SOME EXPERIMENTS
  var materializeOnlyAcyclic = false
}
