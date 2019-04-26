/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.analysis

import java.nio.file.Paths

import ch.ethz.inf.pm.sample.abstractdomain.{AccessPathIdentifier, Expression, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution.{CfgResult, ProgramResult}
import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.oorepresentation.silver.{RefType, SilverIdentifier}
import ch.ethz.inf.pm.sample.test.LatticeTest
import org.scalatest.FunSuite

class AliasAnalysisTest extends FunSuite {
  type S = SimpleAliasAnalysisState

  val directory = "silver/alias_analysis/"

  test("issue_87") {
    val result = analyze("issue_87", "foo")
    val state = result.exitState()

    assert(mayAlias(state, "a.f", "b"))
    assert(!mustAlias(state, "a.f", "b"))
  }

  def analyze(file: String, method: String): CfgResult[S] = {
    val identifier = SilverIdentifier(method)
    val results = analyze(file)
    results.getResult(identifier)
  }

  def analyze(filename: String): ProgramResult[S] = {
    val resource = getClass.getClassLoader.getResource(directory + filename + ".sil")
    assert(resource != null, s"File $directory$filename.sil not found")
    val path = Paths.get(resource.toURI)
    val compilable = Compilable.Path(path)
    AliasAnalysis.run(compilable)
  }

  /**
    * A helper function to check whether the two given access paths may alias in
    * the given state.
    *
    * @param state  The state.
    * @param first  The first access path.
    * @param second The second access path.
    * @return True if the given access paths may alias.
    */
  def mayAlias(state: S, first: String, second: String): Boolean =
    state.mayAlias(access(first), access(second))

  /**
    * A helper function to check whether the two given access paths must alias
    * in the given state.
    *
    * @param state  The state.
    * @param first  The first access path.
    * @param second The second access path.
    * @return True if the given access paths must alias.
    */
  def mustAlias(state: S, first: String, second: String): Boolean =
    state.mustAlias(access(first), access(second))

  def access(string: String): Expression = {
    val list = string.split("\\.").toList.map(x => VariableIdentifier(x)(RefType()))
    if (list.length == 1) list.head
    else AccessPathIdentifier(list)
  }
}

/**
  * Property-based testing of lattice elements for Alias Analysis.
  *
  * @author Caterina Urban
  */
class AliasAnalysisLatticeTest extends LatticeTest[SimpleAliasAnalysisState] {
  override def factory: SimpleAliasAnalysisState = AliasAnalysisEntryStateBuilder().default
}
