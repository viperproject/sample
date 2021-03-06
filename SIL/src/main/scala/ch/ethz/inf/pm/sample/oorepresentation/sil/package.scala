/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.{oorepresentation => rep, abstractdomain => dom}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDefinitionsDomain

package object sil {

  /**
   * Makes it possible to access many different Sample types and values
   * under a single namespace.
   *
   * In code where Sample, SIL and Silicon types are mixed, a consistent
   * prefix is useful.
   */
  object sample {
    // Abstract domain
    type Expression = dom.Expression
    type Identifier = dom.Identifier
    type VariableIdentifier = dom.VariableIdentifier

    val NegatedBooleanExpression = dom.NegatedBooleanExpression
    val BinaryBooleanExpression = dom.BinaryBooleanExpression
    val ReferenceComparisonExpression = dom.ReferenceComparisonExpression
    val BinaryArithmeticExpression = dom.BinaryArithmeticExpression
    val UnaryArithmeticExpression = dom.UnaryArithmeticExpression
    val Constant = dom.Constant
    val VariableIdentifier = dom.VariableIdentifier
    val EmptyScopeIdentifier = dom.EmptyScopeIdentifier
    val BooleanOperator = dom.BooleanOperator
    val ArithmeticOperator = dom.ArithmeticOperator
    val AccessPathIdentifier = dom.AccessPathIdentifier

    // Object-oriented representation
    type ProgramPoint = rep.ProgramPoint
    type Type = rep.Type
    type ClassDefinition = rep.ClassDefinition
    type MethodDeclaration = rep.MethodDeclaration
    type FieldDeclaration = rep.FieldDeclaration
    type Statement = rep.Statement
    type ControlFlowGraph = rep.ControlFlowGraph
    type VariableDeclaration = rep.VariableDeclaration

    val VariableDeclaration = rep.VariableDeclaration
    val EmptyStatement = rep.EmptyStatement
    val ConstantStatement = rep.ConstantStatement
    val FieldAccess = rep.FieldAccess
    val Variable = rep.Variable
    val New = rep.New
    val Assignment = rep.Assignment
    val MethodCall = rep.MethodCall
    val DummyProgramPoint = rep.DummyProgramPoint
    val StaticModifier = rep.StaticModifier
    val PureModifier = rep.PureModifier

    // Value-driven heap analysis
    val ValueHeapIdentifier = dom.vdha.ValueHeapIdentifier

    // Simplon-specific types and values
    type RefType = rep.sil.RefType

    val BoolType = rep.sil.BoolType
    val BottomType = rep.sil.BottomType
    val IntType = rep.sil.IntType
    val RefType = rep.sil.RefType
    val TopType = rep.sil.TopType

    val WrappedProgramPoint = rep.sil.WrappedProgramPoint
    val PackageIdentifier = rep.DummyPackageIdentifier
    val ClassIdentifier = rep.DummyClassIdentifier
    val MethodIdentifier = rep.DummyMethodIdentifier

    // Simplon-specific abstract domains
    type PredicatesDomain = dom.vdha.PredicateDefinitionsDomain
    type PredicateBody = dom.vdha.PredicateBody
    type NestedPredicatesDomain = dom.vdha.NestedPredicatesDomain
    type PredicateIdentifier = dom.vdha.PredicateIdentifier

    val PredicatesDomain = dom.vdha.PredicateDefinitionsDomain
    val PredicateBody = dom.vdha.PredicateBody
    val NestedPredicatesDomain = dom.vdha.NestedPredicatesDomain
    val PredicateIdentifier = dom.vdha.PredicateIdentifier
  }

}