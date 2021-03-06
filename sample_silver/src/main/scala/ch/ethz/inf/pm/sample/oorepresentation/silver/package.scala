/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.{oorepresentation => rep, abstractdomain => dom}

package object silver {

  /**
   * Makes it possible to access many different Sample types and values
   * under a single namespace.
   *
   * In code where Sample, Silver and Silicon types are mixed, a consistent
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
    val FieldAccessPredicate = dom.FieldAccessPredicate
    val CurrentPermission = dom.CurrentPermission

    // Object-oriented representation
    type ProgramPoint = rep.ProgramPoint
    type Type = rep.Type
    type ClassDefinition = rep.ClassDefinition
    type FunctionDeclaration = rep.silver.SilverFunctionDeclaration
    type MethodDeclaration = rep.silver.SilverMethodDeclaration
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

    // Simplon-specific types and values
    type RefType = rep.silver.RefType

    val BottomType = rep.silver.BottomType
    val BoolType = rep.silver.BoolType
    val IntType = rep.silver.IntType
    val RefType = rep.silver.RefType
    val PermType = rep.silver.PermType
    val DomType = rep.silver.DomType
    val TopType = rep.silver.TopType

    val WrappedProgramPoint = rep.silver.WrappedProgramPoint
    val PackageIdentifier = rep.DummyPackageIdentifier
    val ClassIdentifier = rep.DummyClassIdentifier
    val MethodIdentifier = rep.silver.SilverIdentifier
  }

}