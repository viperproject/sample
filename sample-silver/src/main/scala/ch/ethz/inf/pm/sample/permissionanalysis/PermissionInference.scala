/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, VariableIdentifier}
import ch.ethz.inf.pm.sample.execution.{CfgResult, SilverAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverInferenceRunner, TopType}
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.SimpleAliasAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisTypes.AccessPath
import ch.ethz.inf.pm.sample.permissionanalysis.util.Permission.Fractional
import ch.ethz.inf.pm.sample.permissionanalysis.util.{Context, Permission, PermissionStack, PermissionTree}
import viper.silver.ast.Method
import viper.silver.{ast => sil}

trait PermissionInferenceRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
  extends SilverInferenceRunner[PermissionStack, T] {
  /**
    * The flag indicating whether there is a read permission mentioned in the
    * specifications.
    */
  private var read: Boolean = false

  /**
    * Extends the given method using the given result of the analysis.
    *
    * @param method    The method to extend.
    * @param cfgResult The result of the analysis.
    * @return The extended program.
    */
  override def extendMethod(method: Method, cfgResult: CfgResult[T]): Method = {
    // reset read flag and extend method
    read = false
    val extended = super.extendMethod(method, cfgResult)

    if (read) {
      // add read permission to arguments
      val argument = Seq(sil.LocalVarDecl("read", sil.Perm)())
      val arguments = (extended.formalArgs ++ argument).distinct
      // add constraint for read permission to precondition
      val variable = sil.LocalVar("read")(sil.Perm)
      val condition = Seq(sil.And(sil.PermLtCmp(sil.NoPerm()(), variable)(), sil.PermLtCmp(variable, sil.FullPerm()())())())
      val preconditions = condition ++ extended.pres
      // update method
      extended.copy(
        _pres = preconditions,
        formalArgs = arguments
      )(extended.pos, extended.info)
    } else extended
  }

  /**
    * Modifies the list of preconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing preconditions.
    * @param state    The state providing the specifications.
    * @return The modified list of preconditions.
    */
  override def preconditions(existing: Seq[sil.Exp], state: T): Seq[sil.Exp] =
    extendSpecifications(existing, state)

  /**
    * Modifies the list of postconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing postconditions.
    * @param state    The state providing the specifications.
    * @return The modified list of postconditions.
    */
  override def postconditions(existing: Seq[sil.Exp], state: T): Seq[sil.Exp] =
    extendSpecifications(existing, state)

  /**
    * Modifies the list of invariants using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing invariants.
    * @param state    The state providing the specifications.
    * @return The modified list of invariants.
    */
  override def invariants(existing: Seq[sil.Exp], state: T): Seq[sil.Exp] = {
    /*val stack = state.specifications
    val tree = stack.foldLeftTrees(PermissionTree.empty)(_ lub _)
    val paths = stack.headPaths.filter(tree.extract(_)._2.nonEmpty())

    println(s"paths: $paths")*/

    extendSpecifications(existing, state)
  }

  /**
    * Modifies the list of fields of a new statement ussing specifications
    * provided by the given state.
    *
    * @param existing The existing list of fields.
    * @param state    The state providing the specifications.
    * @return The modified list of fields.
    */
  override def fields(existing: Seq[sil.Field], state: T): Seq[sil.Field] = {
    // extract specifications from state
    val specifications = state.specifications.foldLeftTrees[PermissionTree](PermissionTree.Bottom)(_ lub _)

    // TODO: Report if infeasible permissions are inferred.

    val inferred = specifications match {
      case tree: PermissionTree.Inner =>
        tree.children.flatMap { case (field, subtree) =>
          val permission = subtree.permission()
          if (permission.isNone) None
          else Some(createSilverField(field, state))
        }
    }

    (existing ++ inferred).distinct
  }

  /**
    * Extends the list of expressions using specifications provided by the given
    * state.
    *
    * @param existing The list of expressions.
    * @param state    The state providing the specifications.
    * @return The extended list of expressions.
    */
  private def extendSpecifications(existing: Seq[sil.Exp], state: T): Seq[sil.Exp] = {
    val inferredSpecifications = state.specifications
    val (existingSpecifications, unknown) = extractSpecifications(existing, state)
    val specifications = inferredSpecifications.headTree plus existingSpecifications

    read = read || specifications.fold(false) {
      case (result, (_, tree)) =>
        result || (tree.permission() match {
          case Fractional(_, _, rd) => rd > 0
          case _ => false
        })
    }

    createSilverSpecifications(specifications, state) ++ unknown
  }

  /**
    * Extracts permissions from the given expressions.
    *
    * @param expressions The expressions.
    * @param state       The state storing additional information.
    * @return A tuple containing the extracted permissions and the unknown part of the specifications.
    */
  private def extractSpecifications(expressions: Seq[sil.Exp], state: T): (PermissionTree, Seq[sil.Exp]) =
    expressions.foldLeft((PermissionTree.empty, Seq.empty[sil.Exp])) {
      case ((specs, unknown), expression) =>
        val (s, u) = extractPermissions(expression, state)
        (specs plus s, unknown ++ u)
    }

  /**
    * Extracts permissions from the given expression.
    *
    * @param expression The expression.
    * @param state      The state storing additional information.
    * @return A tuple containing the extracted permissions and the unknown
    *         part of the specifications.
    */
  private def extractPermissions(expression: sil.Exp, state: T): (PermissionTree, Seq[sil.Exp]) = expression match {
    case sil.And(left, right) =>
      val (lSpecs, lUnknown) = extractPermissions(left, state)
      val (rSpecs, rUnknown) = extractPermissions(right, state)
      (lSpecs plus rSpecs, lUnknown ++ rUnknown)
    case sil.FieldAccessPredicate(location, permission) =>
      // helper function to convert an expression into an access path
      def accessPath(expression: sil.Exp): AccessPath = expression match {
        case sil.LocalVar(name) =>
          List(VariableIdentifier(name)(DummyRefType))
        case sil.FieldAccess(receiver, field) =>
          val typ = Context.getField(field.name).map(_.typ).getOrElse(TopType)
          accessPath(receiver) :+ VariableIdentifier(field.name)(typ)
      }

      // helper function to convert an expression into a permission
      def permissionAmount(expression: sil.Exp): Permission = expression match {
        case sil.NoPerm() =>
          Permission.none
        case sil.FullPerm() =>
          Permission.write
        case sil.FractionalPerm(sil.IntLit(numerator), sil.IntLit(denominator)) =>
          Permission.fractional(numerator.toInt, denominator.toInt)
        case sil.LocalVar(name) if name == "read" =>
          Permission.read
        case sil.PermAdd(left, right) =>
          permissionAmount(left) plus permissionAmount(right)
      }

      val path = accessPath(location)
      val amount = permissionAmount(permission)

      (PermissionTree.create(path, amount), Seq.empty)
    case unknown => (PermissionTree.empty, Seq(unknown))
  }

  /**
    * Creates a list of Silver specifications from the given permission tree.
    *
    * @param specifications The given permission tree.
    * @param state          The stater storing additional information.
    * @return A list of Silver specifications.
    */
  private def createSilverSpecifications(specifications: PermissionTree, state: T): Seq[sil.Exp] = {
    val framed = specifications.map() { case (_, tree) =>
      val permission = tree.permission()
      if (permission.isSome || tree.isEmpty) permission
      else Permission.read
    }
    framed.tuples().flatMap {
      case (location, permission) if location.length >= 2 =>
        Some(createSilverFieldAccessPredicate(location, permission, state))
      case _ => None
    }
  }

  /**
    * Creates a Silver field access predicate for the given location with the
    * given amount of permission.
    *
    * @param location   The access path representing the location.
    * @param permission The amount of permission.
    * @param state      The state storing additional information.
    * @return A Silver field access predicate.
    */
  private def createSilverFieldAccessPredicate(location: AccessPath, permission: Permission, state: T): sil.FieldAccessPredicate = {
    val fieldAccess = createSilverFieldAccess(location, state)
    val permissionAmount = createSilverPermissionAmount(permission, state)
    sil.FieldAccessPredicate(fieldAccess, permissionAmount)()
  }

  /**
    * Crates a Silver field access corresponding to the given access path.
    *
    * @param path  The access path.
    * @param state The state storing additional information.
    * @return A Silver field access corresponding to the given access path.
    */
  private def createSilverFieldAccess(path: AccessPath, state: T): sil.FieldAccess = {
    val receiver =
      if (path.length == 2) sil.LocalVar(path.head.getName)(sil.Ref)
      else createSilverFieldAccess(path.init, state)
    val field = path.last
    val typ = createSilverType(field, state)
    sil.FieldAccess(receiver, sil.Field(field.getName, typ)())()
  }

  /**
    * Creates a Silver expression representing a permission amount corresponding
    * to the given permission.
    *
    * @param permission The permission.
    * @param state      The state storing additional information.
    * @return A Silver expression representing a permission amount.
    */
  private def createSilverPermissionAmount(permission: Permission, state: T): sil.Exp = permission match {
    case Fractional(numerator, denominator, rd) =>
      val fractionalAmount =
        if (numerator == 0) None
        else if (numerator == denominator) Some(sil.FullPerm()())
        else Some(sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())())
      val readAmount =
        if (rd == 0) None
        else if (rd == 1) Some(sil.LocalVar("read")(sil.Perm))
        else Some(sil.PermMul(sil.IntLit(rd)(), sil.LocalVar("read")(sil.Perm))())

      (fractionalAmount, readAmount) match {
        case (Some(left), Some(right)) => sil.PermAdd(left, right)()
        case (Some(left), None) => left
        case (None, Some(right)) => right
        case (None, None) => sil.NoPerm()()
      }
  }

  /**
    * Creates a Silver field corresponding to the given identifier.
    *
    * @param identifier The identifier.
    * @param state      The state storing additional information.
    * @return A Silver field corresponding to the given identifier.
    */
  private def createSilverField(identifier: Identifier, state: T): sil.Field = {
    val name = identifier.toString
    val typ = createSilverType(identifier, state)
    sil.Field(name, typ)()
  }

  /**
    * Creates a Silver type corresponding to the given identifier.
    *
    * @param identifier The identifier
    * @param state      The state storing additional information.
    * @return A Silver type corresponding to the given identifier.
    */
  private def createSilverType(identifier: Identifier, state: T): sil.Type = {
    val name = identifier.toString
    Context.getField(name).map(_.typ) match {
      case Some(typ) if typ.isObject => sil.Ref
      case Some(typ) if typ.isNumericalType => sil.Int
      case Some(typ) if typ.isBooleanType => sil.Bool
      case None => throw new IllegalArgumentException(s"Type $identifier does not exist.")
    }
  }
}

object PermissionInference
  extends PermissionInferenceRunner[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
  override val analysis: SilverAnalysis[SimplePermissionAnalysisState] = PermissionAnalysis(AliasAnalysisEntryState, PermissionAnalysisEntryState)
}
