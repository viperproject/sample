/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation.scalalang

import scala.collection.mutable
import scala.tools.nsc._
import scala.tools.nsc.symtab._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import scala.language.postfixOps
import scala.tools.nsc.plugins.PluginComponent

class ScalaProgramPoint(pos : scala.reflect.internal.util.Position) extends LineColumnProgramPoint {
  var row : Int = try{
	  pos.line
  }
  catch {
	  case _: Throwable => -1
  }
  var column : Int  = try{
	  pos.column
  }
  catch {
	  case _: Throwable => -1
  }

  def this(r : Int, c : Int) ={
    this(null)
    row=r
    column=c
  }

  override def getColumn() = column

  override def getLine() = row

  override def equals(o : Any) : Boolean = o match {
    case x : ScalaProgramPoint => x.row==row && x.column==column;
    case _ => false;
  }

  override def toString : String = {
    var result : String =""
    if(row != -1)
      result=result+"line "+row+" "
    else result=result+"line unknown "
    if(column != -1)
      result=result+"column "+column+" "
    else result=result+"column unknown "
    result
  }
}


abstract class Named(name : String) {

  def getName() : String = name

  override def equals(o : Any) : Boolean = o match {
    case x : Named => x.getName().equals(name) && x.getClass().equals(this.getClass())
    case _ => false;
  }

  override def hashCode() : Int = name.hashCode()

  override def toString = name
}

class ScalaMethodIdentifier(name : String) extends Named(name) with MethodIdentifier

class ScalaClassIdentifier(val name : String, val thisType : Type) extends Named(name) with ClassIdentifier {
	def getThisType() = thisType
}

class ScalaPackageIdentifier(name : String, pack : PackageIdentifier) extends Named(name) with PackageIdentifier



object Performances {
  var transformationTime : Long = 0
  var classes : Long = 0
}

class ScalaProgramToControlFlowGraph(val global: Global) extends PluginComponent {
  import global._

  override val runsAfter = "dce" :: Nil

  val phaseName = "scala2cfg"

  def newPhase(prev: Phase): Phase = new TraverserPhase(prev)


  class TraverserPhase(prev: Phase) extends StdPhase(prev) {
	def apply(unit: CompilationUnit) {
        ScalaClasses.classes=transformProgram(unit.body, new ScalaPackageIdentifier("", null))
	  }
    }

  def transformProgram(program : Tree, pack : PackageIdentifier) : List[ClassDefinition]= program match {
    case PackageDef(name, stats) => extractListClassDefinitions(stats, new ScalaPackageIdentifier(name.name.decode, pack))
  }

  private def extractListClassDefinitions(list : List[Tree], pack : PackageIdentifier) : List[ClassDefinition] = list match {
    case Nil => Nil
    case x :: list2 if x.isInstanceOf[ClassDef] => extractClassDefinition(x, pack) :: extractListClassDefinitions(list2, pack);
    case x :: list2 if x.isInstanceOf[PackageDef]  => transformProgram(x, pack) ::: extractListClassDefinitions(list2, pack);
  }

  private def extractClassDefinition(program : Tree, pack : PackageIdentifier) : ClassDefinition = program match {
    case ClassDef(mods, name, tparams, Template(parents, self, body)) =>
      val programpoint : ScalaProgramPoint = new ScalaProgramPoint(program.pos)
      val currentType = new ScalaType(program.symbol.tpe)//extractType(program.tpe)
      val parametricTypes : List[ScalaType] = extractListTypes(tparams)
      val extend : List[ClassIdentifier] = Nil
      val classDef = new ClassDefinition(programpoint, currentType, extractModifiers(mods), new ScalaClassIdentifier(name decode, currentType), parametricTypes, extend, null, null, pack, null)
      val members : (List[FieldDeclaration], List[MethodDeclaration]) = extractClassMembers(body, currentType, classDef)
      classDef.methods = members._2
      classDef.fields = members._1
      classDef

    //TODO: I have to consider also parents, and self!

    case _ => throw new ScalaException("I expected a class definition\n"+program.toString)
  }

  private def extractClassMembers(members : List[Tree],
                                  currentType : ch.ethz.inf.pm.sample.oorepresentation.Type,
                                  currentClassDef: ClassDefinition) : (List[FieldDeclaration], List[MethodDeclaration]) = {
    var fields : List[FieldDeclaration] = Nil
    var methods : List[MethodDeclaration] = Nil
    var i : Int = 0
    while(i < members.size) {
      transformClassElement(members apply i, currentType, currentClassDef) match {
        case x : FieldDeclaration => fields = fields ::: x :: Nil
        case x : MethodDeclaration => methods = methods ::: x :: Nil
      }
      i = i + 1
    }
    (fields, methods)
  }

  private def transformClassElement(program : Tree, currentType : ch.ethz.inf.pm.sample.oorepresentation.Type,
                                    currentClassDef: ClassDefinition) : ClassElements = program match {
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
      val programPoint : ScalaProgramPoint = new ScalaProgramPoint(program.pos)
      val parametricTypes : List[ScalaType] = extractListTypes(tparams)
      val returnType : ScalaType = extractType(tpt)
      val arguments : List[List[VariableDeclaration]] = extractListListVariableDeclarations(vparamss)
      val body : ControlFlowGraph = new ControlFlowGraph(new ScalaProgramPoint(program.pos))
      var (cfg, sts, index, goon) = bodyToCFG(rhs, body, Nil, body.addNode(Nil))
      cfg.setNode(index, sts)
      new MethodDeclaration(programPoint,  currentType, extractModifiers(mods),
        new ScalaMethodIdentifier(name.decode), parametricTypes, arguments, returnType,
        RemoveGetterSetter.cleanCFG(cfg), null, null, currentClassDef)
    //TODO pre and post conditions and class invariants

    case ValDef(mods, name, tpt, rhs) =>
      var stringname : String = name.decode
      while(stringname.charAt(stringname.length-1)==' ')
    	  stringname=stringname.substring(0, stringname.length-1)
      new FieldDeclaration(
        new ScalaProgramPoint(program.pos),
        extractModifiers(mods),
        Variable(
          new ScalaProgramPoint(program.pos),
          VariableIdentifier(stringname)(
            new ScalaType(tpt.tpe),
            new ScalaProgramPoint(program.pos))),
        extractType(tpt),
        Some(extractCFG(rhs)))
  }

  /**It collects the label defined during the program by LabelDef statements*/
  private var definedLabel : scala.collection.mutable.Map[String, Int] = new mutable.HashMap[String, Int]()

  /*
   * body : The body to transform
   * cfg : The control flow graph on which you have to add nodes and edges
   * statementsUntilHere : the list of statements already analyzed in the current block
   * currentblock : the index of the current block
   * 1st return : the resulting cfg
   * 2nd return : the list of statements of the current block
   * 3rd return : the index of the current block
   * 4th return : false iff the block ends with a return of a throw statement
   */
  private def bodyToCFG(body : Tree, cfg : ControlFlowGraph, statementsUntilHere : List[Statement], currentblock : Int) : (ControlFlowGraph, List[Statement], Int, Boolean) = body match {
    case Block(stat, expr) => stat match {
      case Nil => bodyToCFG(expr, cfg, statementsUntilHere, currentblock)
      case x :: following =>
      	val (cfg1, sts, block, goon)=bodyToCFG(x, cfg, statementsUntilHere, currentblock)
        if(goon)
        	bodyToCFG(new Block(following, expr), cfg1, sts, block)
        else (cfg1, sts, block, goon)
    }

	case Try(block, catches, finalizer) => bodyToCFG(block, cfg, statementsUntilHere, currentblock)
 // TODO: I have to consider also try-catch-finalize!

	case If(cond, thenp, elsep) =>
	  //CFG of the condition
      val (cfg1, sts, block, gooncond)=bodyToCFG(cond, cfg, statementsUntilHere, currentblock)
      cfg1.setNode(block, sts)
      //Adding the block for then and else branches and computing their CFG
      val thenind : Int = cfg1.addNode(Nil)
    val (thcfg, thsts, thblock, goonth) = bodyToCFG(thenp, cfg1, Nil, thenind)
      val elseind : Int = thcfg.addNode(Nil)
    val (elcfg, elsts, elblock, goonel) = bodyToCFG(elsep, thcfg, Nil, elseind)
      val followingBlock : Int = elcfg.addNode(Nil)
    //Updating the nodes
      elcfg.setNode(elblock, elsts)
      elcfg.setNode(thblock, thsts)
      //Adding the entry edges of the if branches
      elcfg.addEdge(block, thenind, Some(true))
      elcfg.addEdge(block, elseind, Some(false))
      //Adding the exiting edges of the if branches
      if(goonth)
    	  elcfg.addEdge(thblock, followingBlock, None)
      if(goonel)
    	  elcfg.addEdge(elblock, followingBlock, None)
      (elcfg, Nil, followingBlock, goonth || goonel)


    case LabelDef(name, params, rhs) =>
      cfg.setNode(currentblock, statementsUntilHere)
      val chooseBlock : Int = cfg.addNode(Nil)
      definedLabel=definedLabel.+((name decode, chooseBlock))
      cfg.addEdge(currentblock, chooseBlock, None)
      bodyToCFG(rhs, cfg, Nil, chooseBlock)


    case ValDef(mods, name, tpt, rhs) => (cfg, statementsUntilHere ::: extractVariableDefinition(body) :: Nil, currentblock, true);
    //TODO: I have to consider also mods!


    case Assign(lhs, rhs) =>
      (cfg, statementsUntilHere ::: Assignment(new ScalaProgramPoint(body.pos), extractCFG(lhs), extractCFG(rhs)) :: Nil, currentblock, true)
      //bodyToCFG(rhs, cfg, statementsUntilHere ::: new Assignment(new ScalaProgramPoint(body.pos), extractCFG(lhs), extractCFG(rhs)) :: Nil, currentblock)

    case Return(expr) =>
      (cfg, statementsUntilHere ::: extractCFG(expr) :: Nil , currentblock, false)


    /*case Match(selector, cases) =>
      val st : Switch = new Switch(new ScalaProgramPoint(body.pos), extractCFG(selector))
      for(CaseDef(pat, guard, body) <- cases)
        st.addCase(extractCFG(pat), extractCFG(guard), extractCFG(body))
      (cfg, statementsUntilHere ::: st :: Nil , currentblock, true)*/

    case Throw(expr) =>
      (cfg, statementsUntilHere ::: oorepresentation.Throw(new ScalaProgramPoint(body.pos), extractCFG(expr)) :: Nil , currentblock, false)

    case EmptyTree => (cfg, statementsUntilHere, currentblock, true)

    //Typed should be removed by explicit outer, but it does not... I forget type information
    case Typed(what, typed) => bodyToCFG(what, cfg, statementsUntilHere, currentblock)

    case Apply(TypeApply(x, targs), args) =>
      (cfg, statementsUntilHere ::: MethodCall(new ScalaProgramPoint(body.pos), extractCFG(x), extractListTypes(targs), extractListCFG(args), new ScalaType(body.tpe)) :: Nil , currentblock, true)
    case Apply(x, args) =>
      val calledMethod : Statement = extractCFG(x).normalize()
      calledMethod match {
        case variable: Variable if definedLabel.get(variable.getName).isInstanceOf[Some[RunId]] && args.equals(Nil) =>
          //The method call represents a goto statement!
          cfg.setNode(currentblock, statementsUntilHere)
          cfg.addEdge(currentblock, definedLabel(variable.getName), None)
          (cfg, statementsUntilHere, currentblock, false)
        case _ =>
          x.toString match {
            case u if u.equals("scala.Int.box") =>
              (cfg, statementsUntilHere ::: extractListCFG(args), currentblock, true)
            case _ =>
              val result = (cfg, statementsUntilHere ::: MethodCall(new ScalaProgramPoint(body.pos), calledMethod, Nil, extractListCFG(args), new ScalaType(body.tpe)) :: Nil, currentblock, true)
              result
          }
      }
//    	  if(x.toString.equals("scala.Int.box") && args.size==1) //If it's the boxing of an integer, we can ignore that
//    	 	  return (cfg, statementsUntilHere ::: extractListCFG(args), currentblock, true)
//    	  else {
//          val result = (cfg, statementsUntilHere ::: new MethodCall(new ScalaProgramPoint(body.pos), calledMethod, Nil, extractListCFG(args), new ScalaType(body.tpe)) :: Nil , currentblock, true)
//          return result
//        }

    case Ident(name) =>
      (cfg, statementsUntilHere ::: Variable(new ScalaProgramPoint(body.pos),
        VariableIdentifier(name decode)(new ScalaType(body.tpe), new ScalaProgramPoint(body.pos))) :: Nil , currentblock, true)
    case Super(qual, mix) =>
      (cfg, statementsUntilHere ::: Variable(new ScalaProgramPoint(body.pos),
        VariableIdentifier("super")(new ScalaType(body.tpe), new ScalaProgramPoint(body.pos))) :: Nil , currentblock, true)
    //TODO: I have to consider also qual and mix
    // TODO: Should not pass list of statements to the 'FieldAccess' constructor
    // case Select(ArrayValue(elemtpt, trees), field) =>
    //   (cfg, statementsUntilHere ::: new FieldAccess(new ScalaProgramPoint(body.pos), extractListCFG(trees), field decode, new ScalaType(elemtpt.tpe)) :: Nil , currentblock, true)
    //TODO: I forget the types in elemtpt
    case Select(a, field) =>
      if(body.toString.equals("scala.runtime.BoxedUnit.UNIT")) //Ad hoc method to put a Unit value and remove the results of method calls. I wanted to ignore it.
    	  return (cfg, statementsUntilHere, currentblock, true)
      val member=a.tpe.member(field)
      var tpe : ScalaType=new ScalaType(null)
      if(member!=NoSymbol)
    	  tpe = new ScalaType(a.tpe.memberType(member))
      val fieldName=field.decode.replace(" ", "");//remove useless blank spaces, not allowed in fields' names
      val res=(cfg, statementsUntilHere ::: FieldAccess(new ScalaProgramPoint(body.pos), extractCFG(a), fieldName, tpe) :: Nil , currentblock, true)
      res;
    case Literal(value : Constant) =>
      (cfg, statementsUntilHere ::: ConstantStatement(new ScalaProgramPoint(body.pos), value.stringValue, new ScalaType(value.tpe)) :: Nil , currentblock, true)
    //TODO: Support also other numerical type, not only int!
    case x : This =>
      (cfg, statementsUntilHere ::: Variable(new ScalaProgramPoint(body.pos),
        VariableIdentifier("this")(new ScalaType(x.tpe), new ScalaProgramPoint(body.pos))) :: Nil , currentblock, true)
    case New(tpt) => (
      cfg, statementsUntilHere ::: oorepresentation.New(new ScalaProgramPoint(body.pos), extractType(tpt)) :: Nil , currentblock, true)

    case x => throw new ScalaException("Invalid statement:\n"+x)

  }

  private def extractCFG(body : Tree) : ControlFlowGraph = {
      val cfg : ControlFlowGraph = new ControlFlowGraph(new ScalaProgramPoint(body.pos))
      val (cfg1, sts, block, goon)=bodyToCFG(body, cfg, Nil, cfg.addNode(Nil))
      cfg1.setNode(block, sts)
      cfg1
  }
  private def extractListCFG(list : List[Tree]) : List[ControlFlowGraph] = list match {
    case Nil => Nil
    case x :: list2 => x match {
      //TODO: I forget the types of the parameters
      case ArrayValue(elemtpt, trees) => extractListCFG(trees) ::: extractListCFG(list2)
      case _ => extractCFG(x) :: extractListCFG(list2)
    }
  }

  private def extractListTypes(list : List[Tree]) : List[ScalaType] = list match {
    case Nil => Nil
    case x :: list2 => extractType(x) :: extractListTypes(list2)
  }

  private def extractType(body : Tree) : ScalaType = body match {
    case _ => new ScalaType(body.tpe)
  }

  private def extractListVariableDeclarations(list : List[Tree]) : List[VariableDeclaration] = list match {
    case Nil => Nil
    case x :: list2 => extractVariableDefinition(x) :: extractListVariableDeclarations(list2)
  }

  private def extractListListVariableDeclarations(list : List[List[Tree]]) : List[List[VariableDeclaration]] = list match {
    case Nil => Nil
    case x :: list2 => extractListVariableDeclarations(x) :: extractListListVariableDeclarations(list2)
  }

  private def extractVariableDefinition(definition : Tree) : VariableDeclaration = definition match {
    case ValDef(mods, name, tpt, rhs) =>
      VariableDeclaration(
        new ScalaProgramPoint(definition.pos),
        Variable(
          new ScalaProgramPoint(definition.pos),
          VariableIdentifier(name.decode)(extractType(tpt),
            new ScalaProgramPoint(definition.pos))),
        extractType(tpt),
        Some(extractCFG(rhs)))
  }

  private def extractModifiers(mod : Modifiers) : List[Modifier] = {
   var result : List[Modifier] = Nil
    if(mod.hasFlag(Flags.COVARIANT)) result = result ::: CovariantModifier :: Nil
   if(mod.hasFlag(Flags.CONTRAVARIANT)) result = result ::: ContravariantModifier :: Nil
   if(mod.hasFlag(Flags.PRIVATE)) result = result ::: PrivateModifier :: Nil
   if(mod.hasFlag(Flags.PROTECTED)) result = result ::: ProtectedModifier :: Nil
   if(mod.hasFlag(Flags.MUTABLE)) result = result ::: VariableModifier :: Nil
   if(mod.hasFlag(Flags.PARAM)) result = result ::: ArgumentModifier :: Nil
   if(mod.hasFlag(Flags.ACCESSOR)) result = result ::: AccessorModifier :: Nil
   if(mod.hasFlag(Flags.OVERRIDE)) result = result ::: OverrideModifier :: Nil
   if(mod.hasFlag(Flags.ABSTRACT)) result = result ::: AbstractModifier :: Nil
   if(mod.hasFlag(Flags.DEFERRED)) result = result ::: DeferredModifier :: Nil
   if(mod.hasFlag(Flags.CASE)) result = result ::: CaseModifier :: Nil
   if(mod.hasFlag(Flags.SEALED)) result = result ::: SealedModifier :: Nil
   if(mod.hasFlag(Flags.FINAL)) result = result ::: FinalModifier :: Nil
   if(mod.hasFlag(Flags.TRAIT)) result = result ::: TraitModifier :: Nil
   if(mod.hasFlag(Flags.IMPLICIT)) result = result ::: ImplicitModifier :: Nil
   result
  }

  class ScalaType(val typ : global.Type) extends oorepresentation.Type {
	  val sym=if(typ!=null) typ.typeSymbol else null
    val parameters= if(typ!=null)
		  				try{
		  				  typeParamsToExistentials(typ.typeSymbol, typ.typeSymbol.typeParams)
		  				}
                   		catch {
                   		  case _: Throwable => null
                        }
                       else null
    var isTop : Boolean = false
    var isBottom : Boolean = false

    override def toString = name

      override def name: String = {
        if(isTop) return "Any"
        if(isBottom) return "Nothing"
        typ.typeSymbol.name.decode
      }

      override def isStatic: Boolean = {
        if(this.isTop || this.isBottom || typ==null || typ.typeSymbolDirect==NoSymbol)
          false
        else
          typ.typeSymbolDirect.isStatic
      }

      final override def factory() = top()

    def this() = {
        this(null)
        isTop=true
    }

    def lub(other: oorepresentation.Type): oorepresentation.Type = {
      if (other == null) return this
      val (left, right) = cast(this, other)
      if (left.isTop || right.isTop) return top()
      if (left.isBottom) return right
      if (right.isBottom) return left
      try {
        new ScalaType(global.lub(left.typ :: right.typ :: Nil))
      }
      catch {
        case e: Exception => System.out.println("Overapproximation on types because of bug in Scala libraries"); top();
      }
    }

    def glb(other: oorepresentation.Type): oorepresentation.Type = {
      val (left, right) = cast(this, other)
      if (left.isBottom || right.isBottom) return bottom()
      if (left.isTop) return right
      if (right.isTop) return left
      new ScalaType(global.glb(left.typ :: right.typ :: Nil))
    }

    def cast(l: oorepresentation.Type, r: oorepresentation.Type) = {
      if ((!l.isInstanceOf[ScalaType]) || (!r.isInstanceOf[ScalaType]))
        throw new ScalaException("Types are not congruent!")
      (l.asInstanceOf[ScalaType], r.asInstanceOf[ScalaType])
    }

    def cast(l: oorepresentation.Type) = {
      if (!l.isInstanceOf[ScalaType])
        throw new ScalaException("Types are not congruent!")
      l.asInstanceOf[ScalaType]
    }

    def widening(other: oorepresentation.Type): oorepresentation.Type = lub(other)

      def top() : oorepresentation.Type = new ScalaType(); //TODO: Any, problems interfacing with scala compiler
      def bottom() : oorepresentation.Type = { //TODO: Nothing, problems interfacing with scala compiler
        val result : ScalaType=new ScalaType()
      result.isBottom=true
      result.isTop=false
      result
      }

      override def equals(a : Any) : Boolean = a match {
        case x : ScalaType =>
          if(this.isTop && x.isTop) return true
          if(this.isBottom && x.isBottom) return true
          if(this.isTop || x.isTop || this.isBottom || x.isBottom) return false
          if(this.typ==null && x.typ==null) return true
          if(this.typ==null || x.typ==null) return false
          if(this.typ.typeSymbolDirect==null && x.typ.typeSymbolDirect==null) return true
          if(this.typ.typeSymbolDirect==null || x.typ.typeSymbolDirect==null) return false
          this.typ.typeSymbolDirect.equals(x.typ.typeSymbolDirect)
        case _ => false;
      }

      def lessEqual(right : oorepresentation.Type) : Boolean =  {
        val r=cast(right)
        if(r.isTop) return true
        if(this.isTop) return false
        if(this.isBottom) return true
        if(r.isBottom) return false
        if(this.equals(r)) return true
        this.typ <:< r.typ || this.typ =:= r.typ
      }

      def isObject: Boolean = {
        if(this.isTop) return true
        if(this.isBottom) return false
        ! this.isNumericalType
      }

      def isNumericalType: Boolean = {
        if(this.isTop) return true
        if(this.isBottom) return false
        typ.typeSymbol.name.decode.equals("Int") || typ.typeSymbol.name.decode.equals("Float") || typ.typeSymbol.name.decode.equals("Double")
      }

      def isBooleanType: Boolean = {
        if(this.isTop) return true
        if(this.isBottom) return false
        typ.typeSymbol.name.decode.equals("Bool")
      }

      def isFloatingPointType: Boolean = {
        if(this.isTop) return true
        if(this.isBottom) return false
        typ.typeSymbol.name.decode.equals("Float") || typ.typeSymbol.name.decode.equals("Double")
      }

      def isStringType: Boolean = {
        if(this.isTop) return true
        if(this.isBottom) return false
        typ.typeSymbol.name.decode.equals("String")
      }

      def possibleFields: Set[Identifier] = {
        if(! this.isObject) return Set.empty[Identifier]
        if(this.isTop) return Set.empty[Identifier]; //We suppose that Any does not have fields
        var result = Set.empty[Identifier]
        var scope : Scope = typ.decls
        for(el <- scope.toList) {
          val variable=el.isVariable
          val value=el.isValue
          if(variable | value) {
            val typ=el.tpe
            if(! typ.isInstanceOf[MethodType]) {
            	var name=el.name.decode
              if(name.charAt(name.length-1).equals(' '))
            		name=name.substring(0, name.length-1)
              if((! name.equals("_length")) && (! name.equals("$isInstanceOf")) && (! name.equals("$asInstanceOf")) )
                result = result + VariableIdentifier(name)(new ScalaType(typ), new ScalaProgramPoint(el.pos))
             }
            }
        }
        result
      }

  }

}

class ScalaException(message : String) extends Exception(message)