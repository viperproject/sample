package ch.ethz.inf.pm.sample.preprocessing.scalaprocessing

import scala.tools.nsc._
import scala.tools.nsc.symtab._;
import scala.tools.nsc.ast._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
//import ch.ethz.inf.pm.sample.gui._
import ch.ethz.inf.pm.sample.property._
import java.util.ArrayList
import scala.collection.mutable.HashMap
import scala.tools.nsc.plugins.PluginComponent
//import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._

class ScalaProgramPoint(pos : scala.tools.nsc.util.Position) extends ProgramPoint {
  val row : Int = try{
	  pos.line
  }
  catch {
	  case _ => -1
  }
  val column : Int  = try{
	  pos.column
  }
  catch {
	  case _ => -1
  }
  
  override def equals(o : Any) : Boolean = o match {
    case x : ScalaProgramPoint => x.row==row && x.column==column;
    case _ => false;
  }
  
  override def toString() : String = {
    var result : String ="";
    if(row != -1)
      result=result+"line "+row+" ";
    else result=result+"line unknown ";
    if(column != -1)
      result=result+"column "+column+" ";
    else result=result+"column unknown ";
    result;
  }
}


abstract class Named(name : String) {

  def getName() : String = name
  
  override def equals(o : Any) : Boolean = o match {
    case x : Named => x.getName().equals(name) && x.getClass().equals(this.getClass())
    case _ => false;
  }
  
  override def hashCode() : Int = name.hashCode();
  
  override def toString() = name
}

class ScalaMethodIdentifier(name : String) extends Named(name) with MethodIdentifier

class ScalaClassIdentifier(val name : String, val thisType : Type) extends Named(name) with ClassIdentifier {
	def getThisType() = thisType;
}

class ScalaPackageIdentifier(name : String, pack : PackageIdentifier) extends Named(name) with PackageIdentifier



object Performances {
  var transformationTime : Long = 0;
  var classes : Long = 0;
}

class ScalaProgramToControlFlowGraph(val global: Global) extends PluginComponent {
  import global._
  import java.util.Timer
  
  override val runsAfter = "dce" :: Nil

  val phaseName = "scala2cfg"
	
  def newPhase(prev: Phase): Phase = new TraverserPhase(prev)
   
     
  class TraverserPhase(prev: Phase) extends StdPhase(prev) {
	def apply(unit: CompilationUnit) {
	    SystemParameters.currentFile=unit.source.file.toString
        ScalaClasses.classes=transformProgram(unit.body, new ScalaPackageIdentifier("", null))
	  }
    }

  def transformProgram(program : Tree, pack : PackageIdentifier) : List[ClassDefinition]= program match {
    case PackageDef(name, stats) => extractListClassDefinitions(stats, new ScalaPackageIdentifier(name.name.decode, pack))
  }
  
  private def extractListClassDefinitions(list : List[Tree], pack : PackageIdentifier) : List[ClassDefinition] = list match {
    case Nil => Nil
    case x :: list2 if (x.isInstanceOf[ClassDef]) => extractClassDefinition(x, pack) :: extractListClassDefinitions(list2, pack);
    case x :: list2 if (x.isInstanceOf[PackageDef])  => transformProgram(x, pack) ::: extractListClassDefinitions(list2, pack);
  }
    
  private def extractClassDefinition(program : Tree, pack : PackageIdentifier) : ClassDefinition = program match {
    case ClassDef(mods, name, tparams, Template(parents, self, body)) =>
      val programpoint : ScalaProgramPoint = new ScalaProgramPoint(program.pos);
      val currentType = new ScalaType(program.symbol.tpe)//extractType(program.tpe)
      SystemParameters.typ=currentType;
      val parametricTypes : List[ScalaType] = extractListTypes(tparams);
      val extend : List[ClassIdentifier] = Nil;
      val members : (List[FieldDeclaration], List[MethodDeclaration]) = extractClassMembers(body, currentType);
      val classname : String=name.decode;
      new ClassDefinition(programpoint, extractModifiers(mods), new ScalaClassIdentifier(name decode, currentType), parametricTypes, extend, members._1, members._2, pack, null)
      //TODO: I have to consider also parents, and self!
      
    case _ => throw new ScalaException("I expected a class definition\n"+program.toString())
  }
  
  private def extractClassMembers(members : List[Tree], currentType : ch.ethz.inf.pm.sample.oorepresentation.Type) : (List[FieldDeclaration], List[MethodDeclaration]) = {
    var fields : List[FieldDeclaration] = Nil;
    var methods : List[MethodDeclaration] = Nil;
    var i : Int = 0;
    while(i < members.size) {
      transformClassElement(members apply(i), currentType) match {
        case x : FieldDeclaration => fields = fields ::: x :: Nil
        case x : MethodDeclaration => methods = methods ::: x :: Nil
      }
      i = i + 1;
    }
    (fields, methods)
  }
  
  private def transformClassElement(program : Tree, currentType : ch.ethz.inf.pm.sample.oorepresentation.Type) : ClassElements = program match {
    case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
      val programPoint : ScalaProgramPoint = new ScalaProgramPoint(program.pos);
      val parametricTypes : List[ScalaType] = extractListTypes(tparams);
      val returnType : ScalaType = extractType(tpt)
      val arguments : List[List[VariableDeclaration]] = extractListListVariableDeclarations(vparamss)
      val body : ControlFlowGraph = new ControlFlowGraph(new ScalaProgramPoint(program.pos));
      var (cfg, sts, index, goon) = bodyToCFG(rhs, body, Nil, body.addNode(Nil));
      cfg.setNode(index, sts)
      new MethodDeclaration(programPoint,  currentType, extractModifiers(mods), new ScalaMethodIdentifier(name.decode), parametricTypes, arguments, returnType, RemoveGetterSetter.cleanCFG(cfg), null, null)
    //TODO pre and post conditions and class invariants
      
    case ValDef(mods, name, tpt, rhs) => 
      var stringname : String = name.decode;
      while(stringname.charAt(stringname.length-1)==' ')
    	  stringname=stringname.substring(0, stringname.length-1);
      new FieldDeclaration(new ScalaProgramPoint(program.pos),  extractModifiers(mods), new Variable(new ScalaProgramPoint(program.pos), new VariableIdentifier(stringname, new ScalaType(tpt.tpe))), extractType(tpt), extractCFG(rhs) )
  } 
  
  /**It collects the label defined during the program by LabelDef statements*/
  private var definedLabel : scala.collection.mutable.Map[String, Int] = new HashMap[String, Int]();

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
      val thenind : Int = cfg1.addNode(Nil);
      val (thcfg, thsts, thblock, goonth) = bodyToCFG(thenp, cfg1, Nil, thenind)
      val elseind : Int = thcfg.addNode(Nil);
      val (elcfg, elsts, elblock, goonel) = bodyToCFG(elsep, thcfg, Nil, elseind)
      val followingBlock : Int = elcfg.addNode(Nil);
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
      cfg.setNode(currentblock, statementsUntilHere);
      val chooseBlock : Int = cfg.addNode(Nil);
      definedLabel=definedLabel.+((name decode, chooseBlock));
      cfg.addEdge(currentblock, chooseBlock, None)
      bodyToCFG(rhs, cfg, Nil, chooseBlock)
  
      
    case ValDef(mods, name, tpt, rhs) => (cfg, statementsUntilHere ::: extractVariableDefinition(body) :: Nil, currentblock, true);
    //TODO: I have to consider also mods!
    

    case Assign(lhs, rhs) => 
      (cfg, statementsUntilHere ::: new Assignment(new ScalaProgramPoint(body.pos), extractCFG(lhs), extractCFG(rhs)) :: Nil, currentblock, true)
      //bodyToCFG(rhs, cfg, statementsUntilHere ::: new Assignment(new ScalaProgramPoint(body.pos), extractCFG(lhs), extractCFG(rhs)) :: Nil, currentblock)
      
    case Return(expr) => 
      (cfg, statementsUntilHere ::: extractCFG(expr) :: Nil , currentblock, false)
      
      
    /*case Match(selector, cases) =>
      val st : Switch = new Switch(new ScalaProgramPoint(body.pos), extractCFG(selector))
      for(CaseDef(pat, guard, body) <- cases)
        st.addCase(extractCFG(pat), extractCFG(guard), extractCFG(body))
      (cfg, statementsUntilHere ::: st :: Nil , currentblock, true)*/
     
    case Throw(expr) => 
      (cfg, statementsUntilHere ::: new oorepresentation.Throw(new ScalaProgramPoint(body.pos), extractCFG(expr)) :: Nil , currentblock, false)
 
    case EmptyTree => (cfg, statementsUntilHere, currentblock, true)
    
    //Typed should be removed by explicit outer, but it does not... I forget type information
    case Typed(what, typed) => bodyToCFG(what, cfg, statementsUntilHere, currentblock)
    
    case Apply(TypeApply(x, targs), args) => 
      (cfg, statementsUntilHere ::: new MethodCall(new ScalaProgramPoint(body.pos), extractCFG(x), extractListTypes(targs), extractListCFG(args), new ScalaType(body.tpe)) :: Nil , currentblock, true)
    case Apply(x, args) =>
      val calledMethod : Statement = extractCFG(x).normalize();
      if(calledMethod.isInstanceOf[Variable] && definedLabel.get(calledMethod.asInstanceOf[Variable].getName()).isInstanceOf[Some[Int]] && args.equals(Nil)) {
        //The method call represents a goto statement!
        cfg.setNode(currentblock, statementsUntilHere)
        cfg.addEdge(currentblock, definedLabel.get(calledMethod.asInstanceOf[Variable].getName()).get, None);
        (cfg, statementsUntilHere, currentblock, false)
      }
      else
    	  if(x.toString().equals("scala.Int.box") && args.size==1) //If it's the boxing of an integer, we can ignore that 
    	 	  return (cfg, statementsUntilHere ::: extractListCFG(args), currentblock, true)
    	  else return (cfg, statementsUntilHere ::: new MethodCall(new ScalaProgramPoint(body.pos), calledMethod, Nil, extractListCFG(args), new ScalaType(body.tpe)) :: Nil , currentblock, true)
 
    case Ident(name) => 
      (cfg, statementsUntilHere ::: new Variable(new ScalaProgramPoint(body.pos), new VariableIdentifier(name decode, new ScalaType(body.tpe))) :: Nil , currentblock, true)
    case Super(qual, mix) => 
      (cfg, statementsUntilHere ::: new Variable(new ScalaProgramPoint(body.pos), new VariableIdentifier("super", new ScalaType(body.tpe))) :: Nil , currentblock, true)
    //TODO: I have to consider also qual and mix
    case Select(ArrayValue(elemtpt, trees), field) => 
      (cfg, statementsUntilHere ::: new FieldAccess(new ScalaProgramPoint(body.pos), extractListCFG(trees), field decode, new ScalaType(elemtpt.tpe)) :: Nil , currentblock, true)
    //TODO: I forget the types in elemtpt
    case Select(a, field) => 
      if(body.toString.equals("scala.runtime.BoxedUnit.UNIT")) //Ad hoc method to put a Unit value and remove the results of method calls. I wanted to ignore it.
    	  return (cfg, statementsUntilHere, currentblock, true)
      val member=a.tpe.member(field);
      var tpe : ScalaType=new ScalaType(null);
      if(member!=NoSymbol)
    	  tpe = new ScalaType(a.tpe.memberType(member));
      val fieldName=field.decode.replace(" ", "");//remove useless blank spaces, not allowed in fields' names
      val res=(cfg, statementsUntilHere ::: new FieldAccess(new ScalaProgramPoint(body.pos), extractCFG(a) :: Nil, fieldName, tpe) :: Nil , currentblock, true)
      res;
    case Literal(value : Constant) => 
      (cfg, statementsUntilHere ::: new NumericalConstant(new ScalaProgramPoint(body.pos), value.stringValue, new ScalaType(value.tpe)) :: Nil , currentblock, true)
    //TODO: Support also other numerical type, not only int!
    case x : This => 
      (cfg, statementsUntilHere ::: new Variable(new ScalaProgramPoint(body.pos), new VariableIdentifier("this", new ScalaType(x.tpe))) :: Nil , currentblock, true)
    case New(tpt) => (
      cfg, statementsUntilHere ::: new oorepresentation.New(new ScalaProgramPoint(body.pos), extractType(tpt)) :: Nil , currentblock, true)
    
    case x => throw new ScalaException("Invalid statement:\n"+x toString)
      
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
    case ValDef(mods, name, tpt, rhs) => new VariableDeclaration(new ScalaProgramPoint(definition.pos), new Variable(new ScalaProgramPoint(definition.pos), new VariableIdentifier(name.decode, extractType(tpt))), extractType(tpt), extractCFG(rhs) )
  }  
  
  private def extractModifiers(mod : Modifiers) : List[Modifier] = {
   var result : List[Modifier] = Nil;
   if(mod.isCovariant) result = result ::: CovariantModifier :: Nil
   if(mod.isContravariant) result = result ::: ContravariantModifier :: Nil
   if(mod.isPrivate) result = result ::: PrivateModifier :: Nil
   if(mod.isProtected) result = result ::: ProtectedModifier :: Nil
   if(mod.isVariable) result = result ::: VariableModifier :: Nil
   if(mod.isArgument) result = result ::: ArgumentModifier :: Nil
   if(mod.isAccessor) result = result ::: AccessorModifier :: Nil
   if(mod.isOverride) result = result ::: OverrideModifier :: Nil
   if(mod.isAbstract) result = result ::: AbstractModifier :: Nil
   if(mod.isDeferred) result = result ::: DeferredModifier :: Nil
   if(mod.isCase) result = result ::: CaseModifier :: Nil
   if(mod.isSealed) result = result ::: SealedModifier :: Nil
   if(mod.isFinal) result = result ::: FinalModifier :: Nil
   if(mod.isTrait) result = result ::: TraitModifier :: Nil
   if(mod.isImplicit) result = result ::: ImplicitModifier :: Nil
   if(mod.isPublic) result = result ::: PublicModifier :: Nil
   result
  }
  
  class ScalaType(val typ : global.Type) extends oorepresentation.Type {
	  val sym=if(typ!=null) typ.typeSymbol else null;
	  val parameters= if(typ!=null) 
		  				try{
		  				  typeParamsToExistentials(typ.typeSymbol, typ.typeSymbol.typeParams)
		  				}
                   		catch {
                   		  case _ => null
                        }
                       else null;
	  var isTop : Boolean = false;
      var isBottom : Boolean = false;
    
      override def toString() = getName()
      
      override def getName() : String = {
        if(isTop) return "Any";
        if(isBottom) return "Nothing";
        return typ.typeSymbol.name.decode;
      }
      
      override def isStatic() : Boolean = {
        if(this.isTop || this.isBottom || typ==null || typ.typeSymbolDirect==NoSymbol)
          return false
        else
          return typ.typeSymbolDirect.isStatic;
      }
      
      final override def factory() = top();
      
      def this() = {
        this(null)
        isTop=true;
      }
      
      def lub(l : oorepresentation.Type, r : oorepresentation.Type) : oorepresentation.Type = {
        if(l==null) return r;
        if(r==null) return l;
        val (left, right)=cast(l, r);
        if(left.isTop || right.isTop) return top();
        if(left.isBottom) return right;
        if(right.isBottom) return left;
        try{
          new ScalaType(global.lub(left.typ :: right.typ :: Nil))
        }
        catch { case e : Exception => System.out.println("Overapproximation on types because of bug in Scala libraries"); top();} 
      }
      
      def glb(l : oorepresentation.Type, r : oorepresentation.Type) : oorepresentation.Type = {
        val (left, right)=cast(l, r);
        if(left.isBottom || right.isBottom) return bottom();
        if(left.isTop) return right;
        if(right.isTop) return left;
        new ScalaType(global.glb(left.typ :: right.typ :: Nil))
      }
      
      def cast(l : oorepresentation.Type, r : oorepresentation.Type) = {
        if((! l.isInstanceOf[ScalaType]) || (! r.isInstanceOf[ScalaType]))
          throw new ScalaException("Types are not congruent!");
        (l.asInstanceOf[ScalaType], r.asInstanceOf[ScalaType])
      }
      
      def cast(l : oorepresentation.Type) = {
        if(! l.isInstanceOf[ScalaType])
          throw new ScalaException("Types are not congruent!");
        l.asInstanceOf[ScalaType]
      }
            
      def widening(left : oorepresentation.Type, right : oorepresentation.Type) : oorepresentation.Type = lub(left, right)
      
      def top() : oorepresentation.Type = new ScalaType(); //TODO: Any, problems interfacing with scala compiler
      def bottom() : oorepresentation.Type = { //TODO: Nothing, problems interfacing with scala compiler
        val result : ScalaType=new ScalaType();
        result.isBottom=true;
        result.isTop=false;
        result
      }
      
      override def equals(a : Any) : Boolean = a match {
        case x : ScalaType => 
          if(this.isTop==true && x.isTop == true) return true;
          if(this.isBottom==true && x.isBottom == true) return true;
          if(this.isTop==true || x.isTop == true || this.isBottom==true || x.isBottom == true) return false;
          if(this.typ==null && x.typ==null) return true;
          if(this.typ==null || x.typ==null) return false;
          if(this.typ.typeSymbolDirect==null && x.typ.typeSymbolDirect==null) return true;
          if(this.typ.typeSymbolDirect==null || x.typ.typeSymbolDirect==null) return false;
          return this.typ.typeSymbolDirect.equals(x.typ.typeSymbolDirect)
        case _ => return false;
      }
      
      def lessEqual(right : oorepresentation.Type) : Boolean =  {
        val r=cast(right);
        if(r.isTop) return true;
        if(this.isTop) return false;
        if(this.isBottom) return true;
        if(r.isBottom) return false;
        if(this.equals(r)) return true;
        return this.typ <:< r.typ || this.typ =:= r.typ
      }
      
      def isObject() : Boolean = {
        if(this.isTop) return true;
        if(this.isBottom) return false;
        return ! this.isNumericalType();
      }
      
      def isNumericalType() : Boolean = {
        if(this.isTop) return true;
        if(this.isBottom) return false;
        return typ.typeSymbol.name.decode.equals("Int");
      }
      
      def isBottomExcluding(types : Set[oorepresentation.Type]) : Boolean = {
        for(t <- types)
          if(this.lessEqual(t)) 
            return true;
        //typ.typeSymbol.children returns Empty iff it's not a sealed class, the set of childred otherwise
        if(typ==null || typ.typeSymbol==null || typ.typeSymbol.children==null) return false;
        typ.typeSymbol.children match {
          case x if (x.equals(Set.empty[Symbol]))=> return false;
          case x : Set[Symbol] =>
            for(childrenType <- x)
              if(! new ScalaType(childrenType.info).isBottomExcluding(types))
                return false;
            return true;
        }
      }
      
      def getPossibleFields() : Set[(String, oorepresentation.Type)] = {
        if(! this.isObject) return Set.empty[(String, oorepresentation.Type)];
        if(this.isTop) return Set.empty[(String, oorepresentation.Type)]; //We suppose that Any does not have fields
        var result = Set.empty[(String, oorepresentation.Type)];
        var scope : Scope = typ.decls;
        for(el <- scope.toList) {
          val variable=el.isVariable;
          val value=el.isValue;
          if(variable | value) {
            val typ=el.tpe;
            if(! typ.isInstanceOf[MethodType]) {
            	var name=el.name.decode;
            	if(name.charAt(name.size-1).equals(' '))
            		name=name.substring(0, name.size-1);
            	result=result+((name, new ScalaType(typ)));
             }
            }
        }
        result;
      }
  }
    
}

class ScalaException(message : String) extends Exception(message)