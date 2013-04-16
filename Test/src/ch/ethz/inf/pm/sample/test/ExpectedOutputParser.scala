package ch.ethz.inf.pm.sample.test

import
scala.util.parsing.combinator.JavaTokenParsers
import java.io.FileReader
import java.lang.Boolean

class ExpectedOutputParser extends JavaTokenParsers  {

  val BOOL : Parser[Boolean] = ("true" | "false") ^^ {
    case "true" => true;
    case "false" => false;
  }

  private var par : Set[(String, Any)] = Set.empty[(String, Any)];
  private var heappar : Set[(String, Any)] = Set.empty[(String, Any)];

  val meth : Parser[List[String]] = "methods="~ stringLiteral  ^^ {
    case "methods="~ s => s.replaceAll("\"", "").split(' ').toList
  }

  val analysis : Parser[String] = ("set"~"analysis"~"="~ stringLiteral) ^^ {
    case "set"~"analysis"~"="~ s => s.replaceAll("\"", "")
  }

  val property : Parser[String] = ("set"~"property"~"="~ stringLiteral) ^^ {
    case "set"~"property"~"="~ s => s.replaceAll("\"", "")
  }

  val heapanalysis : Parser[String] = ("set"~"heap"~"analysis"~"="~ stringLiteral) ^^ {
    case "set"~"heap"~"analysis"~"="~ s => s.replaceAll("\"", "")
  }

  val parameter : Parser[(String, Any)] = ("set"~"analysis " ~ ident ~ "=" ~ BOOL ^^ {
    case "set"~"analysis " ~ id ~ "=" ~ b => (id, b);
  }
    | "set"~"analysis " ~ ident ~ "=" ~ decimalNumber ^^ {
    case "set"~"analysis " ~ id ~ "=" ~ d => (id, d.toInt);
  }
    | "set"~"analysis " ~ ident ~ "=" ~ stringLiteral ^^ {
    case "set"~"analysis " ~ id ~ "=" ~ s => (id, s.replaceAll("\"", ""));
  }
    )
  val heapparameter : Parser[(String, Any)] = ("set"~"heap"~"" ~ ident ~ "=" ~ BOOL ^^ {
    case "set"~"heap"~"" ~ id ~ "=" ~ b => (id, b);
  }
    | "set"~"heap"~"" ~ ident ~ "=" ~ decimalNumber ^^ {
    case "set"~"heap"~"" ~ id ~ "=" ~ d => (id, d.toInt);
  }
    | "set"~"heap"~"" ~ ident ~ "=" ~ stringLiteral ^^ {
    case "set"~"heap"~"" ~ id ~ "=" ~ s => (id, s.replaceAll("\"", ""));
  }
    )

  val listParameters: Parser[Set[(String, Any)]] = ((parameter ^^ {
    case p => par = par + p;
  }) *) ^^ {case _ => par}

  val listHeapParameters: Parser[Set[(String, Any)]] = ((heapparameter ^^ {
    case p => heappar = heappar + p;
  }) *) ^^ {case _ => heappar}

  val CONTRACT : Parser[Contract]= (
    "invariant(" ~ ident ~ "," ~ stringLiteral ~ ")"  ^^ {
      case "invariant(" ~ c ~ "," ~ e ~ ")" => new Invariant(c, e.replaceAll("\"", ""));
    }
      | "predicate(" ~ ident ~ "," ~ ident ~ "," ~ stringLiteral ~ ")" ^^ {
      case "predicate(" ~ c ~ "," ~ n ~ "," ~ e ~ ")" => new Predicate(c, n, e.replaceAll("\"", ""));
    }
      | "precondition(" ~ ident ~ "," ~ ident ~ "," ~ stringLiteral ~ ")" ^^ {
      case "precondition(" ~ c ~ "," ~ m ~ "," ~ e ~ ")" => new PreCondition(c, m, e.replaceAll("\"", ""));
    }
      | "postcondition(" ~ ident ~ "," ~ ident ~ "," ~ stringLiteral ~ ")" ^^ {
      case "postcondition(" ~ c ~ "," ~ m ~ "," ~ e ~ ")" => new PostCondition(c, m, e.replaceAll("\"", ""));
    }
      | "loopinvariant(" ~ decimalNumber ~ "," ~ decimalNumber ~ "," ~ stringLiteral ~ ")"  ^^ {
      case "loopinvariant(" ~ r ~ "," ~ c ~ "," ~ e ~ ")" => new LoopInvariant(r.toInt, c.toInt, e.replaceAll("\"", ""));
    }
    )

  val expectedOutput : Parser[ExpectedOutput] = (
    "warningPP(" ~ decimalNumber ~ "," ~ decimalNumber ~ ")" ^^ {
      case "warningPP(" ~ l ~ "," ~ c ~ ")" => new WarningPP(l.toInt, c.toInt);
    }
      | "validatedPP(" ~ decimalNumber ~ "," ~ decimalNumber ~ ")" ^^ {
      case "validatedPP(" ~ l ~ "," ~ c ~ ")" => new ValidatedPP(l.toInt, c.toInt);
    }
      | "warningMethod(" ~ stringLiteral ~ "," ~ stringLiteral ~ ")" ^^ {
      case "warningMethod(" ~ c ~ "," ~ m ~ ")" => new WarningMethod(c.replaceAll("\"", ""), m.replaceAll("\"", ""));
    }
      | "validatedMethod(" ~ stringLiteral ~ "," ~ stringLiteral ~ ")" ^^ {
      case "validatedMethod(" ~ c ~ "," ~ m ~ ")" => new ValidatedMethod(c.replaceAll("\"", ""), m.replaceAll("\"", ""));
    }
      | "inferredContract(" ~ CONTRACT ~ ")" ^^ {
      case "inferredContract(" ~ c ~ ")" => new InferredContract(c);
    }
    )

  private var outputs : Set[ExpectedOutput] = Set.empty[ExpectedOutput];

  val listOutputs: Parser[Set[ExpectedOutput]] = ((expectedOutput ^^ {
    case m => outputs = outputs + m;
  }) *) ^^ {case _ => outputs}

  val testCases : Parser[(List[String], String, String, Set[(String, Any)], String, Set[(String, Any)], Set[ExpectedOutput])] =
    meth ~ analysis ~ property ~ listParameters ~ heapanalysis ~ listHeapParameters ~ listOutputs ^^ {
      case a ~ b ~ c ~ d ~ e ~ f ~ g=> (a, b, c, d, e, f, g)
    }

  def parse(f : FileReader) : (List[String], String, String, Set[(String, Any)], String, Set[(String, Any)], Set[ExpectedOutput]) = {
    val res : ParseResult[(List[String], String, String, Set[(String, Any)], String, Set[(String, Any)], Set[ExpectedOutput])] = this.parseAll(testCases, f);
    if(res.isEmpty) {
      System.out.println("Error while parsing the test file");
      System.out.println(res.toString);
      return null;
    }
    else return res.get;
  };

  val analysisSettings : Parser[(List[String], String, String, Set[(String, Any)], String, Set[(String, Any)])] =
    meth ~ analysis ~ property ~ listParameters ~ heapanalysis ~ listHeapParameters ^^ {
      case a ~ b ~ c ~ d ~ e ~ f => (a, b, c, d, e, f)
    }

  def parseAnalysisSettings(f : FileReader) : (List[String], String, String, Set[(String, Any)], String, Set[(String, Any)]) = {
    val res : ParseResult[(List[String], String, String, Set[(String, Any)], String, Set[(String, Any)])] = this.parseAll(analysisSettings, f);
    if(res.isEmpty) {
      System.out.println("Error while parsing the test file");
      System.out.println(res.toString);
      return null;
    }
    else return res.get;
  };

  def parseOnlyTestResults(f : FileReader) : Set[ExpectedOutput] = {
    val res : ParseResult[Set[ExpectedOutput]] = this.parseAll(listOutputs, f);
    if(res.isEmpty) {
      System.out.println("Error while parsing the test file");
      System.out.println(res.toString);
      return null;
    }
    else return res.get;
  };

}