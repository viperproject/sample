import ch.ethz.inf.pm.td.analysis.TestRunner
import ch.ethz.inf.pm.td.parser.PrettyPrinter
import ch.ethz.inf.pm.td.webapi._
import java.io.{FileOutputStream, File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.{Date, GregorianCalendar}

def date(a:String) = new SimpleDateFormat("dd/MM/yyyy").parse(a)

def printer(file:String)(id:String) {

  val p = new PrintWriter(new FileOutputStream(new File("Test/test/TouchDevelop/testsets/"+file),true))

  try {
    p.println(id)
  } finally {
    p.close()
  }

}

TestRunner(new NonErroneousPrefixRootScriptsBefore(date("13/05/2013"), "a"),100000,printer("A_130522_RootNoError"))
//TestRunner(new NonErroneousPrefixRootScriptsBefore(date("24/10/2013"), "a"),100000,printer("A_131024_RootNoError"))
