package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.{SystemParameters, Reporter}
import java.io.{PrintWriter, BufferedWriter, FileWriter, File}
import ch.ethz.inf.pm.td.compiler.{TouchProgramPoint, TouchCompiler}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import xml.Elem
import ch.ethz.inf.pm.td.parser.PrettyPrinter

/**
 * User: lucas
 * Date: 3/5/13
 * Time: 1:17 PM
 */
object HTMLExporter {

  def apply() {

    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    var tmp:File = null
    var fw:FileWriter = null
    var pw:PrintWriter = null
    try {

      tmp = File.createTempFile(compiler.mainID,".html")
      fw = new FileWriter(tmp, true)
      pw = new PrintWriter(fw)

      pw.print(
        """
          |<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
          |  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
          |<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
          |<head>
          |  <title>Analysis results</title>
          |  <meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
          |  <meta http-equiv="Content-Style-Type" content="text/css" />
          |  <meta http-equiv="Content-Script-Type" content="text/javascript" />
          |  <meta name="description" content="" />
          |  <link rel="stylesheet" href="screen.css" type="text/css" media="screen,projection,tv" />
          |  <link rel="stylesheet" href="print.css" type="text/css" media="print" />
          |  <!--[if lt IE 7]>
          |    <link rel="stylesheet" href="ie6.css" type="text/css" media="screen,projection,tv" />
          |    <script src="http://ie7-js.googlecode.com/svn/version/2.0(beta3)/IE7.js" type="text/javascript"></script>
          |  <![endif]-->
          |  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js" type="text/javascript"></script>
          |  <script type="text/javascript">
          |	$(document).ready(function() {
          |			// Tooltip only Text
          |			$('.masterTooltip').hover(function(){
          |					// Hover over code
          |					var title = $(this).attr('title');
          |					$(this).data('tipText', title).removeAttr('title');
          |					$('<p class="tooltip"></p>')
          |					.text(title)
          |					.appendTo('body')
          |					.fadeIn('slow');
          |			}, function() {
          |					// Hover out code
          |					$(this).attr('title', $(this).data('tipText'));
          |					$('.tooltip').remove();
          |			}).mousemove(function(e) {
          |					var mousex = e.pageX + 20; //Get X coordinates
          |					var mousey = e.pageY + 10; //Get Y coordinates
          |					$('.tooltip')
          |					.css({ top: mousey, left: mousex })
          |			});
          |	});
          |	</script>
          |	<style>
          |	.tooltip {
          |		display:none;
          |		position:absolute;
          |		border:1px solid #333;
          |		background-color:#161616;
          |		border-radius:5px;
          |		padding:10px;
          |		color:#fff;
          |		font-size:12px Arial;
          |	}
          |	.masterTooltip {
          |		color:red;
          |	}
          |	</style>
          |</head>
          |<body id="" class="">
        """.stripMargin)

      pw.println("<h1>Analysis results for script with the id "+compiler.mainID+"</h1>")

      for ((id,script) <- compiler.parsedTouchScripts) {
        var annotations: Map[Integer,Elem] = Map.empty

        pw.println(<h2>{id}</h2>)
        pw.println("<pre>" +
          PrettyPrinter.applyWithPPPrinter(script)({curPP:ProgramPoint =>
          (for ((message,pp) <- Reporter.seenErrors) yield {
            if (curPP.equals(pp)) {
              <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip" />.toString()
            } else ""
          }).mkString("") +
          (for ((message,pp) <- Reporter.seenBottom) yield {
            if (curPP.equals(pp)) {
                <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip" />.toString()
            } else ""
          }).mkString("") +
          (for ((message,pp) <- Reporter.seenImprecision) yield {
            if (curPP.equals(pp)) {
                <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip" />.toString()
            } else ""
          }).mkString("")
        })
        + "</pre>")
      }

      pw.println(
        """
          |</body>
          |</html>
        """.stripMargin)

    } finally {

      if(pw != null) pw.close()
      if(fw != null) fw.close()

    }

    if (tmp != null) {
      println("Analysis result have been written to: "+tmp.toURI.toString)
    } else {
      println("Failed to write analysis results")
    }

  }

}
