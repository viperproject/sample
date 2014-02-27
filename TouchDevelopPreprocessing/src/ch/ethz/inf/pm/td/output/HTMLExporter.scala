package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchProgramPoint, TouchCompiler}
import ch.ethz.inf.pm.td.parser.{Script, IdPositional, PrettyPrinter}
import ch.ethz.inf.pm.sample.reporting.{SampleError, Reporter}

/**
 * Exports to HTML
 * 
 * @author Lucas Brutschy
 */
class HTMLExporter extends ErrorExporter {

  def getExtension = "html"

  def apply(compiler: TouchCompiler): String = {
    export(compiler.parsedTouchScripts)
  }

  def apply(compiler: TouchCompiler, id: String): String = {
    export(Map(id -> compiler.parsedTouchScripts.get(id).get))
  }

  private def export(targets: Map[String, Script]): String = {
    var res =
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
        |  <style> body { font-family:sans-serif; } span:target { color:red; background-color:yellow; } </style>
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
      """.stripMargin

    for ((id, script) <- targets) {
      res += <h2>
        {id}
      </h2>
      res += "<pre>" +
        PrettyPrinter.applyWithPPPrinter(script)({
          (curPositional: IdPositional, pretty: String) =>
            val curPP = CFGGenerator.makekTouchProgramPoint(id, curPositional)
                "<span id='" + curPP.fullPosString + "'>" +
                  (for (SampleError(errorTypeId, message, pp) <- Reporter.seenErrors) yield {
                    pp match {
                      case touchPP: TouchProgramPoint if touchPP == curPP =>
                        <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip"/>.toString()
                      case _ => ""
                    }
                  }).mkString("") +
                  (for ((message, pp) <- Reporter.seenBottom) yield {
                    pp match {
                      case touchPP: TouchProgramPoint if touchPP == curPP =>
                            <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip"/>.toString()
                      case _ => ""
                    }
                  }).mkString("") +
                  (for ((message, pp) <- Reporter.seenImprecision) yield {
                    pp match {
                      case touchPP: TouchProgramPoint if touchPP == curPP =>
                        <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip"/>.toString()
                      case _ => ""
                    }
                  }).mkString("") +
                  pretty + "</span>"
        }) + "</pre>"
    }

    res += """
             |</body>
             |</html>
           """.stripMargin

    res

  }

}
