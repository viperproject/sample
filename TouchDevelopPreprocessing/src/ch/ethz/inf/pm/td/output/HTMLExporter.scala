package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.{SystemParameters, Reporter}
import java.io.{PrintWriter, BufferedWriter, FileWriter, File}
import ch.ethz.inf.pm.td.compiler.TouchCompiler
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import xml.Elem

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


      for ((id,source) <- compiler.parsedSourceStrings) {
        var annotations: Map[Integer,Elem] = Map.empty

        for ((message,pp) <- Reporter.seenErrors) {
          val pos = findPosition(id,source,pp)
          if (pos != -1) {
            val xml = <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip" />
            annotations = annotations + ((pos,xml))
          }
        }

        for ((message,pp) <- Reporter.seenBottom) {
          val pos = findPosition(id,source,pp)
          if (pos != -1) {
            val xml = <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip" />
            annotations = annotations + ((pos,xml))
          }
        }

        for ((message,pp) <- Reporter.seenImprecision) {
          val pos = findPosition(id,source,pp)
          if (pos != -1) {
            val xml = <img src="http://i.imgur.com/vTVqlzB.png" title={message} class="masterTooltip" />
            annotations = annotations + ((pos,xml))
          }
        }

        pw.println(<h2>{id}</h2>)
        pw.println(<pre>{
            var lastPos = 0
            (for (pos <- annotations.keySet.toList.sorted) yield {
              val ret =
                if (lastPos != pos) List(xml.Text(source.substring(lastPos,pos)),annotations.get(pos).get)
                else List(annotations.get(pos).get)
              lastPos = pos
              ret
            } ::: List(List(xml.Text(source.substring(lastPos))))).flatten
        }</pre>)
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

  def findPosition(id:String,source:String,pp:ProgramPoint):Integer = {
    var column = 0
    var row = 1
    for (i <- 0 until source.length) {
      if (column == pp.getColumn() && row == pp.getLine()) return i
      if(source.charAt(i).equals('\n')) { column = 0; row = row + 1; }
      else column = column + 1
    }
    -1
  }

}
