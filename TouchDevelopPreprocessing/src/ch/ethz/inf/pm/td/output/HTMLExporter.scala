/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.{Reporter, SampleError}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.{IdPositional, PrettyPrinter, Script}

/**
 * Exports to HTML
 *
 * @author Lucas Brutschy
 */
class HTMLExporter extends FileSystemExporter {

  def getExtension = "html"

  def warningsToString(compiler: TouchCompiler): String = {
    export(compiler.parsedTouchScripts)
  }

  def warningsToString(compiler: TouchCompiler, id: String): String = {
    export(Map(id -> compiler.parsedTouchScripts.get(id).get))
  }

  def export(targets: Map[String, Script]): String = {
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
        |	.hoverError {
        |		background-color:yellow;
        |	}
        |	.hoverCause {
        |		background-color:#4fd5d6;
        |	}
        |	.masterTooltip {
        |		color:red;
        |		display:inline-block;
        |		width:16px;
        |		height:16px;
        |		background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABvUlEQVR4nI2SwW4TMRCGPyfetCkJTRaIWopEqxRo4IDU0/Yd+gS9wL08D8+yb2ApN04gUUAqUrRw6CZNK7Rrr4fDptlGSQQjjSz7n/n8yx5YE0kcRtlwT7LhniRxGK2r0+uEbtg06sU5IHS/fjSAWlVXW3e790LQ2SfoHOALYZ2LlYBWq2Hqrz7M9/Wjc1qthvkvQBKHkXOeoL2DUmcodYZu71IUfqWLJYDWNdN8/R5xN9Whm7I5eIfW9SUXC4DydiFotsFeV4KdEjQf4lyx5GIB4HJvOoNTJJ9APq6EfIzYCd3BKc56sxKQxGFkrSdoaMhTxKbzIslTyFP0RoDNF99iDpimmXly+AaxKZJfIVkFIE+RWYb9Y67TzCwAkjiMrPNsbG1CloIdl3nfQVbmg1YN5yoXGuDX6Nbs7vfAjpF5m8J+PgYR5B4MFN2dHr9/JgZQOonDaHQ5pfOogdgpCimHVhTB24vyEz7173oBeNzTjH6ULvTlt4l5frgN7gZQCIKardX33MJMYXb+9NkWl98nRheFgDgoirnFu+Z8uF2+QfGnggmIAvGWwnn0wcvOycWXq5Vz/q/oH4UnfwFlZuAVdLyXBQAAAABJRU5ErkJggg==');
        |	}
        |	</style>
        |
        |</head>
        |<body id="" class="">
      """.stripMargin

    def toStr(s: ProgramPoint) = s match {
      case p: SpaceSavingProgramPoint => p.fullPosString;
      case _ => ""
    }

    for ((id, script) <- targets) {
      res += <h2>
        {id}
      </h2>
      res += "<pre>" +
        PrettyPrinter.applyWithPPPrinter(script)({
          (curPositional: IdPositional, pretty: String) =>

            val spanId = TouchProgramPointRegistry.get(id, curPositional) match {
              case Some(x) => x.fullPosString;
              case None => ""
            }
            val onmouseover = "$('#" + spanId + "').addClass('hoverError');"
            val onmouseout  = "$('#" + spanId + "').removeClass('hoverError');"
            "<span id='" + spanId + "'>" +
              (for (SampleError(errorTypeId, message, pp, causes) <- Reporter.assertionViolations) yield {
                pp match {
                  case touchPP: SpaceSavingProgramPoint if TouchProgramPointRegistry.matches(touchPP, id, curPositional) =>
                    val onmouseover2 = onmouseover + (for (c <- causes) yield {
                      "$('#" + toStr(c._2) + "').addClass('hoverCause');"
                    }).mkString(";")
                    val onmouseout2 = onmouseout + (for (c <- causes) yield {
                      "$('#" + toStr(c._2) + "').removeClass('hoverCause');"
                    }).mkString(";")
                    <div title={message} class="masterTooltip" onmouseover={onmouseover2} onmouseout={onmouseout2}></div>.toString()
                  case _ => ""
                }
              }).mkString("") +
              (for (m <- Reporter.unreachableCode) yield {
                  m.pp match {
                  case touchPP: SpaceSavingProgramPoint if TouchProgramPointRegistry.matches(touchPP, id, curPositional) =>
                      <div title={m.message} class="masterTooltip" onmouseover={onmouseover} onmouseout={onmouseout}></div>.toString()
                  case _ => ""
                }
              }).mkString("") +
              (for (m <- Reporter.impreciseSemantics) yield {
                m.pp match {
                  case touchPP: SpaceSavingProgramPoint if TouchProgramPointRegistry.matches(touchPP, id, curPositional) =>
                      <div title={m.message} class="masterTooltip" onmouseover={onmouseover} onmouseout={onmouseout}></div>.toString()
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
