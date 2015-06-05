#!/bin/sh
exec scala "$0" "$@"
!#

import java.io.File

val dirname = if (args.length > 0) args(0) else "."
val dir = new File(dirname)

if (!dir.exists || !dir.isDirectory) {
	println("Invalid arguments.")
	exit(0)
}

val basenames = for {
	filename <- dir.list
	val length = filename.length
	if filename.drop(length - 3) == ".gv"
	val basename = filename.take(length - 3)
} yield basename

basenames.foreach(basename => {
	val command = Array("/bin/sh", "-c", "dot -Tpdf " + basename + ".gv > " + basename + ".pdf")
	val process = Runtime.getRuntime.exec(command)
	process.waitFor
	if (process.exitValue == 0) {
		println("Generated " + basename + ".pdf")
	} else {
		println("Could not generate " + basename + ".pdf")
	}
})
