/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import scala.language.implicitConversions

object StringBlock {

	def apply(s: String) = new StringBlock(s.split("\n"))

	def border(s: String): StringBlock = border(s, '-', '|', '+', 1)

	def border(s: String, h: Char, v: Char, c: Char, p: Int): StringBlock = {
		val pt = StringBlock.empty(0, p)
		val ps = StringBlock.empty(p)
		val sb = pt \ (ps + StringBlock(s) + ps) \ pt

		val hl = h.toString * sb.width
		val vl = (v.toString * sb.height).toArray.mkString("\n")

		val hb = StringBlock(c.toString + hl + c.toString)
		val vb = StringBlock(vl)

		hb \ (vb + sb + vb) \ hb
	}

	def empty(): StringBlock = empty(0)

	def empty(x: Int): StringBlock = empty(x, 1)

	def empty(x: Int, y: Int): StringBlock = new StringBlock(Array.fill(y)(" " * x))

}


/**
 * Represents a 2d block of string
 *
 * @author Dominik Gabi
 * @version 0.1
 */
class StringBlock(val lines: Array[String]) {
	require(lines.length > 0)

	/**
	 * The width of the block
	 */
	val width: Int = lines.max(new Ordering[String]() {
		override def compare(x: String, y: String): Int = x.length.compare(y.length)
	}).length

	/**
	 * The height of the block
	 */
	val height: Int = lines.length

	/**
	 * The lines expanded with " " to the full width of the block
	 *
	 * @return The lines
	 */
	def padded: StringBlock = {
		new StringBlock(lines.map(l => l + (" " * (width - l.length))))
	}

	/**
	 * Centers the current
	 *
	 * @param w The new width
	 */
	def centered(w: Int) = {
		val l = (w - width) / 2
		val r = width - l
		StringBlock.empty(l) + this + StringBlock.empty(r)
	}

	def +(s: String): StringBlock = this + StringBlock(s)

	/**
	 * Concatenates two blocks horizontally
	 *
	 * @param b The other block
	 * @return The concatenation of the two blocks
	 */
	def +(b: StringBlock): StringBlock = {
		val combined = for {
			(l, r) <- padded.lines.zipAll(b.lines, " " * width, "")
		} yield l + r
		new StringBlock(combined)
	}

	/**
	 * Concatenates two blocks vertically
	 *
	 * @param b The other block
	 * @return The concatenation of the two blocks
	 */
	def \(b: StringBlock): StringBlock = new StringBlock(lines ++ b.lines)

	/**
	 * Concatenates two blocks aligned vertically
	 */
	def \\(b: StringBlock): StringBlock = {
		if (width < b.width) {
			centered(b.width) \ b
		} else {
			this \ b.centered(width)
		}
	}

	implicit def toStringBlock(s: String): StringBlock = StringBlock(s)

	override def toString: String = lines.mkString("\n")

}