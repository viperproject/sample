package Examples.Chalice2

import Chalice._;

class CellDefault {
	var x : Int = 0;
/*
	def init(v : Int) = {
		x=v;
	}
	
	def set(v : Int) = {
		x=v;
	}

	def increment() = {
		x=x+1;
	}
	
	def dispose() = {
		//I should add free!
	}
	
	def get() : Int = x;
	*/
}

class Interval {
	var left : CellDefault = null;
	var right : CellDefault = null;
	/*
	def init(l : Int, r : Int) = {
		left = new CellDefault();
		left.init(l);
		right = new CellDefault();
		right.init(r);
	}
	def setLeft(l : Int) = {
		left.set(l);
	}
	
	def setRight(r : Int) = {
		right.set(r);
	}
	*/
	def shift(v : Int) = {
		//left.set(left.get()+v);
		//right.set(right.get()+v);
		left.x=left.x//+v;
		right.x=right.x//+v;
	}
	/*
	def getLeft() : Int = {
		left.get();
	}
	
	def getRight() : Int = {
		right.get();
	}*/
}