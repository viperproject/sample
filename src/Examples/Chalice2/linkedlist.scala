package Examples.Chalice2

import Chalice._;

class NodeCell {
	var next : NodeCell = null;
	var value : Int = 0;
	
	def init(v : Int) = {
		next=null;
		value=v;
		Chalice.fold(this, "valid");
	}
	
	def add(x : Int) : Unit = {
		Chalice.unfold(this, "valid");
		if(next==null) {
			var n : NodeCell = new NodeCell();
			n.init(x);
			next=n;
		}
		else next.add(x);
		Chalice.fold(this, "valid");
	}
	
	def addFirst(x : Int) : NodeCell = {
		var n : NodeCell = new NodeCell();
		n.value=x;
		n.next=this;
		Chalice.fold(n, "valid");
		return n;
	}
	
	
	
}