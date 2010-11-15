package Examples.Chalice2

class C {
	
	var F : Any = 1;
	var G : Any = null;
	def n() = {
		var tmp=F;
		F=G;
		G=tmp;
	}

}
