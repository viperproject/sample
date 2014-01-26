package Examples

class Simple {
		def CreateString(i : Int, lowerBound : String) : Unit = {
			//EXAMPLE 1
			/*var x = "a";
			var r = "]";
			while(i > 0) {
				x = "[" + x + r;
			}*/
			
			//EXAMPLE 2
			/*var q : String = "SELECT * FROM address";
			if (i != 0)
				q = q + "WHERE studentId=" //+ i.toString;*/
			
			
			//EXAMPLE 3
			/*var query = "SELECT '$' || (RETAIL/100) FROM INVENTORY WHERE ";
			if (lowerBound != null)
				query = query + "WHOLESALE > " + lowerBound + " AND ";
			var perishableTypeCode = "SELECT TYPECODE, TYPEDESC FROM TYPES WHERE " +
					"NAME = 'fish' OR NAME = 'meat'";
			query = query + "TYPE IN (" + perishableTypeCode + ");";
			return query;*/
			
			//EXAMPLE 4
			/*var sql1 : String = "";
			var sql2 : String = "";
			sql1 = "SELECT";
			sql1 = sql1 + " " + lowerBound;
			sql1 = sql1 + " " + "FROM";
			sql1 = sql1 + " " + lowerBound;
			sql2 = "UPDATE";
			sql2 = sql2 + " " + lowerBound;
			sql2 = sql2 + " " + "SET";
			sql2 = sql2 + " " + lowerBound + " = " + lowerBound;*/
			// lowerBound is used as placeholder for an unknown string
			// in the first 4cases, it should be args.apply(i), where i
			// is a specific index; in the last one it should be the result 
			// of the execution of sql1 query
			
			
			//EXAMPLE 5
			var result = "";
			while(i > 0) {
				//if (lowerBound != "process" && lowerBound != "password2") {
				       result = result + lowerBound + " = " + lowerBound;
				       if (i < 10)
				              result = result + ", ";
				//}
			}
			var query = "UPDATE pblguestbook_config SET ";
			query = query + result;
		 	
		}
}

/*object Tricky {
	def bar(n : Int, k : Int, op : String) : String = {
		if (k == 0)
			return "";
		
		return op + n + ")" + bar(n-1, k-1, op) + " ";
	}
	
	def foo(n : Int) : String = {
		var b : String = new String();
		if(n<2) 
			b = b + "(";
		for(i <- 0 until n)
			b = b+ "(";
		var s : String = bar(n-1, n/2-1, "*").trim();
		var t : String = bar(n-n/2, n-(n/2-1), "+").trim();
		return b.toString + n + (s+t);
	}
	
	def main(args : Array[String]) : Unit = {
		var n : Int = 5//new Random().nextInt();
		var m = foo(n);
		Console.println(m);
	}
}*/
