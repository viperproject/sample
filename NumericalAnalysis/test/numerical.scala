
class SimpleExamples {
	def ex1() = {
		var i : Int = 1;
		var j : Int = 2;
		j=i+j*2+10;
		i=j*j;
	}
	def ex2(i : Int) = {
		var j : Int = 0;
		if(i>0)
			j=i;
		else j=0-i;
	}
	def ex3(i : Int) = {
		var j : Int = 1;
		var counter : Int = 0;
		while(counter<i) {
			counter=counter+1;
			j=j*2;
		}
	}
	def ex4(i : Int, j : Int) = {
		var k : Int = 0;
		if(i>j)
			k=i;
		else k=j;
	}
	def ex5(i : Int) = {
		var x : Int = 0;
		var y : Int = 3;
		var w : Int = 5;
		var t : Int = 1;
		var f : Int = 0;
		x = i + 1;
		y = x;
		w = y;
		if (x > 1) {
			w = w + t;
		} else {
			w = w + f;
		}
		w = i;
	}
}
