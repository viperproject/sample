package Examples;

class AVP2010 {

	def example1() = {
		//basic property: bounds respected
		//advanced property: all the values in the array are positive
		var arr : Array[Int] = new Array[Int](10);
		arr(0)=5;
		arr(1)=3;
		var i : Int =2;
		while(i < 10) {
			arr(i)=i*2;
			i=i+1;
		}
	}
	
	def example2(arr : Array[Int]) = {
		//property: bounds respected
		if(arr.length > 10 ) {
			arr(0)=5;
			arr(1)=3;
			for(i <- 2 to 10)
				arr(i)=i*2;
		}
	}
	
	def example3(arr : Array[Int]) = {
		//property: bounds respected
		for(i <- 0 to arr.length)
			arr(i)=0;
	}
	
	def example4(arr : Array[Int]) : Int = {
		//easy property: bounds respected
		//medium property: result >= 0
		var result : Int = 0;
		var i : Int = 0;
		while(i < arr.length) {
			val temp : Int = arr(i);
			if(arr(i)>=0)
				result=result+arr(i);
			else result=result-arr(i);
			i=i+1;
		}
		return result;
	}
	
	def example5(arr : Array[Int]) : Int = {
		//easy property: bounds respected
		//hard property: result >= 0
		var result : Int = 0;
		for(i <- 0 to arr.length)
			if(arr(i)>=0)
				result=result+arr(i);
			else result=result-arr(i);
		return result;
	}
	
	def example6(arr : Array[Int]) = {
		//easy property: bounds respected
		for(i <- 0 to arr.length) {
			var a : Int = 0;
			for(j <- arr.length to i)
				a=arr(j)+arr(i)
			arr(i)=a;
		}
	}
	
	def example7(size : Int) : Array[Int] = {
		//easy property: \forall index : arr(index)>=0
		//hard property: \forall index : arr(index)==index
		var arr : Array[Int] = new Array[Int](size);
		for(i <- 0 to arr.length)
			arr(i)=i;
		return arr;
	}
	
	def example8(size : Int) : Array[Int]= {
		//easy property: bounds respected
		//hard property: arr(even)==0 && arr(odd)==1
		var arr : Array[Int] = new Array[Int](size);
		for(i <- 0 to arr.length/2) {
			arr(2*i)=0;
			arr(2*i+1)=0
		}
		return arr;
	}
	
	def example9(arr : Array[Int])= {
		//property: \forall index : arr(index)>=0
		for(i <- 0 to arr.length)
			if(arr(i)<0)
				arr(i)=0-arr(i);
	}
		
}