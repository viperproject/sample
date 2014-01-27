class AVP2010 {

	def example1() = {
		//proprieta' semplice: tutti gli accessi all'array sono corretti (indice >= 0 && indice < array.length)
		//proprieta' media: tutti i valori memorizzati all'interno dell'array sono positivi
		var arr : Array[Int] = new Array[Int](10);
		arr(0)=5;
		arr(1)=3;
		var i : Int = 2;
		while(i < 10) {
			arr(i)=i*2;
			i=i+1;
		}
	}
	
	def example2(arr : Array[Int]) = {
		//proprieta' semplice: tutti gli accessi all'array sono corretti
		//if(arr.length > 10 ) {
			arr(0)=5;
			arr(1)=3;
			var i : Int = 2 ;
			while(i < 10) {
				arr(i)=i*2;
				i=i+1;
			}
		//}
	}
	
	def example3(arr : Array[Int]) = {
		//proprieta' media: tutti gli accessi all'array sono corretti
		//proprieta' difficile: tutti i valori memorizzati all'interno dell'array sono uguali zero
		var i : Int = 0;
		while(i < arr.length) {
			arr(i)=0;
			i=i+1;
		}
	}
	
	def example4(arr : Array[Int]) : Int = {
		//proprieta' media: tutti gli accessi all'array sono corretti
		//proprieta' media: il valore ritornato e' >=0
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
		//proprieta' media: tutti gli accessi all'array sono corretti
		//proprieta' difficile: il valore ritornato e' >=0
		var result : Int = 0;
		var i : Int = 0;
		while(i < arr.length) {
			if(arr(i)>=0)
				result=result+arr(i);
			else result=result-arr(i);
			i=i+1;
		}
		return result;
	}
	
	def example6(arr : Array[Int]) = {
		//proprieta' media: tutti gli accessi all'array sono corretti
		var i : Int = 0;
		while(i < arr.length) {
			var a : Int = 0;
			var j = arr.length-1;
			while(j > i) {
				a=arr(j)+arr(i)
				j=j+1;
			}
			arr(i)=a;
			i=i+1;
		}
	}
	
	def example7(size : Int) : Array[Int] = {
		//proprieta' media: tutti i valori all'interno dell'array sono >=0
		//proprieta' difficile: il valore memorizzato all'indice i e' i
		var arr : Array[Int] = new Array[Int](size);
		var i : Int = 0;
		while(i < arr.length) {
			arr(i)=i;
			i=i+1;
		}
		return arr;
	}
	
	def example8(size : Int) : Array[Int]= {
		//proprieta' media: tutti gli accessi all'array sono corretti
		//proprieta' difficile: negli indici pari abbiamo 0, negli indici dispari abbiamo 1
		var arr : Array[Int] = new Array[Int](size);
		var i : Int = 0;
		while(i < arr.length/2) {
			arr(2*i)=0;
			arr(2*i+1)=0
			i=i+1;
		}
		return arr;
	}
	
	def example9(arr : Array[Int])= {
		//proprieta' difficile: tutti i valori memorizzati all'interno dell'array sono >=0
		var i : Int = 0;
		while(i < arr.length) {
			if(arr(i)<0)
				arr(i)=0-arr(i);
			i=i+1;
		}
	}
		
}
