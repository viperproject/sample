package Examples

class Peter {

  def double(i : Int) : Int = 2*i
  
  def square(i : Int) : Int = i*i
  
  def abs(i : Int) : Int = if(i>=0) i else -i
    
  def decrement(i : Int) : Int = i-1
  
  def foo(f : Int => Int, i : Int)= i/f(i)
  
}

