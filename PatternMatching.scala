package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("broken(example))(".toList))  
    println(balance("(working(example))".toList))
    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
  	if (c == 0 || r == 0 || r == c) 1
  	else
	 	  pascal(c, r - 1) + pascal(c - 1, r - 1)
  }                                             
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
 	  def bal(chars: List[Char], opens: Int): Boolean =
 	  if (opens < 0) false
 	  else
 	  chars match {
	 		case Nil                  => if(opens == 0) true else false
	 		case x::xs  if (x == '(') => bal(xs, opens + 1)
	 		case x::xs  if (x == ')') => bal(xs, opens - 1)
	 		case _::xs                => bal(xs, opens)
 		}
 		bal(chars, 0)
 	}         
  
 
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    coins match {
    	case Nil                      => 0
    	case x::xs if(money == x)     => countChange(money, xs) + 1
    	case x::xs if(money - x <  0) => countChange(money, xs)
    	case x::xs if(money - x >  0) => countChange(money - x, coins) + countChange(money, xs)
    }
 }
