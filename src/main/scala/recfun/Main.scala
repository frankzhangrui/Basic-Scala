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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c==0) | (r ==0) | (c==r))
      1
    else
      pascal(c-1,r)+ pascal(c-1,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val filteredChars=chars.filter( x=>x=='(' | x==')')
    if (filteredChars.size%2!=0)
      false
    else
      isBalanced(filteredChars)
  }

  def isBalanced(chars: List[Char]): Boolean ={
    if(chars.isEmpty)
      true
    else
      chars.head=='(' & chars.last==')' & isBalanced(chars.drop(1).dropRight(1))
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0)
      0
    else if (coins.isEmpty)
      if (money == 0) 1 else 0
    else
      countChange(money-coins.head,coins.tail)+countChange(money,coins.tail)
  }
}
