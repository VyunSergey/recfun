package recfun

import scala.annotation.tailrec

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
      if (r < c) 0
      else if (c == 0) 1
      else if (c == 1) r
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def balanceIteration(chars: List[Char], result: Int): Boolean = {
        if (chars.isEmpty)
          result == 0
        else if (result < 0)
          false
        else if (chars.head == '(')
          balanceIteration(chars.tail, result + 1)
        else if (chars.head == ')')
          balanceIteration(chars.tail, result - 1)
        else
          balanceIteration(chars.tail, result)
      }
      balanceIteration(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeIteration(money: Int, coins: List[Int]): Int = {
        if (money < 0 || money > 0 && coins.isEmpty)
          0
        else if (money == coins.head)
          1
        else
          countChangeIteration(money - coins.head, coins) + countChangeIteration(money, coins.tail)
      }
      countChangeIteration(money, coins.sorted)
    }
  }
