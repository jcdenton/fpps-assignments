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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 && r == 0) 1 else if (c < 0 || c > r) 0 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], count: Int, waitingForClosingParenthesis: Boolean): Boolean =
      if (chars.isEmpty) (!waitingForClosingParenthesis && (count == 0))
      else if (chars.head == '(') balance(chars.tail, count + 1, true)
      else if (chars.head == ')') balance(chars.tail, count - 1, false)
      else balance(chars.tail, count, waitingForClosingParenthesis)

    balance(chars, 0, false)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val ways = Array.fill(money + 1)(0)
    ways(0) = 1
    coins.foreach(coin =>
      for (j <- coin to money)
        ways(j) = ways(j) + ways(j - coin)
    )
    ways(money)
  }
}
