package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || r == 1 || c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def checkSubString(remainingChars: List[Char], remainInSubString: Int): Int = {
      if (remainInSubString < 0) remainInSubString
      else if (remainingChars.isEmpty) remainInSubString
      else if (remainingChars.head == '(') checkSubString(remainingChars.tail, remainInSubString + 1)
      else if (remainingChars.head == ')') checkSubString(remainingChars.tail, remainInSubString - 1)
      else checkSubString(remainingChars.tail, remainInSubString)
    }

    if (checkSubString(chars, 0) != 0) false
    else true
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countByCoins(remainingMoney: Int, remainingCoins: List[Int]): Int = {
      if (remainingCoins.isEmpty) 0
      else if (remainingMoney < 0) 0
      else if (remainingMoney == 0) 1
      else countByCoins(remainingMoney, remainingCoins.tail) +
        countByCoins(remainingMoney - remainingCoins.head, remainingCoins)
    }

    countByCoins(money, coins)
  }
}
