package recfun

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
    if(c == 0) 1
    else if (r<c) 0
    else pascal(c-1,r-1) + pascal(c, r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    var cmp = 0
    for(e <- chars) {
      if(e == '(') cmp +=1
      else if(e == ')') {
        cmp-=1
        if(cmp<0) return false
      }
    }
    (cmp == 0)
  }


  /**
    * Exercise 3
    */
  def countChange (money:Int, coins: List[Int]): Int = {
    var cmp = 0
    if (money == 0) 1
    else if(coins.isEmpty || coins.min > money) 0
    else {
      cmp += countChange(money, coins.tail)
      val q =money/coins.head
      for(e <- 1 to q) {
        cmp += countChange(money-e*coins.head, coins.tail)
      }
      cmp
    }
  }

  def sum(f:Int => Int): (Int,Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if(a < b) 0
      else f(a) + sumF( a+1 , b)
    }
    sumF
  }

  def autre(): Unit = {
    val res = sum( (x:Int) => x )
    val res1 = res(3,5)
    println (res1)
  }

}

