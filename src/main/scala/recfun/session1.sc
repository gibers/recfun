object lecture22 {

  def sum(f:Int => Int): (Int,Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if(a > b) 0
      else f(a) + sumF( a+1 , b)
    }
    sumF
  }
  def sumC(f:Int => Int)(a:Int,b:Int):Int = {
    if(a>b) 0
    else f(a)+sumC(f)(a+1, b)
  }

  def fact(x:Int) :Int = {
    if(x==0) 1
    else x*fact(x-1)
  }

  val res = sum( x => x )
  val res1 = res(3,5)
  val res2 = sum(x=>x)(3,5)
  val res3 = sum(x=>x*x)(3,5)
  val res4 = sum(fact)(3,5)
  val res5 = sumC(x=>x)(3,5)

  val dee = new List()


}
