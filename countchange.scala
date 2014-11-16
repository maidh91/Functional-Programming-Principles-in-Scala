object CountChange {
  
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(capacity: Int, changes: List[Int]): Int = {
    		(capacity, changes) match {
    		  case (0, Nil) => 1
    		  case (_, Nil) => 0
    		  case (x, _) if x < 0 => 0
    		  case (c, x::xs) => count(c, xs) + count(c-x, x::xs)
    		}
    }
    
    count(money, coins.sortWith(_.compareTo(_) < 0))
  }
  
  println(countChange(10, List(1,2)))
}
