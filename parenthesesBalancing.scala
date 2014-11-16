object ParenthesesBalancing {

  def balanceCount(chars: List[Char], open: Int): Boolean = {
    (chars, open) match {
      case (Nil, 0) => true
      case (Nil, _) => false
      case (h::ts, _) => {
        h match {
          case '(' => balanceCount(ts, open+1)
          case ')' => balanceCount(ts, open-1)
          case _ => balanceCount(ts, open)
        }
      }
    }
  }
  
  def balance(chars: List[Char]): Boolean = {
    balanceCount(chars, 0)
  }
  
  println(balance("(if (zero? x) max (/ 1 x))".toList))
}
