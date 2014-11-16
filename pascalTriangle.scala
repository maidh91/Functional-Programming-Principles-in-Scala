object PascalsTriangle {
	
	val nextRow: (Array[Int] => Array[Int]) = (p) => {
	  var t = 0 +: p :+ 0;
	  (for {
	    i <- (0 to t.length-2)
	   } yield(t(i) + t(i+1))).toArray
	} 
  
	def pascalTail(c: Int, r: Int, p: Array[Int])(next: Array[Int] => Array[Int]): Int = {
		if (r == 0) p(c)
		else pascalTail(c, (r-1), next(p))(next)
	}
	
	def tailRecPascal(c: Int, r: Int): Int = {
		  pascalTail(c, r, Array(1))(nextRow)
	}
	
	def pascal(c: Int, r: Int): Int = {
	 if (c == 0 || c == r) 1
	 else pascal(c - 1, r - 1) + pascal(c, r - 1)
	}
	
	val p00 = println(pascal(0,0))
	val p01 = println(pascal(0,1))
	val p11 = println(pascal(1,1))
	val p02 = println(pascal(0,2))
	val p12 = println(pascal(1,2))
	val p22 = println(pascal(2,2))
	
	val p03 = println(pascal(0,3))
	val p13 = println(pascal(1,3))
	val p23 = println(pascal(2,3))
	val p33 = println(pascal(30,60))
}
