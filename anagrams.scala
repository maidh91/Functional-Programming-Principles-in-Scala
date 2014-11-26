package forcomp

import common._

object Anagrams {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = 
    w.groupBy(c => c)
	  .map(g => g._1 -> g._2.length)
	  .toList
	  .sortWith((a, b) => a._2 < b._2)


  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString(""))

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy((g) => wordOccurrences(g)) withDefaultValue List()

  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences.get(wordOccurrences(word)).getOrElse(List())
  }
    
  def combinations(occurrences: Occurrences): List[Occurrences] = {
	  occurrences match {
	    case Nil => List(List())
	    case (char, count)::hs => {
		    val res = (for {
		      comb <- combinations(hs)
		      i <- 1 to count
		    } yield (char -> i) :: comb).toList
	    
		    res ::: combinations(hs)
	    }
	  }
  }
  

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    	(
    	  (y.foldLeft(x.toMap))((map, elem) => { 
    	    	if (map.apply(elem._1) - elem._2 > 0)
    	    	  map.updated(elem._1, map.apply(elem._1) - elem._2)
	    	    else
	    	      map - elem._1
    	  })
	    ).toList
  }
  
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    	def sentenceAnagramsAgg(occ: Occurrences): List[Sentence] = {
    	  occ match {
    	    case Nil => List(List())
    	    case xs => (for {
    		  comb <- combinations(occ)
    		  word <- dictionaryByOccurrences.getOrElse(comb, Nil)
    		  rest <- sentenceAnagramsAgg(subtract(occ, comb))
    		  if (word != List())
    	    } yield word :: rest).toList
    	  }
    	  
    	}
    	
    	sentenceAnagramsAgg(sentenceOccurrences(sentence))
  }
  
     
  def main(args : Array[String]) : Unit = {
    val result = combinations(List(('a', 1), ('b', 1), ('c', 1)))
   	println(result.toString())
    	
   	val subtractVal = subtract(
		List(('a', 1), ('b', 1), ('c', 1)),
		List(('a',1))
    )
    println(subtractVal)
    	
    val occ = dictionaryByOccurrences;
    val sentenceAna = sentenceAnagrams(List("I", "Love"))
    println(sentenceAna)
   }
}
