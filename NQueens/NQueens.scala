
object NQueens {


	def placeQueens(n: Int, size: Int): List[List[(Int, Int)]] = n match {
	case 0 => List(Nil)
	case _ => for {
		queens <- placeQueens(n -1, size)
		y <- 1 to size
		queen = (n, y)
		if (isSafe(queen, queens))
	} yield queen :: queens
	}

	def isSafe(queen: (Int, Int), others: List[(Int, Int)]): Boolean = {
			for ( q <- others 
					if q._1 == queen._1 || q._2 == queen._2 ||(q._1-queen._1).abs == (q._2-queen._2).abs) {
				return false
			}
			return true
	}



	def main(args: Array[String]) {
		val size: Int = args(0).toInt
		println(" solutions: " + solutions.size)
		for (s <- solutions) {
			for (queen <- s; x <- 1 to size) {
				if (queen._2 == x) print("Q ") else print("X ")
				if (x == size) println()
			}
			println();
		}
		val solutions = placeQueens(size, size)
	}		
}