package traits


trait Weighted[A] {
	val items: Iterable[A]
	val weightingFn: A=>Double

	def weights:Iterable[Double] = {
		//Complete the weights function so it returns the weights of each of the items, in order, 
		//in an Iterable collection [1 pts].
		items.map(weightingFn)
	}
	def totalWeight:Double = {
		//Complete the totalWeight function to return the sum of the weights of all items [1 pts]
		weights.foldLeft(0.0)(_ + _)
	}
	def sumIf(p: A=> Boolean):Double = {
		//Complete the sumIf functionto return the sum of the weights 
		//of the items that satisfy the predicate p[4 pts]. For example,
		// if the items are [3, 4, 5, 6] with weights [0.1, 0.3, 0.5, 0.9], 
		//and p = { _ > 4}, then sumIf would return 0.5 + 0.9 = 1.4
		var sum:Double = 0.0
		val itemsAndWeights = items zip weights
		itemsAndWeights foreach{
			current => if(p(current._1)) sum += current._2
		}
		sum
	}
}

trait Augmentable[A] {
	val items: scala.collection.mutable.Seq[A] with scala.collection.generic.Growable[A]
	def add(newItem: A): Boolean = {
		//complete the add method. It should return false if the item is already in the collection 
		//(in which case nothing is added) and true if it was a new item (in which case it is 
		///added to items) [2 pts]
		false
	}
}