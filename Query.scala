package query
import traits.Weighted

case class Query(val items: Iterable[String]){

}


//earlier words are weighted higher than later words
class WeightedQuery(override val items: Iterable[String]) extends Query(items) with Weighted[String]{
	//You can pick a default weighting scheme for WeightedQuery, but it has to be different than 
	//uniform. That is, terms should have unequal weights [2 pts]
	val weightingFn = (x:String)=> {
		var ret = 0.0
		items.zipWithIndex.foreach(
			item => if(item._1 == x) ret = 1.0/(item._2 + 1)
		)
		ret
	}
}