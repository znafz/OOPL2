package query
import traits.Weighted

class Query(queryWords: Iterable[String]){

}


//not sure about the weighting function on this yet
class WeightedQuery(queryWords: Iterable[String]) extends Query(queryWords) with Weighted[String]{
	//You can pick a default weighting scheme for WeightedQuery, but it has to be different than 
	//uniform. That is, terms should have unequal weights [2 pts]
	val weightingFn = (x:String)=> -1.0 //see above
	val items = queryWords //not sure about this
}