package page
import pageSummary.PageSummary
import query.Query
import searchResults.SearchResults
import traits.Weighted
import scala.collection.mutable.ArrayBuffer



case class Page(val url: String){
	//Create a Page class [1 pts]. It should have a method url
	// (or public val url) that evaluates to the URL of the web 
	//page [1 pts]. You can decide what else goes in the class.
	
}

class IndexedPages(pages: ArrayBuffer[Page]) extends Iterable[Page]{
	//Create an IndexedPages class [1 pts] that holds a group of Pages. 
	//IndexedPages should extend Iterable[Page] [1 pts] by overriding 
	//the iterator method [2 pts]. You should use a mutable collection
	// in the class so you can add pages (discussed later).

	override def iterator = pages.iterator
	def numContaining(word:String):Double = {
		//In IndexedPages, define a method numContaining(word: String):
		//Double that returns the number of pages that contain the given 
		//word as a whole word at least once [9  pts]. "As a whole word"
		// means that a page containing only snowflake would not count 
		//as containing snow, because the whole word does not match
		0.0
	}

	def search(q:Query):SearchResults = {
		//The search results come from pairing a Query with IndexedPages. Thus, 
		//you should add a method search(q: Query) to the IndexedPages class that
		// returns a SearchResults object [2 pts]. In the case where q is an instance
		// of WeightedQuery, the search results should be adjustedaccordingly [4 pts];
		// the details of how this happens depend upon the scoring method you pick.
		new SearchResults(q, this)
	}
}

class WeightedIndexedPages(pages: ArrayBuffer[Page]) extends IndexedPages(pages) with Weighted[Page]{
	val weightingFn = (x:Page)=>1.0/x.url.length //not sure if this is valid or not
	val items = pages //not sure if this is correct either
	override def search(q:Query) : SearchResults = {
		//most of this is given to us in the spec
		val beforeWeights: SearchResults = super.search(q)    
		val oldScores = beforeWeights.results.unzip._1    
		val unnormScores = oldScores.zip(weights).map { (x) => (x._1 * x._2) }    
		//Multiplying by weights may change the scale of the scores    
		//Thus, newScores is unnormScores by the total of the unnormScores    
		// (This is called "normalizing" the scores)    
		val total = unnormScores.foldLeft(0.0) {_+_}    
		val newScores = unnormScores.map { _ / total }    
		// TODO: create and return adjusted SearchResultsfrom newScores [4 pts]
		beforeWeights //<- this is only here so it compiles

	}
}