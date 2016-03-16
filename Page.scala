package page
import org.apache.http._
import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.client.utils._
import org.apache.http.message._
import org.apache.http.params._
import java.net.URL
import query.Query
import query.WeightedQuery
import searchResults.SearchResults
import traits.Weighted
import scala.collection.mutable.ArrayBuffer



case class Page(val url: String){
	//Create a Page class [1 pts]. It should have a method url
	// (or public val url) that evaluates to the URL of the web 
	//page [1 pts]. You can decide what else goes in the class.

	//copying getTerms and fetch to the page instead of in the searchengine object
	def getTerms(html:String, func: String=>Boolean):List[String] = {
		val terms = html.split("[^a-zA-Z0-9]")
		val clean = terms.toList.filter(x => x!= "")
		clean.filter(func)
	}
	def fetch(URL: String):String = {
		val httpget = new HttpGet(s"${URL}") //(url + "?" + query)
		val responsebody = new DefaultHttpClient().execute(httpget, new BasicResponseHandler())
		responsebody
	}

	val terms = getTerms(fetch(url), {x=>x.length > 1})

	def numOccurences(word:String):Double = {
		var sum = 0.0
		for(term <- terms){
			if(term == word) sum += 1.0
		}
		sum
	}
	
}

class IndexedPages(val items: ArrayBuffer[Page]) extends Iterable[Page]{
	//Create an IndexedPages class [1 pts] that holds a group of Pages. 
	//IndexedPages should extend Iterable[Page] [1 pts] by overriding 
	//the iterator method [2 pts]. You should use a mutable collection
	// in the class so you can add pages (discussed later).

	override def iterator = items.iterator
	def numContaining(word:String):Double = {
		var ret = 0.0
		//In IndexedPages, define a method numContaining(word: String):
		//Double that returns the number of pages that contain the given 
		//word as a whole word at least once [9  pts]. "As a whole word"
		// means that a page containing only snowflake would not count 
		//as containing snow, because the whole word does not match
		for(page <- items){	
			if(page.terms.contains(word)) ret += 1.0
		}
		ret
	}

	def search(q:Query):SearchResults = {
		//The search results come from pairing a Query with IndexedPages. Thus, 
		//you should add a method search(q: Query) to the IndexedPages class that
		// returns a SearchResults object [2 pts]. In the case where q is an instance
		// of WeightedQuery, the search results should be adjustedaccordingly [4 pts];
		// the details of how this happens depend upon the scoring method you pick.
		//weighted queries are handled in the SearchResults class for our implementation
		new SearchResults(q, this)
		
	}
}

class WeightedIndexedPages(override val items: ArrayBuffer[Page]) extends IndexedPages(items) with Weighted[Page]{
	val weightingFn = (x:Page)=>1.0/x.url.length 
	override def search(q:Query) : SearchResults = {
		val beforeWeights: SearchResults = super.search(q)    
		val oldScores = beforeWeights.results.unzip._1    
		val unnormScores = oldScores.zip(weights).map { (x) => (x._1 * x._2) }    
		//Multiplying by weights may change the scale of the scores    
		//Thus, newScores is unnormScores by the total of the unnormScores    
		// (This is called "normalizing" the scores)    
		val total = unnormScores.foldLeft(0.0) {_+_}    
		val newScores = unnormScores.map { _ / total }    
		// TODO: create and return adjusted SearchResultsfrom newScores [4 pts]
		var newResults:Vector[(Double, String)] = Vector() ++ newScores.zip(beforeWeights.results.unzip._2)
		newResults = newResults.sortWith( _._1 > _._1 )
		new SearchResults(q, this, newResults)

	}
    
    override def numContaining(word: String): Double = {
        sumIf( { (item: Page) => item.terms.contains(word) } )
    }
}