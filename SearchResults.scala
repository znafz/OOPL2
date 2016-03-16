package searchResults
import query.Query
import page.IndexedPages

class SearchResults(query:Query, pages:IndexedPages){
	def results:Iterable[(Double,String)] = {
		//SearchResults should have a   results method [2 pts] that takes no 
		//arguments1 and returns an Iterable[ (Double, String) ]. The String 
		//in each Tuple is the URL of the web page and the Double is the score
		// for that web page. The Iterable should be sorted in decreasing order
		// of the scores (i.e., highest scoring web page first) [2 pts]. To 
		//achieve this ordering, you can use the sortWith method. 

		//scoring by number of occurences on page for terms vs total words on page
		var ret = Vector[(Double, String)]()
		for (page <- pages){
			var score = 0.0
			for(term <- query.items){
				score += page.numOccurences(term)/page.terms.size
			}
			ret = ret :+ (score, page.url)
		}
		ret.sortWith( _._1 > _._1 )
	}
	def printTop(n:Int):Unit = {
		//SearchResults should also have a printTop(n: Int): Unit methodthat prints
		// the top n results to the console, one on each line, first the score, 
		//then the URL [2 pts].
		for(res <- results.slice(0,n)){println(s"${res._1} : ${res._2}")}

	}
}