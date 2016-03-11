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
		Vector((1.0,"bad s#1t"))
	}
	def printTop(n:Int):Unit = {
		//SearchResults should also have a printTop(n: Int): Unit methodthat prints
		// the top n results to the console, one on each line, first the score, 
		//then the URL [2 pts].
	}
}