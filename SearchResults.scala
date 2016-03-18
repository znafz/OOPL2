package searchResults
import query._
import page.IndexedPages

class SearchResults(query:Query, pages:IndexedPages, val results:Iterable[(Double,String)]){

	def printTop(n:Int):Unit = {
		//SearchResults should also have a printTop(n: Int): Unit methodthat prints
		// the top n results to the console, one on each line, first the score, 
		//then the URL [2 pts].
		for(res <- results.slice(0,n)){ if(res._1 > 0.0) println(s"${res._1} : ${res._2}")}

	}
	

	def this(query:Query, pages:IndexedPages) = {
		this(query, pages, {
		//SearchResults should have a   results method [2 pts] that takes no 
		//arguments and returns an Iterable[ (Double, String) ]. The String 
		//in each Tuple is the URL of the web page and the Double is the score
		// for that web page. The Iterable should be sorted in decreasing order
		// of the scores (i.e., highest scoring web page first) [2 pts]. To 
		//achieve this ordering, you can use the sortWith method. 

		//scoring by number of occurences on page for terms vs total words on page
		var ret = Vector[(Double, String)]()


		// weight the query terms if necessary
		var termWeightsMap = scala.collection.mutable.Map[String,Double]()
		query match {
			case q:WeightedQuery =>{
				var counter:Int = 0
				val weights = q.weights.toList
				for(term <- query.items){
					termWeightsMap(term) = weights(counter);
					counter = counter + 1
				}
			}
			case _ => query.items.foreach{termWeightsMap(_) = 1.0}
		}
		
		for (page <- pages){
			var score = 0.0
			for(term <- query.items){

				score += ((page.numOccurences(term) * termWeightsMap(term))/page.terms.size) * scala.math.log(pages.size/(1/* preventing division by zero*/+pages.numContaining(term)))
			}
			ret = ret :+ (score, page.url)
		}
		ret.sortWith( _._1 > _._1 )

	})

	}
}