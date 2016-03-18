package searchResults
import query._
import page.IndexedPages
import scala.collection.mutable.ArrayBuffer

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
		
		var totalTermCounts = ArrayBuffer[Double]()//total instances of each term across pages
		var numPageHits = ArrayBuffer[Double]()//number of pages with each term
		var counter = 0 //index of term being checked/ used
		//store a list of total occurences of the term in every page
		for (item <- query.items) {
			totalTermCounts += 0.0
			numPageHits += 0.0
			totalTermCounts(counter) = 1.0//prevent div by 0
			numPageHits(counter) = 1.0//prevent div by 0
			counter += 1
		}
		//have to run this for loop twice for it to work. Once to set totals BEFORE calculating scores, and once to get scores
		for (page <- pages) {
			counter = 0
			for(term <- query.items) {
				totalTermCounts(counter) += page.numOccurences(term)
				if(page.numOccurences(term) > 0) numPageHits(counter) += 1
				counter += 1
			}
		}

		for (page <- pages){
			var score = 0.0
			var numTermsHas: Double = 0.0
			counter = 0
			for(term <- query.items){
				var numOccurencesOfTerm: Double = page.numOccurences(term)
				if(page.numOccurences(term) > 0) numTermsHas += 1
				//score = num term occurences / num overall occurences * /*(1/cube root of page len)*/ (ln(500)/ln(pagelen)) * term weight * (1/num pages that have term) * (num term occurences/ pagelen) * num term occurences * how early the term is hit in the document (first quarter = bonus)
				score += (numOccurencesOfTerm / totalTermCounts(counter)) * /*scala.math.pow((1/page.terms.size.toDouble), 1.toDouble/3)*/ termWeightsMap(term) * (1.toDouble / numPageHits(counter)) * (numOccurencesOfTerm / page.terms.size) * numOccurencesOfTerm * scala.math.pow(page.terms.size.toDouble/(4*page.firstSuccess(term)),0.1)
				//score += ((page.numOccurences(term) * termWeightsMap(term))/page.terms.size) * scala.math.log(pages.size/(1/* preventing division by zero*/+pages.numContaining(term)))
				counter += 1
			}
			//if pagelen < 500, score *= sqrt(pagelen/500) to penalize it
			if(page.terms.size < 500) {
				score *= scala.math.pow((page.terms.size.toDouble / 500), 2)
			} else {//slight penalty to long results
				score *= scala.math.log1p(500.toDouble).toDouble / scala.math.log1p(page.terms.size.toDouble)
			}
			score *= scala.math.pow((numTermsHas/query.items.size), 2)//adjust for not having every term
			ret = ret :+ (score, page.url)
		}
		ret.sortWith( _._1 > _._1 )

	})

	}
}