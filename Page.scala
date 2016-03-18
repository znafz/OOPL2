package page
import org.apache.http._
import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.client.utils._
import org.apache.http.message._
import org.apache.http.params._
import org.apache.http.client.HttpResponseException
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
	//stop words taken from http://xpo6.com/list-of-english-stop-words/
	val STOP_WORDS = List("a", "about", "above", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the")

	//copying getTerms and fetch to the page instead of in the searchengine object
	def getTerms(htmlToClean:String, func: String=>Boolean):List[String] = {
		val terms = htmlToClean.split("[^a-zA-Z0-9]")
		val clean = terms.toList.par.filter({
			case x if x.length < 2 => false
			case x if STOP_WORDS.contains(x) => false
			case _ => true
			}).toList
		clean.filter(func)
	}
	def fetch(URL: String):String = {
		var responsebody : String = ""
		val httpget = new HttpGet(s"${URL}") //(url + "?" + query)
		try {
			responsebody = new DefaultHttpClient().execute(httpget, new BasicResponseHandler())
			responsebody
		} catch {
			case e: org.apache.http.client.HttpResponseException => {
				//e.printStackTrace()
				println("http exception occured at " + URL)
				responsebody = ""
				e.toString()
			}
		}
	}

	val html = fetch(url)
	val terms = getTerms(html, {x=>x.length > 1})
	val links = {
		// See http://www.mkyong.com/regular-expressions/how-to-extract-html-links-with-regular-expression/ for explanation of regex
		val aTagRegex = """(?i)<a([^>]+)>(.+?)</a>""".r
		val urlRegex = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
		
		val opts = for ( a <- aTagRegex findAllMatchIn html ) yield urlRegex.findFirstMatchIn(a.toString)
		
		val hrefs = opts collect { case Some(x) => x group 1 }
		
		// remove leading and trailing quotes, if any
		val cleaned = hrefs.toStream.par map{ _.stripPrefix("\"").stripPrefix("\'").stripSuffix("\"").stripPrefix("\'").split("\\?", -1)(0).split("#", -1)(0) } filter { ! _.startsWith("javascript") } 
		
		// Use Java's URL class to parse the URL
		//   and get the full URL string (including implied context)
		val contextURL = new java.net.URL(url)
		
		def getURL(x: String) = {
          var result = ""
          try {
            result = new java.net.URL(contextURL, x).toString()
          }
          catch {
            case e: java.net.MalformedURLException => Unit
          }
          result
        }
        
        (cleaned map { getURL(_) } ).filter(_.length > 0).toList

	}

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