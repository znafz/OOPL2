import org.apache.http._
import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.client.utils._
import org.apache.http.message._
import org.apache.http.params._
import java.net.URL
import pageSummary.PageSummary
import page._
import scala.collection.mutable.ArrayBuffer
import traits._
import scala.collection.generic.Growable
import query._
import searchResults._

object SearchEngine extends App{
	def fetch(URL: String):String = {
		val httpget = new HttpGet(s"${URL}") //(url + "?" + query)
		val responsebody = new DefaultHttpClient().execute(httpget, new BasicResponseHandler())
		responsebody
	}
	def getLinks( html : String , baseURL : String) : List[String] = {
		// See http://www.mkyong.com/regular-expressions/how-to-extract-html-links-with-regular-expression/ for explanation of regex
		val aTagRegex = """(?i)<a([^>]+)>(.+?)</a>""".r
		val urlRegex = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
		
		val opts = for ( a <- aTagRegex findAllMatchIn html ) yield urlRegex.findFirstMatchIn(a.toString)
		
		val hrefs = opts collect { case Some(x) => x group 1 }
		
		// remove leading and trailing quotes, if any
		val cleaned = hrefs map { _.stripPrefix("\"").stripPrefix("\'").stripSuffix("\"").stripPrefix("\'") } filter { ! _.startsWith("javascript") }
		
		// Use Java's URL class to parse the URL
		//   and get the full URL string (including implied context)
		val contextURL = new java.net.URL(baseURL)
		
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

	def getTerms(html:String, func: String=>Boolean):List[String] = {
		val terms = html.split("[^a-zA-Z0-9]")
		//not sure why, but empty strings were being added
		val clean = terms.toList.filter(x => x!= "")
		clean.filter(func)
	}

	//TODO:
	//Revise your SearchEngine.crawlAndIndex method: 
	//	def crawlAndIndex(startUrl: String, maxPages: Int, mode: String = 
		//"read", weight: Boolean = true): 
	//IndexedPages startUrl and maxPages serve the same purpose as in the
	// previous project.mode should be "read" or   "augment": for "read", 
	//no mixins are needed; for "augment", 
	//Augmentable should be mixed in to the returned object [4 pts].
    
    def crawlAndIndex(startURL: String, maxPages: Int, mode: String = "read", weight: Boolean = true): IndexedPages = {
        var numCrawled = 0
		var URLs = List(startURL) //list of URLS to crawl
		var items = ArrayBuffer[Page]() //list of Pages already crawled or yet to crawl
        
        while (numCrawled < maxPages && URLs.size > 0) {
            val URLtoCrawl = URLs.last
            URLs = URLs.init
            val page = new Page(URLtoCrawl)
            if (!items.contains(page)) {
                items += page
                println(items)
                URLs ++= page.terms
            }
            numCrawled += 1
        }
        
        if (mode == "read") {
            if (weight == true) {
                return new WeightedIndexedPages(items)
            } else {
                return new IndexedPages(items)
            }
        } else if (mode == "augment") {
            if (weight == true) {
            	return new WeightedIndexedPages(items) with Augmentable[Page]
            } else {
                return new IndexedPages(items) with Augmentable[Page]
            }
        } else {
            return new IndexedPages(items) //default
        }
    }
	
	def printBest(query : List[String], pages : List[PageSummary]) = {
		val scores = for(x <- pages) yield (x.url, x.fracMatching(query))
		for (x <- scores.sortBy(_._2).takeRight(5).reverse) println(x._1 + ": " + x._2.toString)
	}
    
	//TEST CODE HERE
	
	/*var pages = new ArrayBuffer[Page]()
	pages += Page("http://www.google.com")
	var test = new IndexedPages(pages)
	println(test.numContaining("google"))*/

	//testing weighted
	/*case class testWeightedTrait(items:List[Int], weightingFn:Int=>Double) extends Weighted[Int]
	val testW = testWeightedTrait(List(6,9,4,20), {x:Int=>1.0/x.toDouble})
	println("weights: " + testW.weights)
	println("totalWeight: " + testW.totalWeight)
	println("sumIf != 20: " + testW.sumIf({_ != 20}))*/

	/*case class testAugmentable(items:ArrayBuffer[Int]) extends Augmentable[Int]
	val buf:ArrayBuffer[Int] with Growable[Int] = ArrayBuffer(1,2,3,4)
	val testA = testAugmentable(buf)
	println(testA.add(1))
	println(testA.add(5))
	println(testA.items)*/

	/*var pages = new ArrayBuffer[Page]()
	pages += Page("http://www.google.com")
	pages += Page("http://www.youtube.com")
	var test = new WeightedIndexedPages(pages)
	println(test.numContaining("google"))*/
    
    crawlAndIndex("http://en.wikipedia.org", 100)

	var q = new WeightedQuery(List("wikipedia","youtube", "asdfasdf", "asdfasdf", "asdfff", "fff", "google"))
	var p = new ArrayBuffer[Page]()
	p += Page("http://www.wikipedia.org")
	p += Page("http://www.youtube.com")
	p += Page("http://www.google.com")
	var test = new WeightedIndexedPages(p)
	var s = test.search(q)
	s.printTop(2)
}



		