package pageSummary

class PageSummary(val url:String, val terms:List[String]) {
	def fracMatching(checkTerm:String):Float = {
		var matches = for(t<-terms) yield if(checkTerm == t) 1 else 0
		var sum = matches.foldRight(0){(x,y)=>x+y}
		return sum.toFloat/terms.size.toFloat
	}

	def fracMatching(checkTerms:List[String]):Float = {
		checkTerms.size  match{
			case 1 => fracMatching(checkTerms(0))
			case x if x > 1 => fracMatching(checkTerms.last) + fracMatching(checkTerms.init)
			case _ => -1.0f
		}
		
	}

}