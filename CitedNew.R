#Replacement for original "Cited" function from RISmed package
#API key allows for enhanced query rate
#Sys.sleep forces lag into script to prevent over-querying
#   NOTE: Need to experiment with calls to figure out script runtime to optimize sleep time
#NOTE: Should see if there's a way to exclude less cited papers to avoid pulling non-useful papers

CitedNew <- function(object){

  if(class(object)[1] == "EUtilsSummary" && object@db != "pubmed")
    stop("Cited is only available for queries of the Pubmed database")
  
  f <- function(id){
    #include to ensure not over-querying pubmed server.
    #A quick download takes ~0.05s so setting sleep value to 0.1s ensures an overhang of ~0.5s if all 10 queries fast
    Sys.sleep(0.1)
    
    #obtained from settings of NCBI acct
    #w/o key, vmax = 3/s; w/ key, vmax = 10/s; w/ key + request to help desk > 10/s
    sk_key = "dcff69acdc62008c90b1354b0ac141dc0808" 
    
    base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?retmode=xml&dbfrom=pubmed&id=ID&cmd=neighbor&api_key=KEY"
    base <- sub("ID", id, base)
    base <- sub("KEY",sk_key,base)
    
    the_url <- url(description = base)
    on.exit(close(the_url))
    
    lines <- readLines(the_url)
    
    citedstart <- grep("pubmed_pubmed_citedin", lines)
    
    
    if(length(citedstart) == 0){
      return(0)
    }
    else{
      citedend <- grep("pubmed_pubmed", lines)
      citedend <- min(citedend[citedend > citedstart])
      
      tags <- grep("<Id>", lines)
      tags <- tags[tags > citedstart & tags < citedend]
      
      if(any(tags)){
        hits <- sub("(.*<ID>)([0-9]+)(.*)","\\2",lines[tags])
        hits <- unique(hits)
        length(hits[hits != id])
      }
      else{
        0
      }	
    }
  }
  
  sapply(FUN = f, object@PMID)
}