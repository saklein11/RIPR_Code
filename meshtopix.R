#This is a R code snippet that grabs out mesh terms and major topic status using R's XML tools
#package. It is a heinous crime against nature but it works and that is the beginning of all
#beautiful things.

###INPUT###
#For this script, a fetch item is used for simplicity.
#To scale code up, we should be able to feed a vector of PMIDs to the below code and
#run it to get information on every article with mesh info from pubmed.
#If the current code uses a fetch option, we should be able to integrate this into that f(x)
#and run it as part of the series
meshtopix <- function (pmidlist) {
  
  #Query PMID in list of pmids provided to meshtopix function
  for (pmid in pmidlist) {
    print(paste("Mesh Terms for: ",pmid))
    f1 = entrez_fetch(db="pubmed",id=pmid,rettype="xml",parsed=T)
    m1 = xpathSApply(f1,"//MeshHeadingList/MeshHeading") #pulls every mesh term
    
    if (xmlSize(m1) == 0) { #provide NAs for articles without mesh terms
      print(NA)
      next
    }
    
    #Dive into pmid's parsed XML from fetch to find mesh terms
    for (x in 1:xmlSize(m1)) {
      if (xmlSize(m1[[x]]) > 1) { #if loop to catch mesh terms with qualifiers
        for (y in 2:xmlSize(m1[[x]])) { #iterate through qualifiers
          print(paste(xmlValue(m1[[x]][[1]]),xmlValue(m1[[x]][[y]]))) #combine descriptor/qualifier
          print(xmlAttrs(m1[[x]][[y]])[2]) #Y/N major topic status
        } #end of qualifier iterating loop
      } #end of qualifier catching if loop
      else { #catches all terms without qualifiers
        print(xmlValue(m1[[x]][[1]]))
        print(xmlAttrs(m1[[x]][[1]])[2])
      } #end of else
    } #end of x:xmlsize for loop
  } #end of pmid:pmidlist for loop
} #end of meshtopix function definition


