#Script for pulling pubmed data into bibliometrix dataframe for analysis
#Currently only altering the "pubmed2df" function in environment via trace call, could make this permanent
#pubmed2df must be edited to use the "CitedNew" function for the TC field as the original function ("Cited")
#   overqueries pubmed leading to termination of the script (see CitedNew for details)
#User must update query currently but maybe could turn this into function to allow user queries as input

library(bibliometrix)
library(RISmed)
library(tidyverse)
trace(pubmed2df,edit=T)
View(pubmed2df)


query = "protein folding[MeSH]" #user-defined query here



qsum = EUtilsSummary(query, retmax=100,mindate=2015,maxdate=2020)
D = EUtilsGet(qsum)
M = pubmed2df(D)

M = transmute(M,TC = as.integer(TC)) #convert total citation column into integers for math

###Pushing queries to pubmed###
#searchterms would be pulled from citation data or from WoS/Scopus dataframes
#can ensure one return by playing with retmax AND using multiple search fields ([Auth],[Titl],[MeSH],[ID],etc.)
searchterms = c("Klein SA[Auth]","Kolbe A[Auth]","Barrick D[Auth]","Shea MA[Auth]","protein folding[MeSH]")

srchstore = c()
for (x in searchterms) {
  tmpsrch = entrez_search(db="pubmed",term=x, retmax = 99999) #this appears to be the max val for retmax
  srchstore = append(srchstore,tmpsrch$ids)
  print(length(tmpsrch$ids))
}
