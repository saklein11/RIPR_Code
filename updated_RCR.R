setwd("C:\\Users/allison.kolbe/Desktop/Bibliometrics/")

## load packages
library(bibliometrix)
library(RISmed)
library(iCiteR)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

########################################################
#### Pull data and format for analysis ####
########################################################

####### TO DO: NEED TO DEFINE AND/OR, DEFAULTS TO AND
####### TO DO: ADD ALTERNATE WORKFLOW WITH RAW DATA FILES FROM PUBMED, WOS AND/OR OTHERS
####### TO DO: VALIDATE RISMED QUERIES - THERE ARE SOME REPORTS OF INCONSISTENT RESULTS (if true may want to switch)

## add search options
author <- ""
title <- "NHANES"
abstract <- ""
mesh_keyword <- ""
all_fields <- ""
year <- "2000:2017"

## create query from user-defined fields
query <- paste(ifelse(nchar(author) > 0, paste(author, "[au] ", sep = ""), ""),
               ifelse(nchar(title) > 0, paste(title, "[ti] ", sep = ""), ""),
               ifelse(nchar(abstract) > 0, paste(abstract, "[ab] ", sep = ""), ""),
               ifelse(nchar(mesh_keyword) > 0, paste(mesh_keyword, "[mh] ", sep = ""), ""),
               ifelse(nchar(all_fields) > 0, paste(all_fields, " ", sep = ""), ""),
               ifelse(nchar(year) > 0, paste(year, "[dp]", sep = ""), ""), sep = "")

## check query
query

## Perform esearch
res <- EUtilsSummary(query, type='esearch', db='pubmed', retmax=5000)

## check query and number of results
summary(res)

## pull in data from query
datapull <- EUtilsGet(res)

## just removed the "Cited" function - citation #s can be pulled with iCite
source("new_pubmed2df.R")

## convert to df
biblio <- pubmed2df(datapull)

## pull from iCite
## iCite does 1000 at a time - this divides up your total # of refs into the appropriate number of pulls and combines into one df
n <- 1
iCite_data <- data.frame()
if(nrow(biblio) <= 1000){
  iCite_data <- get_metrics(c(biblio$UT))
} else{
  for(i in 1:c(ceiling(nrow(biblio)/1000)-1)){
    iCite_data <- rbind(iCite_data, get_metrics(c(biblio$UT[n:c(i*1000)])))
    n <- n + 1000
  }
  iCite_data <- rbind(iCite_data, get_metrics(c(biblio$UT[n:nrow(biblio)])))
}

## convert pmids to characters and merge RISmed and iCite pulls on pmids
iCite_data$pmid <- as.character(iCite_data$pmid)
full_biblio <- left_join(biblio, iCite_data, by = c("UT" = "pmid"))

## replace TC column with iCite citation count - necessary for doing stuff with bibliometrix defined functions
full_biblio$TC <- full_biblio$citation_count

## titles from iCite are better/more complete than titles from RISmed pull
full_biblio$TI <- full_biblio$title

## fix mesh term entries where there is "&AMP;" instead of "and"
full_biblio$MESH <- gsub("&AMP;", "AND", full_biblio$MESH)


########################################################
#### Quick-and-dirty analysis of overall portfolio ####
########################################################

####### TO DO: TWEAK/MAKE CUSTOM SUMMARY SCRIPT TO GIVE A MORE CONCISE AND RELEVANT SUMMARY (i.e. output tables and less nonsense)
########## IDEAS: table with # articles, # citations, # "is clinical", # cited by clinical, mean/median RCR
####### TO DO: MAKE FIGURES LOOK BETTER 

results <- biblioAnalysis(full_biblio, sep = ";")
summary(results)

ggplot(data = full_biblio, aes(x = "", y = relative_citation_ratio)) + 
  theme_bw() + 
  geom_boxplot() +
  labs(x = paste("PubMed references from search query:", query),
       y = "Relative Citation Ratio")

ggplot(data = full_biblio, aes(x = "", y = nih_percentile)) + 
  theme_bw() + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100)) + 
  labs(x = paste("PubMed references from search query:", query),
       y = "RCR percentile relative to NIH publications")


########################################################
#### Analysis of MeSH terms by year ####
########################################################

## This extracts the top MeSH terms for each year.  This is pretty uninformative because the top MeSH terms are usually something stupid, like "adult".  
## Keeping code in for now in case it turns out to be useful later

mesh_by_year <- data.frame()
for(i in unique(full_biblio$PY)){
  tmp <- cbind(data.frame(table(unlist(strsplit(tolower(full_biblio[full_biblio$PY == i,]$MESH), ";")))), i)
  tmp$ratio <- tmp$Freq/nrow(tmp)
  mesh_by_year <- rbind(mesh_by_year, tmp)
  rm(tmp)
}
colnames(mesh_by_year) <- c("MeSH Keyword", "Frequency", "Year", "Ratio")

top5mesh_by_year <- mesh_by_year %>% 
  group_by(Year) %>% 
  arrange(desc(Frequency)) %>% 
  top_n(n = 5, wt = Frequency)


########################################################
#### Incorporation of MeSH tree structure ####
########################################################

####### TO DO: FIGURE OUT IF IT'S WORTH DOING ANYTHING WITH MESH TREE STRUCTURE
########## IDEAS: break down by big groups, for example a user could filter a data pull to say they're only interested in mesh terms relating to "Chemicals and Drugs [D]"
########## IDEAS: or maybe add a customizable function that would filter down to whatever desired tree node, would that be useful?  Could be good if you wanted to differentiate between research for adults vs adolescents, for example
########## IDEAS: there are statistical methods for mesh term enrichment, like GO term enrichment, but not sure if this is a good idea
########## IDEAS: combine/merge branches?  this could get dicey
########## IDEAS: there's also some potentially interesting stuff in the mesh data file that we could consider exploring further (for example, ConceptList)

## this is a datafile I downloaded from NCBI and should be uploaded yearly.
## in a separate script, I read in the XML version of this and convert to csv for our purposes - the XML makes it slow
meshdata2020 <- read.csv("mesh2020.csv", row.names = 1)

## generate list of all mesh terms in a query
all_mesh <- unlist(strsplit(tolower(full_biblio$MESH), ";"))

## extract relevant MeSH terms from the MeSH tree structure file
## this is kinda slow so it may be possible to speed up if we put in a function or something
## cannot just do a simple subset! many mesh terms exist in multiple forms so if you search for a term like "development" you get a lot more hits than you want
## this specifies it must be an exact match, with nothing else before or after
## you will get NAs for anything that is a "qualifier" in the MeSH hierarchy.  This is good!  Eliminates crap like "analysis" which exists under many different descriptors
## you really shouldn't get any "multiples" or "ERROR" unless something is wrong. I put that in there for a sanity check

meshdata2020$TreeNumberList <- as.character(meshdata2020$TreeNumberList) ## otherwise doesn't populate list with actual tree id
tree_id <- vector()
for(i in unique(all_mesh)){
  query <- grep(paste("^", i, "$", sep = ""), meshdata2020$DescriptorName, ignore.case = TRUE)
  ifelse(length(query) == 1, tree_id[i] <- meshdata2020[query,"TreeNumberList"],
         ifelse(length(query) == 0, tree_id[i] <- NA,
                ifelse(length(query) > 1, tree_id[i] <- "multiples", tree_id[i] <- "ERROR")))
}
## check sanity - should say zero
grep("multiples", tree_id)
grep("ERROR", tree_id)

tree_id_df <- as.data.frame(tree_id)

## add MeSH tree ID to datafile with MeSH broken out by year
mesh_by_year_withID <- merge(mesh_by_year, tree_id_df, by.x = "MeSH Keyword", by.y = 0)


########################################################
#### Linking RCR with MeSH ####
########################################################

## create an expanded df in which mesh terms have separate rows (repeats other entries)
expanded_mesh_df <- full_biblio %>% separate_rows(MESH, sep = ";")

## calculate median RCR for research articles, no provisional RCRs allowed
rcr_by_mesh <- expanded_mesh_df %>% 
  filter(provisional == "No", MESH != "NA", is_research_article == "Yes") %>% 
  group_by(MESH) %>% 
  summarise(n = n(), 
            median_RCR = median(relative_citation_ratio, na.rm = TRUE)) %>% 
  filter(n > 3, median_RCR > 1) %>% 
  arrange(desc(median_RCR))

## top RCR MeSH terms with > 3 publications
head(rcr_by_mesh, 20)

## calculate median RCR for research articles including provisional RCRs
## this will have more or less of an effect depending on how recent the query is
rcr_by_mesh_provisional <- expanded_mesh_df %>% 
  filter(MESH != "NA", is_research_article == "Yes") %>% 
  group_by(MESH) %>% 
  summarise(n = n(), 
            median_RCR = median(relative_citation_ratio, na.rm = TRUE)) %>% 
  filter(n > 3) %>% 
  arrange(desc(median_RCR))

head(rcr_by_mesh_provisional, 20)


## can we use this to identify topics with increasing RCR or trends over time?
## data will be misleading for new/changed MeSH terms as these changes are NOT retrospectively applied
rcr_by_mesh_by_year <- expanded_mesh_df %>% 
  filter(provisional == "No", MESH != "NA", is_research_article == "Yes") %>% 
  group_by(MESH, PY) %>% 
  summarise(n = n(), 
            median_RCR = median(relative_citation_ratio, na.rm = TRUE)) %>% 
  filter(n > 3) %>% 
  arrange(desc(PY), desc(median_RCR)) 

presence_across_years <- dcast(rcr_by_mesh_by_year, MESH ~ PY, value.var = "median_RCR")
presence_across_years[presence_across_years$MESH == "OBESITY",]


