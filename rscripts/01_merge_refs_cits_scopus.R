# Description

# This code merges the included refereneces (from Endnote), their citations (from Endnote) and their scopus profiles (exported from scopus as csv)
# It only cleans the datasets in so far as to allow them to merge correctly and assign headers and export merged csv


## TO DO merge on title (after removing all punct etc) or article id

#########
#### packages####
#########

library(tidyverse)

###################
#### import source ####
###################

# import name of database (i.e. source) the reference was retrieved from

source <- read.csv("data/02_source_w_headers.csv", header = T, stringsAsFactors = F, encoding = "UTF-8", na.strings = "")

################
#### clean source ####
#################

# function to remove any duplicates

remove_dups <- function(df, col1, col2){
  cols_to_check <- c(col1, col2)
  df <- df[!duplicated(df[, cols_to_check]), ]
}

# check & remove duplicates

if(sum(duplicated(source[, c("title", "doi")])) > 0){
  #remove dups
  source <- remove_dups(source, "title", "doi")
} else {
  print("no dups")
}

# function to check each row has a unique doi (not counting NA as a value)

check_doi <- function(doi_col, df){
  if((length(na.omit(unique(doi_col))) == nrow(df)) == FALSE)
    stop("missing/duplicated dois")
  else
    print("dois OK")
}

# check dois

check_doi(source$doi, source)

# check each row has unique doi or is missing

(sum(is.na(source$doi)) + length(unique(na.omit(source$doi)))) == nrow(source)

###################
#### import citations####
###################

# read in citations 

cite <- read.csv("data/03_citations.csv", header = F, encoding = "UTF-8")

#add header

colnames(cite)[1] <- "citation"

#  check number of rows

nrow(cite)

# create doi col by separating it from end of citations

cite <- separate(cite, citation, into = c("temp", "doi"), sep = "doi:", fill = "left", remove = FALSE)

# remove temp col

cite$temp <- NULL

# remove duplicates

if(sum(duplicated(cite[, "doi"])) > 0){
  #remove dups
  cite <- remove_dups(cite, "doi", col2 = NULL)
} else {
  print("no dups")
}

# check dois

check_doi(cite$doi, cite)

###############
#### import included ####
###############

#import references from cleaned csv file

df <- read.csv("outputs/csv_clean.csv", header =T, stringsAsFactors = F, encoding = "UTF-8")

# remove all unicode characters from df

df <- data.frame(lapply(df, function(x) {
  gsub("[^\u0001-\u007F]+", "", x)
}))

#################################
#### create scopus search string ####
##################################

# create search string (to be pasted into Scopus)

doi_search <- paste("DOI(", df$doi, ")", sep = "", collapse = " OR ")

# export the search string

write.table(doi_search, "outputs/scopus_search_string.txt", quote = F, row.names = F, col.names = F)

##################
#### import scopus results ####
#################

# NOT AUTOMATED: paste scopus_search_string into advanced search in Scopus & click search
# select 'All' > click drop down arrow next to 'RIS export' > select 'CSV' > select all items you want to export > 
# click 'Export' > save csv as 'scopus' in this project's 'data' folder

# import csv exported from scopus

scopus <- read.csv("data/scopus.csv", header = T, stringsAsFactors = F, encoding = "UTF-8")

# drop affiliations col, this had to be exported to export the 'authors with affiliations' col

scopus$Affiliations <- NULL

# find duplicate DOIs

dup_doi <- scopus$DOI[duplicated(scopus$DOI)]

id <- which(scopus$DOI %in% dup_doi)

if ((length(id) > 0)) {
  stop(dups <- scopus[id, ])
} else {
  print("no dups")
}

# NOT AUTOMATED - I manually checked dois in dups are correct = 1 is not correct
# replace dois of duplicate that has an incorrect doi 

scopus$DOI[which(scopus$Title == "A case of recent myocardial infarction with cardiac failure")] <- "10.1136/heartjnl-2016-309715"

#check duplicates again

dup_doi <- scopus$DOI[duplicated(scopus$DOI)]

id <- which(scopus$DOI %in% dup_doi)

if ((length(id) > 0)) {
  stop(dups <- scopus[id, ])
} else {
  print("no dups")
}

##################################
#### check same dois have same title ####
##################################

# get df of all titles & dois included refs
in_df <- select(df, c(title, doi)) %>%
  # rename doi to be same format as scopus DOI col
  rename(DOI = "doi") %>%
  #add col to indicate where data contained in
  mutate(source = rep("refs"))

# get df of all titles & dois included refs in scopus results 

in_scopus <- select(scopus, c(Title, DOI)) %>%
  #rename title to be same format as df title col
  rename(title = "Title") %>%
  #add col to indicate where data contained in
  mutate(source = rep("scopus"))

# convert all to character

in_scopus[] <- lapply(in_scopus, as.character)
in_df[] <- lapply(in_df, as.character)

# make all lower 

in_scopus[] <- lapply(in_scopus, tolower)
in_df[] <- lapply(in_df, tolower)

# merge everything together
all <- merge(in_df, in_scopus, all= T)

# remove any punctuation  from title to make sure identical titles with different punctuation are matched
all$title  <- gsub("[[:punct:]]", "", all$title)
all$title  <- gsub("–", "", all$title)

#many titles are identicial but have some different characters so >
# add col of title substrings to make it easier to find duplicate titles then >
# find items with duplicated title substring and doi ( this means refs have same dois & titles in scopus search)

all$sub <- substring(all$title, 1,35)

dup_sub_doi <- all[duplicated(all[, c("DOI", "sub")]), ]

matches <- all[all$DOI %in% dup_sub_doi$DOI, ]

# find refs that were not in matches & check

not_matches <- all[!(all$DOI %in% dup_sub_doi$DOI), ]

id <- not_matches[duplicated(not_matches[, "DOI"]), ]
dup_doi <- not_matches[not_matches$DOI %in% id$DOI, ]

print("all not_matches with same DOI have same title just have slight differences in spacing etc")

not_dup_doi <- not_matches[!(not_matches$DOI %in% id$DOI), ]

print("not_dup_doi shows refs that do not have a matching scopus result BUT also shows one scopus result that is not an included reference")

#################
#### add citations ####
#################

# convert all cols to characters
df[] <- lapply(df, as.character)
cite[] <- lapply(cite, as.character)

# check colnames

colnames(df)
colnames(cite)

# merge with citation

df_cite <- full_join(df, cite, by = "doi")

# check correct # cols

length(colnames(df)) +length(colnames(cite)) == length(colnames(df_cite)) + 1

# check title still contains 564 included refs

length(df_cite$title[!is.na(df_cite$title)]) == 564

##############
#### add scopus ####
##############

# convert all cols to characters
df_cite[] <- lapply(df_cite, as.character)
scopus[] <- lapply(scopus, as.character)

# check colnames

colnames(df_cite)
colnames(scopus)

# merge with citation

df_cite_scop <- full_join(df_cite, scopus, by = c("doi" = "DOI"))

# check correct # cols

length(colnames(df_cite)) +length(colnames(scopus)) == length(colnames(df_cite_scop)) + 1

# check title still contains 564 included refs

length(df_cite_scop$title[!is.na(df_cite_scop$title)]) == 564


#################
#### add source ####
##################

# convert all cols to characters
df_cite_scop[] <- lapply(df_cite_scop, as.character)
source[] <- lapply(source, as.character)

# check colnames

colnames(df_cite_scop)
colnames(source)

# add source to df

df_cite_scop_sourc <- full_join(df_cite_scop, source, by = "doi")

# check correct num of cols
# num of cols of two merged dfs combined should == num of cols in new df + 1 (the col you merged on)

length(colnames(df_cite_scop)) +length(colnames(source)) == length(colnames(df_cite_scop_sourc)) + 1

# check title still contains 564 included refs

length(df_cite_scop_sourc$title[!is.na(df_cite_scop_sourc$title)]) == 564


#########
#### export ####
#########

write.csv(df_cite_scop_sourc, "outputs/merged.csv", row.names = F, fileEncoding = "UTF-8")
