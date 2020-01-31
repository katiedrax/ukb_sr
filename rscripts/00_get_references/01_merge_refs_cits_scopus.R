# Description

# This code merges the included refereneces (from Endnote), their citations (from Endnote) and their scopus profiles (exported from scopus as csv)
# It only cleans the datasets in so far as to allow them to merge correctly and assign headers and export merged csv

#TO DO check merges, change incorrect titles & remerge

#########
#### packages####
#########

library(tidyverse)


###################
#### functions ####
###################

# create function to check no missing, duplicated or incorrect dois

check_doi <- function(doi_col){
  # check no dois missing
  if(sum(is.na(doi_col)) > 0) stop("dois missing") else
    # check all dois contain 10.
    if(length(doi_col[!grepl("10", doi_col)]) != 0) stop("some dois don't contain 10") else
      # check all dois start with 10
      if(length(doi_col[!grepl("^10", doi_col)]) != 0) stop("some dois don't start with 10") else
        # check no dois duplicated
        if(sum(duplicated(doi_col)) > 0) stop("duplicate dois") else
          # correct
          return("dois OK")
}

###################
#### import source ####
###################

# import name of database (i.e. source) the reference was retrieved from

source <- read.csv("data/02_source_w_headers.csv", header = T, stringsAsFactors = F, encoding = "UTF-8", na.strings = "")

################
#### clean source ####
#################

# remove duplicate rows

source <- source[!duplicated(source[, c("title", "doi")]), ]

# check dois that aren't missing

check_doi(na.omit(source$doi))

# check if any dois don't start with 10 

source$doi[!grepl("^10", source$doi)]

# remove everything that comes before 10. in dois that don't start with 10

source$doi[!grepl("^10", source$doi)] <- sub(".*(?=10\\.)","",source$doi[!grepl("^10", source$doi)], perl=TRUE)

# check dois that aren't missing again

check_doi(na.omit(source$doi))

# check each row has unique doi or is missing

(sum(is.na(source$doi)) + length(unique(na.omit(source$doi)))) == nrow(source)

###################
#### import citations####
###################

# read in citations 

cite <- read.csv("data/03_citations.csv", header = F, encoding = "UTF-8")

#add header

colnames(cite)[1] <- "citation"

###################
#### clean citations####
###################

#  check number of rows

nrow(cite)

# create doi col by separating it from end of citations

cite <- separate(cite, citation, into = c("temp", "doi"), sep = "doi:", remove = FALSE)

# remove temp col

cite$temp <- NULL

# remove duplicates

if(sum(duplicated(cite)) > 0){
  #remove dups
  cite <- cite[!duplicated(cite), ]
} else {
  print("no dups")
}

# check dois that aren't missing

check_doi(na.omit(cite$doi))

# check if any dois don't start with 10 

cite$doi[!grepl("^10", cite$doi)]

# remove everything that comes before 10. in dois that don't start with 10

cite$doi[!grepl("^10", cite$doi)] <- sub(".*(?=10\\.)","",cite$doi[!grepl("^10", cite$doi)], perl=TRUE)

# check dois that aren't missing again

check_doi(na.omit(cite$doi))

###############
#### import included ####
###############

# import references from cleaned csv file

df <- read.csv("outputs/csv_clean.csv", header =T, stringsAsFactors = F, encoding = "UTF-8")


##################
#### import scopus results ####
#################

# import csv exported from scopus

scopus <- read.csv("data/scopus.csv", header = T, stringsAsFactors = F, encoding = "UTF-8")

##################
#### clean scopus ####
#################

# drop affiliations col, this had to be exported to export the 'authors with affiliations' col

scopus$Affiliations <- NULL

# check & remove duplicates

if(sum(duplicated(scopus)) > 0){
  #remove dups
  scopus <- scopus[!duplicated(scopus), ]
} else {
  print("no dups")
}

# check dois

check_doi(scopus$DOI)

# function to find all duplicate values in one vector

match_dups <- function(df, col){
  dups <- col[duplicated(col)]
  id <- which(col %in% dups)
  dups <- df[id, ]
}

# find all duplicate dois

dup_dois <- match_dups(scopus, scopus$DOI)

# NOT AUTOMATED - google dois in dup_dois 
# replace incorrect dois

scopus$DOI[which(scopus$Title == "A case of recent myocardial infarction with cardiac failure")] <- "10.1136/heartjnl-2016-309715"

#check dois again

check_doi(scopus$DOI)

###################### 
#### prepare for merge ####
######################

# function to clean a string vector and extract substring

clean_str <- function(string){
  #remove punctionation
  string  <- gsub("[[:punct:]]", "", string)
  #remove punctionation
  string  <- gsub("–", "", string)
  #remove punctionation
  string <- gsub("[^\u0001-\u007F]+", "", string)
  # remove any unicode characters
  string <- gsub("[^\u0001-\u007F]+","", string)
  # remove any numbers 
  string <- gsub("[0-9]", "", string)
  #make all strings lowercase
  string <- tolower(string)
  #remove all spaces
  string <- gsub(" ", "", string)
  # extract substring
  string <- substring(string, 1,50)
}

# add clean title string to merge on to datasets with a title col (df, scopus & source)

df$ti_clean <- clean_str(df$title)
scopus$ti_clean <- clean_str(scopus$Title)
source$ti_clean <- clean_str(source$title)

# dois are case insensitive so make all dois strings lowercase

df$doi <- tolower(df$doi)
scopus$DOI <- tolower(scopus$DOI)
cite$doi <- tolower(cite$doi)
source$doi <- tolower(source$doi)

##############
#### add scopus ####
##############

# convert all cols to characters
df[] <- lapply(df, as.character)
scopus[] <- lapply(scopus, as.character)

# check colnames

colnames(df)
colnames(scopus)

# merge with citation

merged <- full_join(df, scopus, by = c("doi" = "DOI", "ti_clean"))

# check correct # cols

length(colnames(df)) +length(colnames(scopus)) == length(colnames(merged)) + 2

# check title still contains 564 included refs

length(na.omit(merged$title)) == 564

# TO DO CHECK MERGES
#merged$title[is.na(merged$Title)]

#dup_dois <- match_dups(merged, merged$doi)

#dup_titles <- match_dups(merged, merged$ti_clean)

#setdiff(dup_titles$title, dup_titles$Title)

#################
#### add citations ####
#################

# convert all cols to characters
merged[] <- lapply(merged, as.character)
cite[] <- lapply(cite, as.character)

# check colnames

colnames(merged)
colnames(cite)

# merge with citation

merged <- full_join(merged, cite, by = "doi")

# check correct # cols

length(colnames(df)) +length(colnames(cite)) == length(colnames(merged)) + 1

# check title still contains 564 included refs

length(na.omit(merged$title)) == 564

#################
#### add source ####
##################

# convert all cols to characters
merged[] <- lapply(merged, as.character)
source[] <- lapply(source, as.character)

# check colnames

colnames(merged)
colnames(source)

# add source to df

merged <- full_join(merged, source, by = c("doi", "ti_clean"))

# check correct num of cols
# num of cols of two merged dfs combined should == num of cols in new df + 1 (the col you merged on)

length(colnames(df)) +length(colnames(source)) == length(colnames(merged)) + 1

# check title still contains 564 included refs

length(na.omit(merged)) == 564


#########
#### export ####
#########

write.csv(merged, "outputs/merged.csv", row.names = F, fileEncoding = "UTF-8")
