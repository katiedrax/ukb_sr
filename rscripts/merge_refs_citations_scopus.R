#########
#### packages####
#########

library(tidyverse)

###################
#### import citations####
###################

# read in citations (exported from endnote )

cite <- as.data.frame(readLines("citations.txt", encoding = "UTF-8-BOM"))
colnames(cite)[1] <- "X1"

cite$X1 <- gsub("\\ï»¿", "", cite$X1)

cite <- separate(cite, X1, into = c("temp", "doi"), sep = "doi:", fill = "left", remove = FALSE)

# check number of DOIs

if ((length(unique(cite$doi)) == nrow(cite)) == FALSE){
  stop("dois wrong")
} else {
  print("dois OK")
}

# remove temp col

cite$temp <- NULL

###############
#### import ref details ####
###############

#import details of included references (exported from endnote)

df <- read.csv("csv.csv", header =F, stringsAsFactors = F, encoding = "UTF-8-BOM")

# remove empty cols

all_na <- sapply(df, function(x) all(is.na(x)))

df <- df[!all_na]

# rename doi col

df <- dplyr::rename(df, doi = "V50")

# add in citation

df2<- merge(df, cite, by = "doi", all.x = TRUE)

sapply(df, class)

# check dois

if ((length(unique(df$doi)) == nrow(df)) == FALSE){
  stop("dois wrong")
} else {
  print("dois OK")
}

# remove strange symbol from df

df <- data.frame(lapply(df, function(x) {
  gsub("\\ï»¿", "", x)
  }))

# create search string (to be pasted into Scopus)

doi_search <- paste("DOI(", df$doi, ")", sep = "", collapse = " OR ")

# add headers
##TO DO
#names(df) <- c("authors", "year", "title")

##################
#### import scopus results ####
#################


scopus <- read.csv("scopus_doi.csv", stringsAsFactors = F, encoding = "UTF-8")

# find duplicate DOIs

dup_doi <- scopus$DOI[duplicated(scopus$DOI)]

id <- which(scopus$DOI %in% dup_doi)

if ((length(id) > 0)) {
  stop(dups <- scopus[id, ])
} else {
  print("no dups")
}

# check dois of refs in dups are correct & replace those that arent in scopus

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
in_df <- select(df, c(V3, doi)) %>%
  # rename headers
  rename(DOI = "doi", title = "V3") %>%
  #add col to indicate where data contained in
  mutate(source = rep("refs")) %>%
  # add col of title substrings to make merge on titles more accurate
  mutate(title_sub = substring(in_df$title, 1,60))


# get df of all titles & dois included refs in scopus results 

in_scopus <- select(scopus, c(Title, DOI)) %>%
  #rename headers
  rename(title = "Title") %>%
  #add col to indicate where data contained in
  mutate(source = rep("scopus"))%>%
  # add col of title substrings to make merge on titles more accurate
  mutate(title_sub = substring(in_scopus$title, 1,60))


# convert all to character

in_scopus[] <- lapply(in_scopus, as.character)
in_df[] <- lapply(in_df, as.character)

# make all lower 

in_scopus[] <- lapply(in_scopus, tolower)
in_df[] <- lapply(in_df, tolower)

# merge on title substrings
all <- full_join(in_df, in_scopus, by = c("DOI", "title_sub"))

all <- full_join(in_df, in_scopus, by = "DOI")


# see if any duplicate dois 

dup_doi <- all$DOI[duplicated(all$DOI)]

id <- which(all$DOI %in% dup_doi)

if ((length(id) > 0)) {
  stop(dup_doi <- all[id, ])
} else {
  print("no dups")
}

# check duplicate dois have same title

setdiff(dups$title_sub[dups$source.y == "scopus"], dups$title_sub[dups$source.x == "refs"])

setdiff(dups$title_sub[dups$source.x == "refs"], dups$title_sub[dups$source.y == "scopus"])

#manual check indicates all duplicate dois have same title

#check for duplicate title substrings

dup_title <- all$title_sub[duplicated(all$title_sub)]

id <- which(all$title_sub %in% title_sub)

if ((length(id) > 0)) {
  stop(dup_title <- all[id, ])
} else {
  print("no dups")
}
