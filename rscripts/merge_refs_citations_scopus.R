# Description

# This code merges the included refereneces (from Endnote), their citations (from Endnote) and their scopus profiles (exported from scopus as csv)
# It only cleans the datasets in so far as to allow them to merge correctly 

#########
#### packages####
#########

library(tidyverse)

###################
#### import citations####
###################

# read in citations (exported from endnote )

cite <- as.data.frame(readLines("data/citations.txt", encoding = "UTF-8"))
colnames(cite)[1] <- "citation"

# remove all unicode characters 

cite$citation <- gsub("[^\u0001-\u007F]+", "", cite$citation)

# create doi col by separating it from end of citations

cite <- separate(cite, citation, into = c("temp", "doi"), sep = "doi:", fill = "left", remove = FALSE)

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

df <- read.csv("data/csv.csv", header =F, stringsAsFactors = F, encoding = "UTF-8")

# add headers
##TO DO
#names(df) <- c("authors", "year", "title")

# rename doi col

df <- dplyr::rename(df, doi = "V50")

# check dois

if ((length(unique(df$doi)) == nrow(df)) == FALSE){
  stop("dois wrong")
} else {
  print("dois OK")
}

# remove all unicode characters from df

df <- data.frame(lapply(df, function(x) {
  gsub("[^\u0001-\u007F]+", "", x)
}))

# create search string (to be pasted into Scopus)

doi_search <- paste("DOI(", df$doi, ")", sep = "", collapse = " OR ")


##################
#### import scopus results ####
#################


scopus <- read.csv("data/scopus_doi.csv", stringsAsFactors = F, encoding = "UTF-8")

# find duplicate DOIs

dup_doi <- scopus$DOI[duplicated(scopus$DOI)]

id <- which(scopus$DOI %in% dup_doi)

if ((length(id) > 0)) {
  stop(dups <- scopus[id, ])
} else {
  print("no dups")
}

# check dois of refs in dups are correct & replace those that arent

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
  mutate(source = rep("refs"))

# get df of all titles & dois included refs in scopus results 

in_scopus <- select(scopus, c(Title, DOI)) %>%
  #rename headers
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
# add citations#
#################

# convert all cols to characters
df[] <- lapply(df, as.character)
cite[] <- lapply(cite, as.character)

# merge with citation

df_cit <- full_join(df, cite, by = "doi")

##############
# add scopus #
##############

# convert all cols to characters
df[] <- lapply(df, as.character)
scopus[] <- lapply(scopus, as.character)

# merge with citation

df_cit_scop <- full_join(df, scopus, by = c("doi" = "DOI"))

# remove scopus result that is not in refs 

df_cit_scop <- df_cit_scop[!is.na(df_cit_scop$V3), ]

#########
# export#
#########

write.csv(df_cit_scop, "outputs/merged_cleaned.csv", row.names = F)
