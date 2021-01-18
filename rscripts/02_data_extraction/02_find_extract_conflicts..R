#############
# libraries ####
#############

library(magrittr)
library(stringr)

###############
# functions ####
##############

# get clean string function

source("rscripts/functions/clean-string-fun.R")

##########
# import####
##########

# assign input file 

input <- "outputs/clean_extraction_form.csv"
# Import and set NA and strip white space to clean free text answers

df <- read.csv(input, encoding = "UTF-8", stringsAsFactors = F, header = T, na.strings = c("", " "),strip.white = T)

# add substring of title for easier matching in later checks

df$title_sub <- clean_string(df$title)

################################################
# check all entries have article id and title ####
##############################

# some rows may have many NAs because of unforced answers >
# if rows contain too many NAs  the article any be unidentifiable just incase so remove these rows
# set limit of 99% NA values
na_limit <- ncol(df) - (ncol(df) * 0.01)

if(sum(is.na(df$article_id)| is.na(df$title)) != 0){
  # find all obs missing article_id or title
  miss <- df[is.na(df$article_id) | is.na(df$title), ]
  # count number of NAs in each row
  miss$na_sum <- apply(miss, 1, function(x) sum(is.na(x)))
} else {
  print("no articles missing article_id or title")
}

# check all rows missing article_id or title contain too many NAs to be identifiable and remove them
if(all(miss$na_sum > na_limit)) {
  # remove
  df <- df[!is.na(df$article_id)| !is.na(df$title), ]
} else {
  stop("could identify some obs missing id or title")
}


###############
# check article_id and title ####
#need to check article_id matches the title
################

# df of all articles in csv_clean_epi.csv on OSF

articles_df <- read.csv("https://osf.io/8uy9w/?action=download", encoding = "UTF-8", stringsAsFactors = F)

# vector of all article id's in csv_clean_epi.csv on oSF

articles <- articles_df$id

# create substring of title in articles_df for easier matching

articles_df$title_sub <- clean_string(articles_df$title)

# get article_ids and title substrings so can check similarities

orig_id <- articles_df[, c("id", "title_sub")]
df_id <- df[, c("article_id", "title_sub")]

# check all article_ids are right

df_id$article_id[!(df_id$article_id %in% orig_id$id)]

######################
# remove empty rows ####
#####################

df$nas <- apply(df, 1, function(x) sum(is.na(x)))

############################
# MANUAL remove duplicate articles ####
#########################

# I wrote the find_duplicate_articles() function >
# I manually identified duplicates and found which were incomplete/out of date >
# wrote a manual function to remove these duplicates

MANUAL_remove_duplicate_articles <- function(){
  # One Foste2018hort00-7 entry by MG is incomplete, 
  mg_dup <- which(df$article_id =="Foste2018hort00-7" & is.na(df$s1a) & df$initials == "MG")
  df <- df[-mg_dup, ]
  # Mulle2017tudy2467 and Peter2018bank8507 are prediction articles >
  # KD and RR duplicated one of these articles to mark them as prediction articles,after failing to do so >
  # remove entries not marked as prediction articles
  kd_dup <- which(df$article_id =="Mulle2017tudy2467" &is.na(df$predict) &df$initials == "KD")
  df <- df[-kd_dup,]
  rr_dup <- which(df$article_id =="Peter2018bank8507" &is.na(df$predict) & df$initials == "RR")
  df <- df[-rr_dup, ]
}

# apply function 
df <- MANUAL_remove_duplicate_articles()

# function to find any duplicate articles for each initial
find_duplicate_articles <- function(){
  # get vector of unique initials to find duplicate articles for each initial
  initials <- unique(df$initials)
  # add names of initials so list output from lapply will have names
  names(initials) <- initials
  # loop for each initial
  a <- lapply(initials, function(x){
    # find all articles for initial x so can search for duplicates
    initial_articles <- df[df$initials == x, "article_id"]
    # find values of duplicated articles so can find position in df 
    dup_article <- initial_articles[duplicated(initial_articles)]
    # find position of duplicate articles for each initial
    dup_article_by_initial <- which(df$initials == x & df$article_id == dup_article)
    # return
    return(dup_article_by_initial)
  })
  # check there are no duplicate articles for any initials
  if(any(lengths(a)>0)){
    warning(paste(names(which(lengths(a) >0)), collapse = " "), " have duplicate article_ids")
    return(a)
  } else {
    print("no data extractor has any duplicate articles")
  }
}

# check all duplicates gone
find_duplicate_articles()

###########
# merge####
##########

# join sets of articles assessed by md and kd. Add suffix's to indicate who's columns are whos
both <- merge(df[df$initials == "KD", ], df[df$initials == "MG", ], by = "article_id", suffixes = c(".kd", ".mg"))

# order column names alphabetically & put article_id and title first 

both <- both[,order(colnames(both))] %>%
  dplyr::select(article_id, title.kd, title.mg, everything())

# drop initials columns as only needed for merge

both$initials.kd <- NULL
both$initials.mg <- NULL

###################
# check title conflicts ####
########################


title_cons <- both[which(both$title_sub.kd != both$title_sub.mg), c("article_id", "title.kd", "title.mg")]
warning("manually compare titles View(title_cons) to check they are the same")

# titles may all be correct but differ in spelling, unicode characters, etc
# if are correct delete Katie's title and title_sub columns

# add any titles of articles Katie has assessed and Mark has not
both$title.mg[is.na(both$title.mg)] <- both$title.kd[is.na(both$title.mg)]

#delete Katie's title and title_sub columns 
both$title.kd <- NULL
both$title_sub.kd <- NULL

# rename remaining columns
colnames(both)[colnames(both) == "title.mg"] <- "title"
colnames(both)[colnames(both) == "title_sub.mg"] <- "title_sub"

