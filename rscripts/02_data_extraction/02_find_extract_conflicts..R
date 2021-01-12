#############
# libraries ####
#############

library(magrittr)
library(stringr)

# TO DO this code is copy and pasted from clean_design.R >
# need to clean properly by adapting copy pasted code

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

df <- read.csv(input, encoding = "UTF-8", stringsAsFactors = F, header = T, na.strings = c("", " ", "NA"),strip.white = T)

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


####################
# merge prep ####
##################

kd <- df[df$initials == "kd", ]
mg <- df[df$initials == "mg", ]
rr <- df[df$initials == "rr", ]
bj <- df[df$initials == "bj", ]


###############################
# check article id duplicates ####
###############################


# find Mark's duplicate article_ids
mg_id_dup <- mg$article_id[duplicated(mg$article_id)]
mg_id_dup_df <-   mg[mg$article_id %in% mg_id_dup,]

# find Katie's
kd_id_dup <- kd$article_id[duplicated(kd$article_id)]
kd_id_dup_df <-   kd[kd$article_id %in% kd_id_dup,]


#########################
# check title duplicates ####
#########################

# check there are no duplicate titles

if(length(kd$title[duplicated(kd$title)]) >0 | length(mg$title[duplicated(mg$title)]) >0){
  print(kd$title[duplicated(kd$title)])
  print(mg$title[duplicated(mg$title)])
  stop("kd or mg have duplicated titles")
} else {
  print("kd and mg have no duplicate titles")
}


###########
# merge####
##########

# join sets of articles assessed by md and kd. Add suffix's to indicate who's columns are whos
both <- full_join(kd, mg, by = "article_id", suffix = c(".kd", ".mg"))

# order column names alphabetically & put article_id and title first 

both <- both[,order(colnames(both))] %>%
  select(article_id, title.kd, title.mg, everything())

# drop initials columns as only needed for merge

both$initials.kd <- NULL
both$initials.mg <- NULL

###################
# check title conflicts ####
########################


title_cons <- both[which(both$title_sub.kd != both$title_sub.mg), ] %>%
  select(c(article_id, title.kd, title.mg))
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

