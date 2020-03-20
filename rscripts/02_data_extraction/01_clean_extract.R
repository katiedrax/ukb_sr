#############
# libraries ####
#############

library(dplyr)
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

input <- "data/data_extraction/Data+Extraction+Form_10+February+2020_18.20.csv"
# Import first two rows of the  Qualtrics csv export

rows_3 <- read.csv(input, encoding = "UTF-8", nrows = 3, stringsAsFactors = F, header = F)

header <- read.csv("outputs/extraction_dictionary.csv", encoding = "UTF-8", stringsAsFactors = F)

# order by import number so order will match order of headers in row 3
header <- header[order(header$import_num), ]

############
# import ####
###########

if(identical(as.character(rows_3[2, ]), as.character(header$question))){
  # if row 2 == question text row import csv and skip first three rows (which contain the qualrics header rows)
  # set "Not applicable" responses used in strobe item options to missing so won't be included in tables
  df <- read.csv(input, encoding = "UTF-8", stringsAsFactors = F, header = F, skip = 3, na.strings = c("", " ", "NA"))
  # assign header$variable as column names as order will now match
  colnames(df) <- header$variable
} else {
  stop("row 2 in csv != question text row")
}

#################
# clean properly ####
#################

# column containing clean substring of title for easier matching

df$title_sub <- clean_string(df$title)

####################
# merge prep ####
##################

kd <- df[df$initials == "kd", ]
mg <- df[df$initials == "mg", ]
rr <- df[df$initials == "rr", ]
bj <- df[df$initials == "rj", ]

# df of all articles in csv_clean_epi.csv on OSF

articles_df <- read.csv("https://osf.io/8uy9w/?action=download", encoding = "UTF-8", stringsAsFactors = F)

# vector of all article id's in csv_clean_epi.csv on oSF

articles <- articles_df$id

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

