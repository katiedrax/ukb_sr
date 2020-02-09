#############
# libraries ####
#############

library(dplyr)
library(stringr)

# TO DO this code only cleans data for poster the remaining code is copy and pasted from clean_design.R >
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

input <- "data/data_extraction/Data+Extraction+Form_9+February+2020_11.33.csv"
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

###################
# clean for poster ####
####################

df <- df[df$Finished != "False", ]


# drop all evidence boxes for strobe items
df <- df[, -grep("[1-9]{1,}.*ev", colnames(df))]

# remove finished

df <- df[df$Finished == "True", ]

# standardise initials by lowering and removing punctuation

df$initials <- tolower(df$initials)

# only select katie's data

df <- df[df$initials == "kd", ]

if(sum(df$initials =="kd") != nrow(df)){
  # stop if dataframe contains initials other than kd
  stop("df contains more than katie's data")
  # else drop initials column
} else {
  df$initials <- NULL
}
# vector of cols automatically outputted by Qualtrics (always first 10 cols if responses anonymised and should be 112 characters)

qual_cols <- colnames(df)[1:10]

if(sum(str_count(qual_cols)) != 112){
  stop("check qual_cols contains qualtrics columns")
} else {
  # remove qual_cols
  df <- df[, -c(1:10)]
}


#sort by article id

df <- df[order(df$article_id), ]

# df of all articles in csv_clean_epi.csv on OSF

articles_df <- read.csv("https://osf.io/8uy9w/?action=download", encoding = "UTF-8", stringsAsFactors = F)

# vector of all article id's in csv_clean_epi.csv on oSF

articles <- articles_df$id

# check all article id's are in csv_clean_epi.csv
if(sum(df$article_id %in% articles) != nrow(df)) stop("some article_ids not in csv_clean_epi")

# find all prediction models  - they will have 7_iii (predictors) values or "Yes" predict values
predict <- df[!is.na(df$`7_iii`),]
predict <- rbind(predict, df[!is.na(df$predict == "Yes"),])

if(nrow(predict[predict$predict == "Yes"| !is.na(predict$`7_iii`),]) == nrow(predict)){
  # remove articles in predict if predict df only contains prediction models
  df <- df[!(df$article_id %in% predict$article_id), ]
} else {
  stop("some articles in predict are not prediction models")
}


# check all yes_exact values for ukb_credit_ev are correct
df$ukb_exact <- grepl("thisresearchhasbeenconductedusingtheukbiobankresource",  clean_string(df$ukb_credit_ev))

if(all(df$ukb_credit[df$ukb_exact == T] == "yes_exact") == F) stop("yes_exact evidence not exact")
if(all(df$ukb_credit[df$ukb_exact != T] != "yes_exact") == F) stop("some ukb_credit_ev is said to be not exact when it is")

df$ukb_exact <- NULL

# remove "parent topic" columns qualtrics exports if they're empty

if(all(is.na(df$`Q22_89_TEXT - Parent Topics`) && all(is.na(df$`Q22_89_TEXT - Topics`)))){
  df <- df[, -grep("Q22_89", colnames(df))]
} else {
  stop("topic columns not empty")
}

#####################
# export for poster ####
#####################

write.csv(df, "outputs/clean_poster.csv",fileEncoding = "UTF-8", row.names = F)

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

