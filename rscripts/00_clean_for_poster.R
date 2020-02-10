#############
# libraries ####
#############

library(dplyr)
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

###################
# clean for poster ####
####################


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

# remove unfinished
df <- df[df$Finished != "False", ]


# drop all evidence boxes for strobe items
df <- df[, -grep("[[:digit:]]{1,}.*ev", colnames(df))]


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

# remove "parent topic" columns qualtrics exports if they're empty

if(all(is.na(df$`Q22_89_TEXT - Parent Topics`) && all(is.na(df$`Q22_89_TEXT - Topics`)))){
  df <- df[, -grep("Q22_89", colnames(df))]
} else {
  stop("topic columns not empty")
}

######################
# data quality checks ####
#####################

# check data is correct once cleaned
# check all strobe responses are correct

strobe_opts <- c("Partially", "Partially-External", "Unsure", "Yes", "No", NA)

strobe_cols <- grep("^[[:digit:]]{1,2}", colnames(df), value = T)

strobe_values <- stack(sapply(df[, strobe_cols], unique))%>%
  .$values

if(length(setdiff(strobe_values, strobe_opts)) == 0){
  print("correct strobe values")
} else{
  print(setdiff(strobe_responses, strobe_opts))
  stop("incorrect strobe values")
}

# check all yes_exact values for ukb_credit_ev are correct
df$ukb_exact <- grepl("thisresearchhasbeenconductedusingtheukbiobankresource",  clean_string(df$ukb_credit_ev))

if(all(df$ukb_credit[df$ukb_exact == T] == "yes_exact") == F) stop("yes_exact evidence not exact")
if(all(df$ukb_credit[df$ukb_exact != T] != "yes_exact") == F) stop("some ukb_credit_ev is said to be not exact when it is")

df$ukb_exact <- NULL

#############################
# manual data quality checks ####
###############################

# manually checked strobe item 10. should all be NA because all studies used all eligible ppts
df$X10 <- NA

#manually checked strobe item 6 and 12d_cc, should all be NA because no matched articles
match_cols <- grep("X6b|X12d_cc", colnames(df), value = T)

if(sum(is.na(match_cols)) != 0) stop("missing cols in match_cols")

# make all cols in match_cols NA
for(i in match_cols){
  df[[i]] <- NA
  if(all(is.na(df[[i]])) == F) stop("all isn't NA in ", i)
}

#####################
# export for poster ####
#####################

write.csv(df, "outputs/clean_poster.csv",fileEncoding = "UTF-8", row.names = F)
