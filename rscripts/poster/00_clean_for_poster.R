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

input <- "data/data_extraction/Data+Extraction+Form_20+March+2020_09.21.csv"
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
df <- df[, -grep("[[:digit:]]{1,2}.*ev", colnames(df))]


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
predict <- df[!is.na(df$X7_iii),]
predict <- rbind(predict, df[!is.na(df$predict == "Yes"),])

if(nrow(predict[predict$predict == "Yes"| !is.na(predict$X7_iii),]) == nrow(predict)){
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

strobe_cols <- grep("^X[[:digit:]]{1,2}", colnames(df), value = T)

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

# save list of empty cols for checking later
pre <- colnames(df[colSums(!is.na(df)) == 0])

# manually checked strobe item 10. should all be NA because all studies used all eligible ppts
if(sum(is.na(df$X10)) != nrow(df)){
  df$X10 <- NA
} else {
  stop("df$10 not empty when it should be")
}

#manually checked strobe item 6 and 12d_cc, should all be NA because no matched articles
match_cols <- grep("X6b|X12d_cc", colnames(df), value = T)

if(length(match_cols) != 5) stop("wrong number of cols in match_cols")

# make all cols in match_cols NA
for(i in match_cols){
  df[[i]] <- NA
  if(sum(is.na(df[[i]])) != nrow(df)) stop("all isn't NA in ", i)
}

#############
# merge_1 prep #####
#############

# want to display frequency stats for year, journal and supplementary material & article access >
# get these from first_80 csv on osf
merge_1 <- read.csv("https://osf.io/9w72e/?action=download", encoding = "UTF-8", stringsAsFactors = F) %>%
  # select article_id for merging, design & title for checking 
  select(., c("article_id", "title", "design", "year", "journal",  "access_article", "access_supp"))

# check article ids
if(all(merge_1$article_id %in% articles_df$id) != T) stop("some article ids in first_80 are not in csv_clean_epi csvs")


if(all(df$article_id %in% merge_1$article_id) != T){
  # don't run if some article_ids difference
  stop("some df article_ids not in first_80")
} else {
  # subset merge_1 to article_ids in df
  merge_1 <- merge_1[which(merge_1$article_id %in% df$article_id), ] %>%
    # order by article_id
    .[order(.$article_id), ]
  # order df by article_id
  df <- df[order(df$article_id), ]
  # check article_id cols identical
  if(identical(merge_1$article_id, df$article_id) != T) stop("merge_1 & df article_id cols not identical")
}

# check cleaned title substrings are identical and if not print out title strings that are in one dataframe but not the other
if(identical(clean_string(merge_1$title), clean_string(df$title)) == F){
  x <- clean_string(merge_1$title)
  y <- clean_string(df$title)
  # manual check showed that one title differs because of unicode characters so ignore false identical if it is just this title
  if(x[x != y] == "associationsofleglengthtrunklengthandtotaladultheightwithmenierescrosssectionalanalysisintheukbiobank" &&
     y[y != x] == "associationsofleglengthtrunklengthandtotaladultheightwithmnirescrosssectionalanalysisintheukbiobank"){
    print("strings only differ because one string contains unicode character and other doesn't")
    rm(x, y)
    merge_1$title <- NULL
    } else {
    warning("merge_1 titles not in df: ", x[x != y])
    warning("df titles not in merge_1: ", y[y != x])
    stop()
  }
}

# check designs identical
if(identical(merge_1$design, tolower(df$designs))){
  merge_1$design <- NULL
}else {
  stop("designs not identical")
}

#########
# merge_1 ####
##########

if(identical(df$article_id, merge_1$article_id)){
  x <- anti_join(df, merge_1, by = "article_id")
  if(nrow(x) == 0){
    rm(x)
    print("all rows will merge in df & merge_1")
  } else {
    stop("some rows in df & merge_1 cols won't merge")
  }
}


# full join 
df_full <- full_join(df, merge_1, by = "article_id")


###############
# access merge ###
##############

# want to display frequency stats for open access, corrections, etc - data I extracted individually >
# get these from epi_access.csv
access <- read.csv("data/epi_access.csv", stringsAsFactors = F, encoding = "UTF-8", na.strings = c("NA", "", " ")) %>%
  # drop authors,  doi and year
  select(., -c("authors", "doi", "year"))

# subset if article ids all in df_full
if(sum(access$article_id %in% df_full$article_id) == nrow(df_full)){
  access <- access[which(access$article_id %in% df_full$article_id),] %>%
    # order by article_id
    .[order(.$article_id), ]
  # order df_full by article_id
  df_full <- df_full[order(df_full$article_id), ]
  # check article_id cols identical
  if(identical(access$article_id, df_full$article_id) != T) stop("access & df_full article_id cols not identical")
} else {
  stop("some article ids in access are not in df_full")
}

# check cleaned title substrings are identical and if not print out title strings that are in one dataframe but not the other
if(identical(clean_string(access$title), clean_string(df_full$title)) == F){
  x <- clean_string(access$title)
  y <- clean_string(df_full$title)
  # manual check showed that one title differs because of unicode characters so ignore false identical if it is just this title
  if(x[x != y] == "associationsofleglengthtrunklengthandtotaladultheightwithmenierescrosssectionalanalysisintheukbiobank" &&
     y[y != x] == "associationsofleglengthtrunklengthandtotaladultheightwithmnirescrosssectionalanalysisintheukbiobank"){
    print("strings only differ because one string contains unicode character and other doesn't")
    rm(x, y)
    access$title <- NULL
  } else {
    warning("access titles not in df_full: ", x[x != y])
    warning("df_full titles not in access: ", y[y != x])
    stop()
  }
}


#########
# merge access ####
##########

if(identical(df_full$article_id, access$article_id)){
  x <- anti_join(df_full, access, by = "article_id")
  if(nrow(x) == 0){
    rm(x)
    print("all rows will merge in df_full & access")
  } else {
    stop("some rows in df_full & access cols won't merge")
  }
}


# full join 
df_full <- full_join(df_full, access, by = "article_id")


#################
# jif merge prep####
###############

df_full$journal_clean <- clean_string(df_full$journal)
# read in jif

jif <- read.csv("data/jif/all_years.csv", stringsAsFactors = F, encoding = "UTF-8")
jif$JCR.Abbreviated.Title <- clean_string(jif$JCR.Abbreviated.Title)

#subset jif by journal names in df_full

if(sum(duplicated(jif)) == 0){
  jif_match <- jif[which(jif$JCR.Abbreviated.Title %in% clean_string(df_full$journal)),]
  rm(jif)
  # rename journal column
  colnames(jif_match)[colnames(jif_match) == "JCR.Abbreviated.Title"] <- "journal_clean"
  jif_match <- select(jif_match, -c("Rank", "Impact.Factor.without.Journal.Self.Cites", "ISSN", "Full.Journal.Title"))
} else {
  message("jif duplicates")
}

add_year <- function(df, journalcol, jifyear, year){
  x <- select(df, c(journalcol, jifyear))
  x$year <- rep(year, nrow(x))
  return(x)
}

jif12 <- add_year(jif_match, "journal_clean", "jif2012", 2012)
jif13 <- add_year(jif_match, "journal_clean", "jif2013", 2013)
jif14 <- add_year(jif_match, "journal_clean", "jif2014", 2014)
jif15 <- add_year(jif_match, "journal_clean", "jif2015", 2015)
jif16 <- add_year(jif_match, "journal_clean", "jif2016", 2016)
jif17 <- add_year(jif_match, "journal_clean", "jif2017", 2017)
jif18 <- add_year(jif_match, "journal_clean", "jif2018", 2018)


if(nrow(anti_join(df_full, jif12, by = c("journal_clean", "year"))) != nrow(df_full)){
  df_full <- left_join(df_full, jif12, by = c("journal_clean", "year"))
  rm(jif12)
} else {
  message("jif no relevant journals")
}

if(nrow(anti_join(df_full, jif13, by = c("journal_clean", "year"))) != nrow(df_full)){
  df_full <- left_join(df_full, jif13, by = c("journal_clean", "year"))
  rm(jif13)
} else {
  message("jif no relevant journals")
}

if(nrow(anti_join(df_full, jif14, by = c("journal_clean", "year"))) != nrow(df_full)){
  df_full <- left_join(df_full, jif14, by = c("journal_clean", "year"))
  rm(jif14)
} else {
  message("jif no relevant journals")
}

if(nrow(anti_join(df_full, jif15, by = c("journal_clean", "year"))) != nrow(df_full)){
  df_full <- left_join(df_full, jif15, by = c("journal_clean", "year"))
  rm(jif15)
} else {
  message("jif no relevant journals")
}

if(nrow(anti_join(df_full, jif16, by = c("journal_clean", "year"))) != nrow(df_full)){
  df_full <- left_join(df_full, jif16, by = c("journal_clean", "year"))
  rm(jif16)
} else {
  message("jif no relevant journals")
}

if(nrow(anti_join(df_full, jif17, by = c("journal_clean", "year"))) != nrow(df_full)){
  df_full <- left_join(df_full, jif17, by = c("journal_clean", "year"))
  rm(jif17)
} else {
  message("jif no relevant journals")
}

if(nrow(anti_join(df_full, jif18, by = c("journal_clean", "year"))) != nrow(df_full)){
  df_full <- left_join(df_full, jif18, by = c("journal_clean", "year"))
  rm(jif18)
} else {
  message("jif contains no relevant journals")
}

# vector of jif col names
jif_cols <- colnames(df_full)[grep("jif", colnames(df_full))]

df_full$jif <- paste(df_full$jif2014, df_full$jif2015, df_full$jif2017,df_full$jif2018) %>%
  gsub("NA| ", "", .)

# drop jif cols by name as now all pasted together using jif_cols vector
df_full <- select(df_full, -jif_cols)

# set empty cells in jif to NA
df_full$jif[df_full$jif == ""] <- NA

# drop cleaned journal string now jif merged in

df_full$journal_clean <- NULL

# remove merged jif col if too much missing data (30%)

if((sum(is.na(df_full$jif)) / nrow(df_full)) >0.3){
  df_full$jif <- NULL
  message("removed jif because too much missing")
}
################
# scopus merge ####
################
# want to verify extracted data using scopus data

scopus <- read.csv("data/scopus.csv", stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " ")) 

# add open_access column
if(unique(scopus$Access.Type[!is.na(scopus$Access.Type)]) == "Open Access"){
  scopus$open_access <- grepl("Open Access", scopus$Access.Type)
} else {
  stop("other access types than open access")
}

# subset by title

scopus <- scopus[which(clean_string(scopus$Title) %in% clean_string(df_full$title)),]%>%
  select(c("Cited.by", "Author.Keywords", "open_access", "Title"))

if(nrow(anti_join(df_full, scopus, by = c("title" = "Title"))) > 5) {
  message("too many rows won't merge to make it worth it")
}

###################
# recode logicals ####
####################

# if link to open_access_other or correction given, set to true

df_full$open_access_other[grep("http", df_full$open_access_other)] <- T
df_full$correction[grep("http", df_full$correction)] <- T

if(identical(unique(df_full$access_supp), c("Yes", "Not present"))){
  df_full$access_supp[df_full$access_supp == "Yes"] <- T
  df_full$access_supp[df_full$access_supp == "Not present"] <- F
} else {
  stop("access_supp not just yes or not present")
}

#####################
# export for poster ####
#####################

write.csv(df_full, "outputs/clean_poster.csv",fileEncoding = "UTF-8", row.names = F)
