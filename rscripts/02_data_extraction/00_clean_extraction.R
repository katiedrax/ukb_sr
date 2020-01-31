#############
# libraries ####
#############

library(dplyr)
library(stringr)

###############
# functions ####
##############

# function to clean string 
clean_string <- function(string){
  # string: a vector of strings
  # remove any non-english character, numbers, spaces or punctuation and lower
  string <- gsub("[^\u0001-\u007F]+","", string)
  string <- gsub("[0-9]", "", string)
  string <- gsub("[[:punct:]]", "", string)
  string <- tolower(string)
  string <- gsub("[[:space:]]", "", string)
}

##########
# import####
##########

# assign input file 

input <- "data/data_extraction/Data+Extraction+Form_24+January+2020_12.30.csv"
# Import first three rows of the  Qualtrics csv export

header <- read.csv(input, encoding = "UTF-8", nrows = 2, stringsAsFactors = F)

# Row 1 should contain the "question numbers", row 2 the full question text and row 3 the "import id"s
# As "Question Export Tags" assigned and question numbers changed to variable names the first row contains the correct variable names >
# warn user to check row content and that variables in row 1 match full questions

if(sum(str_count(colnames(header))) != 227) stop("check header colnames contains variable names")
if(sum(grepl("ImportId", header[2,])) != 21) stop("check header row 2 contains import ids")
if(sum(str_count(header[1, ])) != 1794) stop("check header row 1 contains full questions")

# assign column names to header

header <- as.character(header[1, ])

header <- gsub("Indicate if the authors report the following items for all the studies in the article \\(unless a study design is specified\\) - ", "", header)%>%
  gsub("Indicate if the authors report the following items for all|in the article", "", .)

# remove row 2 and 3 on import skipping first three rows and assign header as col.names

df <- read.csv(input, skip = 3,
               encoding = "UTF-8", header = F, col.names = header, na.strings = c("", " "), stringsAsFactors = F)

# check length correct

if(nrow(df) != 359) stop("too few/many observations")

############
# clean ####
###########

# vector of cols automatically outputted by Qualtrics (always first 10 cols if responses anonymised and should be 112 characters)

qual_cols <- colnames(df)[1:10]

if(sum(str_count(qual_cols)) != 112) stop("check qual_cols contains qualtrics columns")

# remove qual_cols

df <- select(df, -qual_cols)

#sort by article id

df <- df[order(df$article_id), ]

# standardise initials by lowering and removing punctuation

df$initials <- tolower(df$initials)
df$initials <- gsub("[[:punct:]]", "", df$initials)
if(sum(df$initials != "kd" |df$initials != "mg") != length(df$article_id)) stop("initials contains strings other than kd and mg")

# column containing clean substring of title for easier matching

df$title_sub <- clean_string(df$title)

write.csv(df, "../resolve_conflicts.csv", row.names = F, fileEncoding = "UTF-8")

################
# check designs ####
##############

# check designs only missing because material was inaccessible

if(sum(df$access_article[is.na(df$design)] != "Yes" | df$access_supp[is.na(df$design)] == "Present but not accessible") != sum(is.na(df$design))){
  stop("check NA designs")
} else {
  print("all NA designs had inaccesible material")
}

# check all design responses are correct

design_labels <- c("case-control", "no_statement", "cross-sectional", "cohort", "cohort,cross-sectional", "cohort,case-control", "cross-sectional,no_statement")

if(length(setdiff(df$design[!is.na(df$design)], design_labels)) == 0){
  print("correct design responses")
} else{
  print(setdiff(df$design[!is.na(df$design)], design_labels))
  stop("incorrect design responses")
}

# check all design_judg responses are correct

df$design_judg[df$design_judg == "cross-sectional,cohort"] <- "cohort,cross-sectional"

design_judg_labels <- c(design_labels, "other")

if(length(setdiff(df$design_judg[!is.na(df$design_judg)], design_judg_labels)) == 0){
  print("correct design_judg responses")
} else{
  print(setdiff(df$design_judg[!is.na(df$design_judg)], design_judg_labels))
  stop("incorrect design_judg responses")
}

#################
# column of all designs ####
######################

# new column combining design_judg and design to see stated and judged designs together

df$design_all <- paste(df$design, df$design_judg, sep = ",")

# remove NAs added by paste 

df$design_all[df$design_all == "NA,NA"] <- NA

df$design_all <- gsub("no_statement,|NA,|,NA", "", df$design_all)

# standarise order of designs
df$design_all[grep("cross-sectional,cohort", df$design_all)] <- "cohort,cross-sectional"
df$design_all[grep("cross-sectional,cross-sectional", df$design_all)] <- "cross-sectional"

# check all design responses are correct

design_all_labels <- c("case-control", "other", "no_statement", "cross-sectional", "cohort", 
                       "cohort,cross-sectional", "cohort,case-control", "cross-sectional,no_statement", "cross-sectional,other")

if(length(setdiff(df$design_all[!is.na(df$design_all)], design_all_labels)) == 0){
  print("correct design_all responses")
} else{
  print(setdiff(df$design_all[!is.na(df$design_all)], design_all_labels))
  stop("incorrect design_all responses")
}


####################
# merge preparation ####
##################

kd <- df[df$initials == "kd", ]
mg <- df[df$initials == "mg", ]

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

# mark has duplicate titles for article that had inaccessible supplementary material (which ended the questionnaire) >
# he redid the questionnaire this time incorrectly responding 'not present' or 'yes' to the supplementary material question so that he could complete with the form
# merge Mark's responses with the same article_id by >
# removing the incomplete responses and correcting the access_supp for the complete but incorrect responses

id <- which(mg$access_supp == "Present but not accessible" & mg$article_id %in% mg_id_dup)
mg <- mg[-id, ]

mg$access_supp[mg$article_id %in% mg_id_dup] <- "Present but not accessible"

# find Katie's
kd_id_dup <- kd$article_id[duplicated(kd$article_id)]
kd_id_dup_df <-   kd[kd$article_id %in% kd_id_dup,]

# Katie has given the wrong article_id for the title "Association between adiposity outcomes and residential density: a full-data, cross-sectional analysis of 419<U+2008>562 UK Biobank adult participants"
# create vectors to identify and replace id of title with wrong article_id

if(kd_id_dup != "Sarka2018ants51-2") stop("duplicate article id is wrong") else wrong_id <- kd_id_dup

correct_id <- articles_df$id[grep("Association between adiposity outcomes and residential density", articles_df$title)]
title_with_wrong_id <- grep("Association between adiposity outcomes and residential density", kd_id_dup_df$title, value = T)

# replace article_id of row that has title with wrong article_id
kd$article_id[kd$article_id == wrong_id & kd$title == title_with_wrong_id] <- correct_id

# check article id duplicates again

if(length(kd$title[duplicated(kd$article_id)]) >0 | length(mg$title[duplicated(mg$article_id)]) >0){
  print(kd$title[duplicated(kd$article_id)])
  print(mg$title[duplicated(mg$article_id)])
  stop("kd or mg have duplicated article ids")
} else {
  print("kd and mg have no duplicated article ids")
}

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

######################
# check all articles classified ####
######################

# check kd & md classified all articles in csv_clean_epi.csv

if(length(setdiff(articles, kd$article_id)) > 0 | length(setdiff(articles, mg$article_id)) >0){
  print(setdiff(articles, kd$article_id))
  print(setdiff(articles, mg$article_id))
  stop("kd or mg have not assessed the articles above")
} else {
  print("kd and mg have assessed all articles")
}

# vector of article_ids Mark has not assessed
mg_no <- setdiff(articles, mg$article_id)

if(length(mg_no) > 0){
  # create string of articles_df$title that match the article id's Mark has not assessed then shorten them for easier matching
  mg_no_title <- articles_df$title[articles_df$id %in% mg_no] %>%
    substr(., 1, 40)
  # check title is not in mg
  print(grep(mg_no_title, mg$title))
  stop("Mark has not assessed article: ", mg_no)
} else {
  print("Mark has assessed all articles")
}

# vector of article ids katie has not assessed
kd_no <- setdiff(articles, kd$article_id)

if(length(kd_no) > 0){
  # create string of articles_df$title that match the article id's Mark has not assessed then shorten them for easier matching
  kd_no_title <- articles_df$title[articles_df$id %in% mg_no] %>%
    substr(., 1, 40)
  # check title is not in kd
  print(grep(mg_no_title, mg$title))
  stop("Katie has not assessed article: ", kd_no)
} else {
  print("Katie has assessed all articles")
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

##################################
# assign random number list ####
##############################

# if article ids are identical to those in csv_clean_epi add random number list and names

if(identical(sort(both$article_id), sort(articles))){
  set.seed(1)
  both$num <- sample(1:length(both$article_id), length(both$article_id), replace = F)
  both <- select(both, num, everything())
} else {
  stop("article_id's incorrect")
}
