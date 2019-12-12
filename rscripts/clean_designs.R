# libraries ####

library(dplyr)
library(stringr)

##########
# import####
##########

# assign input file 

input <- "data/Classifying+epi+study+designs_11+December+2019_23.50.csv"
# Import first three rows of the  Qualtrics csv export

header <- read.csv(input, encoding = "UTF-8", nrows = 3)

# Row 1 should contain the "question numbers", row 2 the full question text and row 3 the "import id"s
# As "Question Export Tags" assigned and question numbers changed to variable names the first row contains the correct variable names >
# warn user to check row content and that variables in row 1 match full questions

warning("manually check rows 1 = variables & row 2& 3 are import id and full questions")
warning("manually check variables names match full questions")

# remove rows 2 and 3 by assigning column names (row 1)

header <- colnames(header)

# remove row 2 and 3 on import skipping first three rows and assign header (row 1) as col.names

df <- read.csv(input, skip = 3,
               encoding = "UTF-8", header = F, col.names = header, na.strings = c("", " "), stringsAsFactors = F)

############
# clean ####
###########

# vector of cols automatically outputted by Qualtrics (always first 10 cols if responses anonymised)

qual_cols <- colnames(df)[1:10]
qual_cols

# remove qual_cols

df <- select(df, -qual_cols)

#sort by article id

df <- df[order(df$article_id), ]


# standardise initials by lowering and removing punctuation

df$initials <- tolower(df$initials)
df$initials <- gsub("[[:punct:]]", "", df$initials)
table(df$initials)

#################
# new column of designs ####
######################

# new column combining design_judg and design to see stated and judged designs together

df$design_all <- paste(df$design, df$design_judg, sep = ",")

# remove NAs added by paste 

df$design_all[df$design_all == "NA,NA"] <- NA
df$design_all <- gsub("no_statement,|NA,|,NA", "", df$design_all)

# standarise order of designs
df$design_all[grep("cross-sectional,cohort", df$design_all)] <- "cohort,cross-sectional"
df$design_all[grep("cross-sectional,cross-sectional", df$design_all)] <- "cross-sectional"

####################
# merge preparation ####
##################

kd <- df[df$initials == "kd", ]
mg <- df[df$initials == "mg", ]

# vector of all articles in csv_clean_epi.csv on OSF

articles <- read.csv("https://osf.io/8uy9w/?action=download", encoding = "UTF-8", stringsAsFactors = F) %>%
  .$id

# check kd & md assessed all articles in csv_clean_epi.csv

if(length(dplyr::setdiff(articles, kd$article_id)) > 0 | dplyr::setdiff(articles, mg$article_id) >0){
  stop("kd or mg have not assessed all articles")
}

# check title duplicates

if(length(kd$title[duplicated(kd$title)]) >0 | length(mg$title[duplicated(mg$title)]) >0){
  stop("kd or mg have duplicated titles")
}

# check article id duplicates

if(length(kd$title[duplicated(kd$article_id)]) >0 | length(mg$title[duplicated(mg$article_id)]) >0){
  stop("kd or mg have duplicated titles")
}


###########
# merge####
##########

# join sets of articles assessed by md and kd. Add suffix's to indicate who's columns are whos
both <- full_join(kd, mg, by = "article_id", suffix = c(".kd", ".mg"))

# order column names alphabetically & put article_id and title first 

both <- both[,order(colnames(both))] %>%
  select(article_id, title.kd, title.mg, everything())

#################
# find title conflicts ####
##################

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

# create df
title_cons <- both

# clean title strings for better matching

# clean title strings

title_cons$title.kd <- clean_string(title_cons$title.kd)
title_cons$title.mg <- clean_string(title_cons$title.mg)

title_cons <- title_cons[which(title_cons$title.kd != title_cons$title.mg),] %>%
  .$article_id

title_cons <- cbind(both$title.kd[both$article_id %in% title_cons], both$title.mg[both$article_id %in% title_cons])


conflict <- both[which(both$design.kd != both$design.mg),]%>%
  select(c(design.kd, design.mg), everything())

no_conflict <- both[which(both$design.kd == both$design.mg),]%>%
  select(c(design.kd, design.mg), everything())


# export ####

write.csv(df, "outputs/clean_designs.csv", row.names = F)
