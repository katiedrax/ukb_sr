#############
# libraries ####
#############

library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)

###############
# functions ####
##############

# create clean string function

# function to clean string 
clean_string <- function(string){
  if(is.character(string) == F) stop("string not character")
  if(identical(string, character(0)) == T) stop("string empty")
  # string: a vector of strings
  # remove any non-english character, numbers, spaces or punctuation and lower
  string <- gsub("[^\u0001-\u007F]+|<U\\+\\w+>","", string)
  string <- gsub("[0-9]", "", string)
  string <- gsub("[[:punct:]]", "", string)
  string <- tolower(string)
  string <- gsub("[[:space:]]", "", string)
  return(string)
}

##########
# import####
##########

# assign input file 

input <- "outputs/clean_extraction_form.csv"
# Import and set NA and strip white space to clean free text answers

df <- read.csv(input, encoding = "UTF-8", stringsAsFactors = F, header = T, na.strings = c("", " "),strip.white = T)

# strip white from all columns just incase strip.white read in didn't work
df <- as.data.frame(lapply(df, trimws, "both"))

# remove survey previews as these are not valid responses
df <- df[df$response_type != "Survey Preview", ]

################################################
# check all entries have article id and title ####
##############################

# some rows may have no article_id or title >
# can remove rows with too many NAs to identify the article they relate to

if(sum(is.na(df$article_id)| is.na(df$title)) != 0){
  # limit of NAs that will make article unidentifiable or ob useless is 99% NA values
  na_limit <- ncol(df) - (ncol(df) * 0.01)
  # find all obs missing article_id or title
  miss <- df[is.na(df$article_id) | is.na(df$title), ]
  # count number of NAs in each obs
  miss$na_sum <- apply(miss, 1, function(x) sum(is.na(x)))
  # remove all obs missing article_id or title if all contain too many NAs to be identifiable
  if(all(miss$na_sum > na_limit)) {
    # remove all rows missing article_id or title
    df <- df[!is.na(df$article_id)| !is.na(df$title), ]
    print(paste(nrow(miss), " row missing 99% values, including article_id and title, removed"))
  } else {
    stop("articles missing id or title could be identifiable")
  }
} else {
  print("no articles missing article_id or title")
}

###############
# check article_id and title ####
################

# df of all articles in csv_clean_epi.csv on OSF to compare

osf <- read.csv("https://osf.io/8uy9w/?action=download", encoding = "UTF-8", stringsAsFactors = F)

# set id col to articles_id so matches df

colnames(osf)[colnames(osf) == "id"] <- "article_id"

# check all article ids in df are correct so can just keep subset of osf that is in df

if(all(df$article_id %in% osf$article_id)){
  osf <- osf[osf$article_id %in% df$article_id, ]
}

# create cleaned title string in osf and df for easier matching
osf$title_clean <- clean_string(osf$title)
df$title_clean <- clean_string(df$title)

# check no titles in osf duplicated, df will have duplicates because should be 2 entries for each article_id
if(any(duplicated(osf$title_clean))) stop("duplicated title in osf")

# save names of cols needed to check article_id and titles
chk_cols <- c("title", "article_id", "title_clean")

# check all df$article_ids in osf, otherwise some ids may have the wrong titles
chk <- merge(df[, chk_cols], osf[, chk_cols], 
              by = c("title_clean", "article_id"), suffixes = c(".df", ".osf"), all = T)

# save all article_ids that failed to merge (meaning the titles are wrong in df or osf)
wrong <- chk[is.na(chk$title.df) | is.na(chk$title.osf), ]

###############
# MANUAL CHECK ####
############

# any obs in wrong with NA in title.df mean that the article_id-title pair is only in OSF >
# this could be because they are mispelt in OSF, and correctly when extracted >
# MANUAL check this by finding online version of article for article_id and checking spelling in osf
wrong_osf_id <- wrong$article_id[is.na(wrong$title.df)]
wrong_osf_title<- osf$title[osf$article_id == wrong_osf_id]


# manual check revealed the title in mispelled so correct it
osf$title[osf$article_id == wrong_osf_id] <-  "Associations of Leg Length, Trunk Length, and Total Adult Height With Ménière's: Cross-Sectional Analysis in the UK Biobank"

# last time code ran there was 1 article in wrong_osf_id, check this is still true
n_wrong_osf <- 1
if(length(wrong_osf_id) != n_wrong_osf){
  rm(osf)
  stop("more than 1 article is just in osf and not df")
}

# merge chk and find wrong subset again so includes corrected osf title
chk <- merge(df[, chk_cols], osf[, chk_cols], 
               by = c("title_clean", "article_id"), suffixes = c(".df", ".osf"), all = T)

# save all article_ids that failed to merge (meaning the titles are wrong in df or osf)
wrong <- chk[is.na(chk$title.df) | is.na(chk$title.osf), ]

# find rows in wrong that have now been corrected
id <- which(wrong$article_id == wrong_osf_id & is.na(wrong$title.df))

# should only be dropping wrong osfs that have been corrected, check this
if(nrow(wrong[-id, ]) == nrow(wrong)- n_wrong_osf) {
  # drop corrected osf title from wrong because know it is right now
  wrong <- wrong[-id,]
  # all dropped titles should be the ones only in OSF, so should be no NA values in title.df now
  if(any(is.na(wrong$title.df))) stop("title.df col missing somes values")
} else {
  stop("more than 1 articles removed from wrong")
}

# save article_ids in wrong
wrong_id <- unique(wrong$article_id)
# add names to wrong so will save names in lapply
names(wrong_id) <- wrong_id

# create list of all titles of article_ids in wrong and check titles are correct but just mispelt
manual_chk <- lapply(wrong_id, function(x){
  # find them in df since all osf titles now corrected
  y <- df$title[df$article_id == x]
})

writeLines(unlist(manual_chk), "outputs/manual-chk.txt")

# MANUAL - all titles in manual_chk correct, just unicode characters created mismatches and one title in osf was mispelt
# manually corrected mispelt title in osf meaning all osf titles now correct >
# so replace all titles in df with osf titles

for(i in 1:nrow(df)){
  df$title_correct[i] <- osf$title[osf$article_id == df$article_id[i]]
}

# chk title.df and title.osf mismatched if >
# 1) cleaned titles didn't match so dataframes failed to merge >
# 2) cleaned titles matched but titles still mismatched because had different capitalisation/punctuation >
# articles with title_correct value should == NA title.osf, NA title.df, or title.df != title.osf in chk
# check this

x <- chk$article_id[chk$title.df != chk$title.osf | is.na(chk$title.df) |is.na(chk$title.osf)] %>%
  unique() %>%
  sort()

corrected <- df$article_id[df$title_correct != df$title] %>%
  unique() %>%
  sort()

if(identical(x, corrected)){
  df$title <- df$title_correct
  df$title_correct <- NULL
  print("corrected the right titles")
} else {
  df <- NULL
  stop("corrected the wrong titles or some titles not correcte")
}

############################
# MANUAL remove duplicate articles ####
#########################

# function to find any duplicate articles for each initial
find_duplicate_articles <- function(){
  # get vector of unique initials to find duplicate articles for each initial
  initials <- unique(df$initials)
  # add names of initials so list output from lapply will have names
  names(initials) <- initials
  # loop for each initial
  dup_list <- lapply(initials, function(initial){
    # find all articles for initial x so can search for duplicates
    initial_articles <- df[df$initials == initial, "article_id"]
    # find values of duplicated articles so can find position in df 
    dup_article <- initial_articles[duplicated(initial_articles)]
    # nest for loop to find position of each duplicated article for each initial in df
    lapply(dup_article, function(x){
      which(df$initials == initial & df$article_id == x)
    })
  })
  # unlist and return dup_list if is same length as initials
  if(identical(length(dup_list), length(initials))){
    # unlist if dup_list contains dups/statement of no dups for each coder
    dups <- unlist(dup_list) 
    # return 
    # return dups if numeric vector so can use to find subset of duplicates in df
    if(is.numeric(dups)) return(dups) 
    } else {
      stop("dup_list missing some initials")
    }
}

dups <- find_duplicate_articles() %>%
  # subset df to find dups
  df[., ]

coder <- unique(df$initials)
names(coder) <- coder
y <- lapply(coder, function(x){
  coder_dups <- dups[dups$initials %in% x, ] 
  predict_id <- coder_dups$article_id[coder_dups$predict %in% "Yes"]
  which(df$article_id %in% predict_id & !(df$predict %in% "Yes") &df$initials %in% x)
})

df <- df[-unlist(y),]

# MANUAL - I inspected the duplicates and found which were incomplete/out of date >
# manual because automating would be complicated >
# e.g. remove prediction papers by each initial to ensure don't accidentally remove all prediction papers not labelled as such

MANUAL_remove_duplicate_articles <- function(){
  # One Foste2018hort00-7 entry by MG is incomplete
  mg_dup <- which(df$article_id =="Foste2018hort00-7" & df$finished %in% "False" & df$initials == "MG")
  # one Morri2018bank.496	entry by RR is incomplete
  rr_dup <- which(df$article_id =="Morri2018bank.496" & df$finished %in% "False" & df$initials == "RR")
  df <- df[-c(mg_dup, rr_dup),]
}

# apply function 
df <- MANUAL_remove_duplicate_articles()

# check there are no duplicate articles for any initials
dups <- find_duplicate_articles()

if(length(dups) != 0){
  warning(paste(names(which(lengths(dups) >0)), collapse = " "), " have duplicate article_ids")
} else {
  print("no coder has any duplicate articles")
}

###########
# merge KD with each coder ####
##########

merge_with_kd <- function(initials_coder_2){
  # copy of input
  init_2 <- initials_coder_2
  # update substring of title so always reflects correct title
  df$title_sub <- clean_string(df$title)
  # all init_2 entries should merge with a KD entry >
  # check this using anti_join on init_2, any non-matches should just be KD entires
  just_init_2 <- anti_join(df[df$initials == init_2, ], df[df$initials == "KD", ], by = "article_id")
  b <- anti_join(df[df$initials == "KD", ], df[df$initials == init_2, ], by = "article_id")
  if(nrow(just_init_2) >0) warning("some ", init_2, " entires not in KD entries")
  # create suffix for init_2
  suffix_2 <- paste0(".", tolower(init_2))
  # join sets of articles assessed by md and kd. Add suffix's to indicate who's columns are whos >
  # only inner join
  both <- merge(df[df$initials == "KD", ], df[df$initials == init_2, ], 
                by = "article_id", suffixes = c(".kd", suffix_2))
  # create vector of columns to call in select
  cols <- c("article_id", "title.kd", paste0("title", suffix_2))
  # order columns using mixedorder so strobe items keep numerical order
  both <- both[ , gtools::mixedorder(colnames(both))] %>%
    # put article_id and title first >
    # cols is external vector of variable names so need to use all_of to remove ambiguity in tidyverse
    select(all_of(cols), everything())
  # save name of coder 2 initial col
  initial_col_2 <- paste0("initials", suffix_2)
  # drop initials columns as only needed for merge
  both$initials.kd <- NULL
  both[initial_col_2] <- NULL
  # save name of coder 2 title_sub col
  title_sub_2 <- paste0("title_sub", suffix_2)
  # find any conflicting title substrings 
  title_cons <- which(both$title_sub.kd != both[title_sub_2])
  # stop if  are any title_cons, should be none after manual correction
  if(length(title_cons) >0) stop("title_sub cols of KD and ", initials_coder_2, " conflict in rows ", title_cons)
  # only kd extracted some variables but merge will create a copy for coder 2 >
  # save vector of variables only kd extractor
  kd_only <- c("email","country","ukb_app","keywords","coi")
  # save vector coder 2 copy of kd_only cols
  drop_2 <- paste0(kd_only, suffix_2)
  # save names of remaining versions of identical cols so can remove suffix
  kd_to_rename <- paste0(kd_only, ".kd")
  # drop drop_2 and emove suffix from kd_to_rename if all in colnames
  if(all(drop_2 %in% colnames(both)) & all(kd_to_rename %in% colnames(both))){
    both <- both[, -which(colnames(both) %in% drop_2)]
    colnames(both)[colnames(both) %in% kd_to_rename] <- gsub("\\..*","", colnames(both)[colnames(both) %in% kd_to_rename])
  }
  # return
  return(both)
}



kd_mg <- merge_with_kd("MG")

# merge kd with RR
kd_rr <- merge_with_kd("RR")

# merge kd with RR
kd_bw <- merge_with_kd("BW")

###########
# export ####
##########

# merged dfs of each kd-coder pair will only contain articles that kd and coder_2 did >
# single coded articles will be in df but no merged df >
# export if everything double coded

merged_id <- unique(c(kd_bw$article_id, kd_rr$article_id, kd_mg$article_id))
if(all(df$article_id %in% merged_id)){
  write.csv(kd_bw, "outputs/kd-bw-articles.csv", row.names = F, fileEncoding = "UTF-8", na = "")
  write.csv(kd_mg, "outputs/kd-mg-articles.csv", row.names = F, fileEncoding = "UTF-8", na = "")
  write.csv(kd_rr, "outputs/kd-rr-articles.csv", row.names = F, fileEncoding = "UTF-8", na = "")
} else {
  # save list of ids only single coded for stop message
  single <- df$article_id[!(df$article_id %in% merged_id)]
  # print error
  stop(length(single)," articles not double coded")
}