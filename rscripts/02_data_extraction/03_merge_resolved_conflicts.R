###############
# libraries #####
#############

library(dplyr)
library(magrittr)
library(gtools)

##########
# import ####
##########

import <- function(df_path){
  # import header to check names
  df <- read.csv(df_path, stringsAsFactors = F, encoding = "UTF-8", nrow = 1, check.names = F, na.strings = c("", " "))
  # check no colnames duplicated
  if(any(duplicated(colnames(df)))) stop("duplicated colnames")
  # check no colnames missing
  if(any(is.na(colnames(df))) | any(colnames(df) %in% c("", " "))) stop("missing colnames")
  # read in if passes name check
  df <- read.csv(df_path, stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " "))
  return(df)
}

bw <- import("data/kd-bw-articles_conflicts.csv")
rr <- import("data/kd-rr-articles_conflicts.csv")
mg <- import("data/kd-mg-articles_conflicts.csv")

########################
# save single coded variables ####
######################

# save stems of variables that were not double coded (and therefore not conflicted)
# kd alone extracted some variables so save all these variables single/unique variables
kd_only <- c("email","country","ukb_app","keywords","coi", "article_id")
#  qualtrics metadata, comments & title substring vars are unique to each coder so stems duplicated but not colnames >
qualtrics <- c("distribution_channel", "duration_in_seconds", "end_date", "finished", 
               "progress", "recorded_date", "response_id", "response_type", "start_date", "user_language")
single <- c(kd_only, qualtrics, "comments", "title_clean", "title_sub", "title")

################
# check suffixes all correct ####
##############
 
check_suf <- function(df){
  # just save colnames
  x <- colnames(df)
  # save suffixes used to indicate which column relates to which coder
  suf <- c(".bw", ".rr", ".kd", ".mg", ".correct")
  # check all suffixes should come at end of col names >
  # save patterns to find columns containing suffixes at end or not
  pat_no_dollar <- paste0("\\", suf,collapse ="|")
  suf_pat <- paste0("\\", suf, "$", collapse ="|")
  # check results with both patterns are same
  if(identical(grep(suf_pat, x), grep(pat_no_dollar, x))) print("all suffixes come at end") else stop("coder suffixes not all at end")
  # check all columns containing suffixes are correct >
  # save cols containing "."
  dot_cols <- grep("\\.", x, value =T) %>%
    # remove all text before suffix
    gsub(".*\\.", "", .) %>%
    # add full stop back in because pattern inclusive of last "." so will match with suf
    paste0(".", .)
  # check all suffixes are in suf
  if(all(dot_cols %in% suf)) print("all suffixes correct") else stop("suffixes incorrect")
  # check all columns with suffixes are double coded >
  # save suffixes used to indicate which column relates to which coder
  coder <- c(".bw", ".rr", ".kd", ".mg")
  # save pattern to find suffixes in grep
  coder_pat <- paste0("\\", coder, "$", collapse ="|")
  # save cols containing coder suffixes
  suf_cols <- grep(coder_pat, x, value = T) %>%
    # remove suffixes
    gsub(coder_pat, "", .)
  # check all suf_cols are duplicated (i.e. double coded)
  if(all(table(suf_cols) %in% 2)) print("all double coded cols double coded") else stop("double coded not double coded")
  # check all columns not containing suffixes are the kd_only  >
  # find colnames not containing coder suffixes
  match <- x[grepl(suf_pat, x) == F]
  if(identical(sort(kd_only), sort(match))) print("kd_only cols single coded") else stop("kd only cols not single coded")
  # check all ".correct" cols are duplicates of coder cols>
  # save stem of all cols with .correct suffix
  correct_cols_pat <- grep("\\.correct$", x, value = T) %>%
    # remove correct suffix
    gsub("\\.correct$", "", .) %>%
    # add escaped "." at end so will only search up to suffix
    paste0("\\.", "", collapse = "|")
  # save all cols that have a correct version
  corrected <- grep(correct_cols_pat, x, value = T) %>%
    # remove suffixes
    gsub(suf_pat, "", .) %>%
    # remove  kd_only from corrected because wasn't double coded so wont be double coded
    .[!(. %in% kd_only)]
  # check all corrected are duplicated (i.e. double coded)
  if(all(table(corrected) %in% 3)) print(".correct cols triple coded") else warning(".correct cols not triple coded")
}

check_suf(rr)
check_suf(bw)
check_suf(mg)

################
# kd to check ####
##################

find_checks <- function(df){
  cols_to_check <- c("article_id", "title.kd")
  for(i in colnames(df)){
    x <- any(grep("kd to check", df[[i]]))
    if(x ==T){
      df[[i]][grepl("kd to check", df[[i]]) == F] <- NA
      cols_to_check <- c(cols_to_check, i)
    }
  }
  df <- df[, cols_to_check]
  row_to_check <- c()
  for(i in 1:nrow(df)){
    x <- any(grep("kd to check", as.character(df[i, ])))
    if(x ==T){
      row_to_check <- c(row_to_check, i)
    }
  }
  df <- df[row_to_check, ]
  if(identical(cols_to_check, c("article_id", "title.kd"))){
    print("no kd to check values")
  } else {
    return(df)
  }
}


mg_check <- find_checks(mg)
rr_check <- find_checks(rr)
bw_check <- find_checks(bw)


find_becky_its <- function(df){
  cols_to_check <- c("article_id", "title.kd")
  for(i in colnames(df)){
    x <- any(grep("becky it", df[[i]]))
    if(x ==T){
      df[[i]][grepl("becky it", df[[i]]) == F] <- NA
      cols_to_check <- c(cols_to_check, i)
    }
  }
  df <- df[, cols_to_check]
  row_to_check <- c()
  for(i in 1:nrow(df)){
    x <- any(grep("becky it", as.character(df[i, ])))
    if(x ==T){
      row_to_check <- c(row_to_check, i)
    }
  }
  df <- df[row_to_check, ]
  return(df)
  if(identical(cols_to_check, c("article_id", "title.kd"))){
    print("no becky it values")
  } else {
    return(df)
  }
}


mg_becky_it <- find_becky_its(mg)
rr_becky_it <- find_becky_its(rr)
bw_becky_it <- find_becky_its(bw)

######################
# drop evidence and star columns & set duplicates ####
#######################

drop_ev_star <- function(df){
  # save colnames
  x <- colnames(df)
  # remove all evidence cols as conflicts resolved so no longer needed
  # also remove star items because too conflicted to resolve
  df <- df[, -grep("_ev\\.|_star_", colnames(df))]
  
  return(df)
} 

rr <- drop_ev_star(rr)
bw <- drop_ev_star(bw)
mg <- drop_ev_star(mg)

##################
# remove prediction papers ####
####################

# save dfs with prediction papers
rr_pred <- rr
bw_pred <- bw
mg_pred <- mg

# create function to update kd only coded variables with corrected value

update_kd_only <- function(df, kd_col){
  cor_name <- paste0(kd_col, ".correct")
  if(cor_name %in% colnames(df)){
    # check sing_col and correct cols all in df and only 1 match
    if(length(cor_name %in% colnames(df)) != 1 & length(kd_col %in% colnames(df)) != 1) stop(kd_col, " not in df or has multiple matches")
    # find values that have a corrected value
    cors <- which(!is.na(df[[cor_name]]))
    # correct values by replacing with corrected one
    df[[kd_col]][cors] <- df[[cor_name]][cors]
    # drop correct col
    df <- df[, -which(colnames(df) %in% c(cor_name))]
  }
  return(df)
}

# create function to combine double coded variables in dfs if all conflicts resolved

resolve_conflict <- function(df, col_stem, coder_2){
  # save .kd column that starts with col_stem
  kd_name <- paste0(col_stem, ".kd")
  # save coder 2 column that starts with col_stem
  b_name <- paste0(col_stem, coder_2)
  # save correct column - this will be col_stem pasted with .correct
  cor_name <- paste0(col_stem, ".correct")
  # check kd, coder 2, and correct cols all in df and only 1 match
  if(length(kd_name %in% colnames(df)) != 1 & length(b_name %in% colnames(df)) != 1) stop(col_stem, " not in df or has multiple matches")
  if(!(cor_name %in% colnames(df))){
    # add correct column so can match on it later
    df[[cor_name]] <- rep(NA, nrow(df))
  }
  if(is.null(df[[kd_name]])) stop(kd_name, " doesn't exist")
  if(is.null(df[[b_name]])) stop(b_name, " doesn't exist")
  if(is.null(df[[cor_name]])) stop(cor_name, " doesn't exist")
  # create df of kd and coder2's columns that don't match and include NAs in matching
  if(identical(df[[kd_name]], df[[b_name]]) & all(is.na(df[[cor_name]]))){
    df[[col_stem]] <- df[[kd_name]]
    df <- df[, -which(colnames(df) %in% c(kd_name, b_name, cor_name))]
  } else {
    cons <- which((!is.na(df[[kd_name]]) & !is.na(df[[b_name]]) & df[[kd_name]]==df[[b_name]] | is.na(df[[kd_name]]) & is.na(df[[b_name]])) == F)
    # if all conflicts have a correct value then reolace empty correct values with unconflicted values
    if(all(!is.na(df[[cor_name]][cons]))) {
      df[[col_stem]] <- df[[cor_name]]
      uncon <- which(is.na(df[[col_stem]]))
      df[[col_stem]][uncon] <- df[[kd_name]][uncon]
      df <- df[, -which(colnames(df) %in% c(kd_name, b_name, cor_name))]
    } else {
      message(col_stem, " has unresolved conflicts")
    }
  }
  if(all(is.na(df[[cor_name]]))){
    df[[cor_name]] <- NULL
  }
  df <- df[ , gtools::mixedorder(colnames(df))]
  if(any(duplicated(colnames(df)))) stop("columns duplicated")
  return(df)
}

# resolve predict conflicts
rr <- resolve_conflict(rr, "predict", ".rr")
bw <- resolve_conflict(bw, "predict", ".bw")
mg <- resolve_conflict(mg, "predict", ".mg")

# remove prediction papers
rr <- rr[!rr$predict %in% "Yes", ]
bw <- bw[!bw$predict %in% "Yes", ]
mg <- mg[!mg$predict %in% "Yes", ]

###################
# calculate irr at subdiv level ####
##################

# merge and keep conflicts to calculate interrater reliability

create_inter_df <- function(){
  # replace coder2 suffixes with same suffix so cols will match
  colnames(bw) <- gsub(".bw$", ".coder2", colnames(bw))
  colnames(rr) <- gsub(".rr$", ".coder2", colnames(rr))
  colnames(mg) <- gsub(".mg$", ".coder2", colnames(mg))
  # join dfs
  bw_rr <- full_join(bw, rr, by = intersect(colnames(bw), colnames(rr)))
  inter <- full_join(bw_rr, mg, by = intersect(colnames(bw_rr), colnames(mg))) %>%
    # reorder for ease
    .[ , gtools::mixedorder(colnames(.))]
  # drop all correct cols because not necessary for irr calculation
  inter <- inter[, -grep(".correct$", colnames(inter))]
  # set all values for duplicates and not applicable items to NA
  drop_pat <- paste(c("5_vi", "s6a_iii", "10", "8starred_iii"), collapse = "|")
  # set all to NA
  for(i in grep(drop_pat, colnames(inter), value = T)){
    inter[[i]] <- rep("NA", length(inter[[i]]))
  }
  
  return(inter)
}

inter <- create_inter_df()

write.csv(inter, "outputs/data-extraction-form-conflicted.csv", row.names = F, fileEncoding = "UTF-8", na = "")


##################
# merge without conflicts ####
################

# function to check conflicts
save_col_stems <- function(df){
  # save suffixes used to indicate which column relates to which coder
  suf <- c(".bw", ".rr", ".kd", ".mg", ".correct")
  # save patterns to find columns containing suffixes at end or not
  suf_pat <- paste0("\\", suf, "$", collapse ="|")
  # remove suffixes to leave stems
  cols_stem <- gsub(suf_pat, "", colnames(df))
  # read in extraction dictionary to check all colstems in dict
  dict <- read.csv("outputs/extraction_dict.csv", stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " "))
  # remove duplicated
  cols_stem  <- cols_stem[!duplicated(cols_stem)] %>%
    # remove single coded items since these can't be conflicted
    .[!. %in% single] %>%
    # remove predict because we already resolved these conflicts
    .[!. %in% "predict"] 
  if(!all(cols_stem %in% dict$variable)){
    count <- sum(!cols_stem %in% dict$variable)
    warning(count, " col stems not in extraction dictionary")
    return(cols_stem)
    stop()
  } else {
    return(cols_stem)
  }
}

for(i in kd_only){
  bw <- update_kd_only(bw, i)
}

for(i in kd_only){
  rr <- update_kd_only(rr, i)
}

for(i in kd_only){
  mg <- update_kd_only(mg, i)
}

for(i in save_col_stems(bw)){
  bw <- resolve_conflict(bw, i, ".bw")
}

for(i in save_col_stems(mg)){
  mg <- resolve_conflict(mg, i, ".mg")
}

for(i in save_col_stems(rr)){
  rr <- resolve_conflict(rr, i, ".rr")
}


merge_all_three <- function(){
  qualtrics <- c("distribution_channel", "duration_in_seconds", "end_date", "finished", 
                 "progress", "recorded_date", "response_id", "response_type", "start_date", "user_language")
  unique <- c(qualtrics, "comments", "title_clean", "title_sub")
  rr[, setdiff(colnames(bw), colnames(rr))] <- NA
  bw[, setdiff(colnames(rr), colnames(bw))] <- NA
  x <- rbind(rr, bw)
  mg[, setdiff(colnames(x), colnames(mg))] <- NA
  x[, setdiff(colnames(mg), colnames(x))] <- NA
  x <- rbind(x, mg)
  x <- x[ , gtools::mixedorder(colnames(x))]
  if(any(duplicated(colnames(x)))) stop("columns duplicated")
  return(x)
}

df <- merge_all_three()

####################
# clean strobe responses ####
####################

# catergories should only be a few check this

find_all_values <- function(df){
  # remove all columns that aren't strobe variables (i.e. don't begin with s[digit])
  df <- df[, grep("^s[0-9]", colnames(df))]
  ls <- list()
  for(i in colnames(df)){
    ls[[i]] <- unique(df[[i]])
  }
  ls <- ls[lengths(ls) != 33]
  ls <- lapply(ls, `length<-`, max(lengths(ls)))
  x <- as.data.frame(ls)
  all_value <- c()
  for(i in 1:ncol(x)){
    all_value <- sort(c(all_value, as.character(x[,i])))
  }
  return(unique(all_value))
}

clean_values <- function(){
  
  vals <- find_all_values(df)
  rules<- sort(unique(grep("rule", vals, value = T, ignore.case = T))) %>%
    # remove any text that comes before rule, this will clean any like 'NA - resolved but Rule ...'
    gsub(".*Rule", "Rule", .)
  # save rules in rule dict so can check all existing rule values are correct
  rules_dict <- read.csv("data/rules.csv", stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " "), )
  # check all rule values correct
  if(length(setdiff(rules, rules_dict$rule)) != 0) stop("some rule values wrong")
  
  
  # replace rule responses with value rule applies to
  
  for(i in 1:length(colnames(df))){
    col <- df[, i]
    if(any(grepl("Rule =", col, ignore.case = T))){
      for(j in 1:length(rules_dict$rule)){
        rule <- rules_dict$rule[j]
        col[grep(rule, col)] <- rules_dict$response[j]
      }
    }
    df[, i] <- col
  }
  
  
  # clean non-strobe items
  
  for(i in colnames(df)){
    # save values that indicate a resolved conflict for a strobe item so can clean them>
    # these will be responses to strobe items with the name of the coder who resolved them after the value >
    # only becky, I, or both of us resolved them so it will be "becky" or "me"
    res <- c(" becky ",  "becky$", " me ", " me$", " us ", "us$") 
    # save as a pattern so can use in gsub
    pat <- paste(res, collapse = "|")
    x <- df[[i]]
    if(any(grepl(pat, x, ignore.case = T))){
      ids <- grep(pat, x, ignore.case = T)
      x[ids] <- gsub(" .*$", "", x[ids])
      df[[i]] <- x
    }
  }
  return(df)
}

df <- clean_values()

###########################
# merge in epi_access ####
#####################

# add in whether they had supplementary material, this is in the first_80 ref csv
first_80 <- read.csv("https://osf.io/9w72e/?action=download", encoding = "UTF-8", na.strings = c("", " ")) %>%
  .[, which(colnames(.) %in% c("article_id", "access_supp"))]

# I removed Davie from the first_80 in the first revision >
# add this back in because we decided it did actually contain a cohort design

first_80 <- rbind(first_80, c("Davie2018bank79-y", "Yes"))

epi_access <- read.csv("data/epi_access.csv", stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " ")) %>%
  .[, which(!colnames(.) %in% c("doi", "title", "authors"))]

epi_access <- left_join(first_80, epi_access, by = "article_id")

df <- left_join(df, epi_access, by = "article_id")

# TEMPORARY REPLACE BECKY IT VALUES AS NA
 
for(i in colnames(df)){
  if(any(grepl("becky", df[[i]]))) print(i)
  becky_its <- grep("becky it", df[[i]], ignore.case = T)
  df[[i]][becky_its] <- NA
}

# add check that only values with resolved me or becky tags have a space

write.csv(df, "outputs/data-extraction-form-clean.csv", row.names = F, fileEncoding = "UTF-8", na = "")
