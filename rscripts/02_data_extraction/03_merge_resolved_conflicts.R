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
    gsub(suf_pat, "", .)
  # check all corrected are duplicated (i.e. double coded)
  if(all(table(corrected) %in% 3)) print(".correct cols triple coded") else warning(".correct cols not triple coded")
  # check all columns not containing suffixes are the single coded variables >
  # save single coded variables or those merged in previous code
  single <- c("email","country","ukb_app","keywords","coi", "article_id")
  # find colnames not containing coder suffixes
  match <- x[grepl(suf_pat, x) == F]
  if(identical(sort(single), sort(match))) print("single coded cols single coded") else stop("single coded cols not single coded")
}

check_suf(rr)
check_suf(bw)
check_suf(mg)

####################
# check responses ####
####################

# catergories should only be a few check this

find_all_values <- function(df){
  # remove evidence columns 
  df <- df[, -grep("_ev\\.", colnames(df))]
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
  return(all_value)
}

rr_values <- find_all_values(rr)
mg_values <- find_all_values(mg)
bw_values <- find_all_values(bw)

rr_rules<- sort(unique(grep("rule", rr_values, value = T, ignore.case = T))) %>%
  gsub(".*Rule", "Rule", .)
mg_rules<- sort(unique(grep("rule", mg_values, value = T, ignore.case = T)))%>%
  gsub(".*Rule", "Rule", .)
bw_rules<- sort(unique(grep("rule", bw_values, value = T, ignore.case = T)))%>%
  gsub(".*Rule", "Rule", .)

rules_dict <- read.csv("data/rules.csv", stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " "), )

rr_rules[which(!rr_rules %in% rules_dict$rule)]
mg_rules[which(!mg_rules %in% rules_dict$rule)]
bw_rules[which(!bw_rules %in% rules_dict$rule)]


rr_not_rules<- sort(unique(rr_values[grepl("rule", rr_values, ignore.case = T) == F]))
mg_not_rules<- sort(unique(mg_values[grepl("rule", mg_values, ignore.case = T) == F]))
bw_not_rules<- sort(unique(bw_values[grepl("rule", bw_values, ignore.case = T) == F]))

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
    x <- any(grep("becky", df[[i]]))
    if(x ==T){
      df[[i]][grepl("becky", df[[i]]) == F] <- NA
      cols_to_check <- c(cols_to_check, i)
    }
  }
  df <- df[, cols_to_check]
  row_to_check <- c()
  for(i in 1:nrow(df)){
    x <- any(grep("becky", as.character(df[i, ])))
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
# drop evidence and star columns ####
#######################

drop_ev_star <- function(df){
  # save colnames
  x <- colnames(df)
  # remove all evidence cols as conflicts resolved so no longer needed
  # also remove star items because too conflicted to resolve
  df <- df[, -grep("_ev\\.|_star_", colnames(df))]
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

#####################
# MANUAL CHECK & TEMPORARY ####
#####################
# remove articles that are currently not double coded 
# Becky is in the process of extracting these
remove_single_coded <- function(df, coder_2_suffix){
  df_orig <-df
  kd_name <- "s20.kd"
  b_name <- paste0("s20", coder_2_suffix)
  if(is.null(df[[kd_name]])) stop(kd_name, " doesn't exist")
  if(is.null(df[[b_name]])) stop(b_name, " doesn't exist")
  x <- df$article_id[is.na(df[[kd_name]]) |is.na(df[[b_name]])]
  if(length(x) > 0){
    warning("removed ", length(x), " single coded articles")
    df <- df[-which(df$article_id %in% x), ]
    return(df)
  } else {
    print("all articles double coded")
    if(!identical(df, df_orig)) stop("df changed even though no articles double coded")
    return(df)
  }
}

mg <- remove_single_coded(mg, ".mg")
bw <- remove_single_coded(bw, ".bw")
rr <- remove_single_coded(rr, ".rr")

##################
# merge without prediction papers ####
################

# function to check conflicts
save_col_stems <- function(df){
  # save suffixes used to indicate which column relates to which coder
  suf <- c(".bw", ".rr", ".kd", ".mg", ".correct")
  # save patterns to find columns containing suffixes at end or not
  suf_pat <- paste0("\\", suf, "$", collapse ="|")
  #  qualtrics metadata and comments vars are double coded but are unique to each coder >
  # kd alone extracted some variables so save all these variables single/unique variables
  kd_only <- c("email","country","ukb_app","keywords","coi", "article_id")
  qualtrics <- c("distribution_channel", "duration_in_seconds", "end_date", "finished", 
                 "progress", "recorded_date", "response_id", "response_type", "start_date", "user_language")
  single <- c(qualtrics, "comments", kd_only)
  # remove suffixes to leave stems
  cols_stem <- gsub(suf_pat, "", colnames(df))
  # read in extraction dictionary to check all colstems in dict
  dict <- read.csv("outputs/extraction_dict.csv", stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " "))
  # remove duplicated
  cols_stem  <- cols_stem[!duplicated(cols_stem)] %>%
    # remove single coded items since these can't be conflicted
    .[!. %in% single] %>%
    # remove predict because we already resolved these conflicts
    .[!. %in% "predict"] %>%
    # remove title substring variables
    .[!. %in% c("title_clean", "title_sub")]
  if(!all(cols_stem %in% dict$variable)){
    count <- sum(!cols_stem %in% dict$variable)
    warning(count, " col stems not in extraction dictionary")
    return(cols_stem)
    stop()
  } else {
    return(cols_stem)
  }
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


#################
# find any remaining conflicts ######
##################

#grep("\\.kd$", colnames(rr), value = T)
#grep("\\.kd$", colnames(mg), value = T)
#grep("\\.kd$", colnames(bw), value = T)
