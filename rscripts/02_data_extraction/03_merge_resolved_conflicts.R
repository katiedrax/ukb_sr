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
  df <- read.csv(df_path, stringsAsFactors = F, encoding = "UTF-8", nrow = 1, check.names = F)
  # check no colnames duplicated
  if(any(duplicated(colnames(df)))) stop("duplicated colnames")
  # check no colnames duplicated
  if(any(is.na(colnames(df)))) stop("missing colnames")
  # check no colnames duplicated
  if(any(colnames(df) %in% c("", " "))) stop("missing colnames")
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

######################
# drop evidence columns ####
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

##########
# merge ####
#########

# save suffixes used to indicate which column relates to which coder
suf <- c(".bw", ".rr", ".kd", ".mg", ".correct")
# save patterns to find columns containing suffixes at end or not
suf_pat <- paste0("\\", suf, "$", collapse ="|")

# function to check conflicts
resolve_conflicts <- function(df, coder_2){
  # save suffixes used to indicate which column relates to which coder
  suf <- c(".bw", ".rr", ".kd", ".mg", ".correct")
  # save patterns to find columns containing suffixes at end or not
  suf_pat <- paste0("\\", suf, "$", collapse ="|")
  # save single coded items
  single <- c("email","country","ukb_app","keywords","coi", "article_id")
  cols_stem <- gsub(suf_pat, "", colnames(df)) %>%
    # remove duplicated
    .[!duplicated(.)] %>%
    # remove single coded items since these can't be conflicted
    .[!. %in% single]
  # for each double coded variable, find the conflicts
  x <- list()
  for(i in cols_stem){
    kd_name <- paste0(i, ".kd")
    b_name <- paste0(i, coder_2)
    cor_name <- paste0(i, ".correct")
    if(!(kd_name %in% colnames(df)) & !(b_name %in% colnames(df))) stop(i, " not in df")
    if(!(cor_name %in% colnames(df))){
      df[[cor_name]] <- rep(NA, nrow(df))
    }
    if(is.null(df[[kd_name]])) stop(kd_name, " doesn't exist")
    if(is.null(df[[b_name]])) stop(b_name, " doesn't exist")
    if(is.null(df[[cor_name]])) stop(cor_name, " doesn't exist")
    # create df of coder1 and coder2's columns that don't match and include NAs in matching
    if(identical(df[[kd_name]], df[[b_name]]) & all(is.na(df[[cor_name]]))){
      df[[i]] <- df[[cor_name]]
      df <- df[, -which(colnames(df) %in% c(kd_name, b_name, cor_name))]
    } else {
      cons <- which((!is.na(df[[kd_name]]) & !is.na(df[[b_name]]) & df[[kd_name]]==df[[b_name]] | is.na(df[[kd_name]]) & is.na(df[[b_name]])) == F)
      # if all conflicts have a correct value then reolace empty correct values with unconflicted values
      if(all(!is.na(df[[cor_name]][cons]))) {
        df[[i]] <- df[[cor_name]]
        uncon <- which(is.na(df[[i]]))
        df[[i]][uncon] <- df[[kd_name]][uncon]
        df <- df[, -which(colnames(df) %in% c(kd_name, b_name, cor_name))]
      } else {
        x[[i]] <- cons
      }
    }
    if(all(is.na(df[[cor_name]]))){
      df[[cor_name]] <- NULL
    }
  }
  df <- df[ , gtools::mixedorder(colnames(df))]
  return(df)
  if(any(duplicated(colnames(df)))) stop("columns duplicated")
}

bw_cons <- resolve_conflicts(bw, ".bw")
rr_cons <- resolve_conflicts(rr, ".rr")
mg_cons <- resolve_conflicts(mg, ".mg")

write.csv(kd_bw, "outputs/kd-bw-resolved.csv", row.names = F, fileEncoding = "UTF-8", na = "")
write.csv(kd_mg, "outputs/kd-mg-resolved.csv", row.names = F, fileEncoding = "UTF-8", na = "")
write.csv(kd_rr, "outputs/kd-rr-resolved.csv", row.names = F, fileEncoding = "UTF-8", na = "")