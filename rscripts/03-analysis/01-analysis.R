############
# libraries ####
#############
library(magrittr)
library(ggplot2)
library(tableone)
library(gtools)
library(tidyr)
library(RColorBrewer)
library(dplyr)
library(irr)
library(lpSolve)

######################
# import  ####
######################


# save naming convention used for strobe item variables as a pattern to be used in grep

strobe_pattern <- "^s[[:digit:]]{1,2}"

import_recoded <- function(){
  # most variables are unordered factors (nominal with set responses) so read in with stringsAsFactors = T
  df_orig <- read.csv("outputs/data-extraction-form-clean.csv", encoding = "UTF-8", na.strings = c(""), stringsAsFactors = T)
  df <- df_orig
  for(i in colnames(df)){
    # remove - from "Partially-External" as R doesn't like punctuation in factor levels
    x <- gsub("Partially-External", "PartiallyExternal", df[[i]]) %>%
      # make not applicable lowercase so doesn't clash with missing (NA) notation
      gsub("NA", "na", .) %>%
      # convert back into factor as gsub will have converted it to character
      as.factor(.)
    # check removed
    if(length(which(x == "Partially-External") != 0)) stop("some Partially-External remain in ", i)
    if(length(which(x =="NA") != 0)) stop("some not applicable values remain in ", i)
    df[[i]] <- x
  }
  return(df)
}

df <- import_recoded()

###################
# add presence cols ####
#####################

# add non-strobe cols indicating if non-strobe items present

add_pres <- function(df){
  df$ukb_app_pres <- !is.na(df$ukb_app)
  df$email_pres <- !is.na(df$email)
  df$keywords_pres <- !is.na(df$keywords)
  df$coi_pres <- !is.na(df$coi)
  # drop cols just created presence cols for as no longer needed for frequencies
  df <- df[, -which(colnames(df) %in% c("ukb_app", "email", "keywords", "coi"))]
  return(df)
}

df <- add_pres(df)

############
# drop non-strobe cols ####
#############

drop_cols <- function(){
  # remove predict col as df only contains non-prediction papers
  if(!any(df$predict == "Yes", na.rm = T)){
    df$predict <- NULL
  }else{
    stop("predict col not empty")
  }
  # drop access_article as this should be all yes
  if(all(df$access_article == "Yes")){
    df$access_article <- NULL
  } else {
    stop("access_article not just yes")
  }
  # save names of cols not needed 
  void <- grep("title|comments\\.", colnames(df), value = T)
  if(!all(void %in% colnames(df))) stop("df doesn't have ", void[!void %in% colnames(df)])
  # drop not needed cols by name
  df <- df[, -which(colnames(df) %in% void)]
}

df <- drop_cols()

# vector of original column names for checking later

if(sum(duplicated(colnames(df))) == 0){
  df_cols <- colnames(df)
} else {
  stop("duplicated col names")
}

########################
# save vector of strobe cols ####
########################

strobe_cols_names <- grep(strobe_pattern, colnames(df), value =T)

strobe_stem <- gsub("_.*", "", strobe_cols_names) %>%
  unique()

###################################
# split into strobe and non-strobe ####
#################################


# create funciton to check strobe items named & positioned correctly
check_strobe <- function(df){
  # find all strobe cols
  strobe_cols_positions <- which(colnames(df) %in% strobe_cols_names)
  # vector of numbers in strobe col names
  strobe_nums <- strobe_cols_names %>%
    # remove all punctuation and letters
    gsub("[[:punct:]]|[[:alpha:]]", "", .)%>%
    # remove duplicates
    unique()
  
  # check all strobe_nums are in 1-22
  if(!identical(unique(gsub("[[:alpha:]]", "", strobe_nums)), as.character(c(1:22)))){
    df <- NULL
    warning("strobe_nums don't equal 1-22")
  } 
  if(sum(duplicated(strobe_nums)) != 0){
    df <- NULL
    warning("duplicate strobe_nums")
  } 
  # check positions of strobe items are sequential (i.e. all positions have a diff of 1 between them)
  if(!all(abs(diff(strobe_cols_positions)) == 1)){
    df <- NULL
    warning("strobe items are not in sequential in df")
  } 
  return(df)
}

df <- check_strobe(df)

# if passes strobe check, separate df into strobe items and non-strobe items

s_df <- df[, which(colnames(df) %in% c("article_id", strobe_cols_names))]
not_s_df <- df[, -which(colnames(df) %in% strobe_cols_names)]

# check total ncol of s_df and not_s_df is same as original df +1 because article_id col is in both
if(ncol(s_df) + ncol(not_s_df) != ncol(df) +1) stop("wrong number of cols in strobe or not_strobe dfs")

###################
# frequencies non-strobe ####
##################

# save version of not strobe df without qualtrics metadata variables to create table 1

create_table1_df <- function(){
  # save pattern to find qualtrics variables
  qual <- c("distribution_channel", "duration_in_seconds", "end_date", "finished", "progress","recorded_date", "response_id", "response_type", "start_date", "user_language")
  qual_pat <- paste0("^", qual, ".", collapse = "|")
  
  # find qualtrics columns in df
  qual_cols <- grep(qual_pat, colnames(not_s_df))
  
  # check there are 4 of each qual column
  if(length(qual_cols) != 4* length(qual)) stop("some qualtrics cols missing")
  
  table1 <- not_s_df[, -qual_cols]
  
  # drop article_id col as don't need freqs of these
  table1 <- table1[, -which(colnames(table1) %in% "article_id")]
  
  #convert all non-strobe cols into characters as easier than setting multiple different factor levels
  for(i in colnames(table1)){
    table1[[i]] <- as.character(table1[[i]])
  }
  
  # ensure UK and USA country values are capitalised as frequency is case-sensitive
  uk_usa <- grep("^uk$|^usa$", table1$country, ignore.case = T)
  table1$country[uk_usa] <- toupper(table1$country[uk_usa])
  return(table1)
}


table1 <- create_table1_df()

CreateTableOne(data = table1, includeNA = F)%>%
  print(., noSpaces = T, showAllLevels = T) %>%
  # export
  write.csv(., "outputs/table1_all_levels.csv")


CreateTableOne(data = table1, includeNA =F )%>%
  print(., noSpaces = T) %>%
  # export
  write.csv(., "outputs/table1_no_levels.csv")

#########################
# recode strobe factors ####
########################

# recode strobe variables into factors so can analyse later
factorise_strobe <- function(){
  # check strobe cols correct
  if(!all(strobe_cols_names %in% colnames(s_df))) stop("some strobe cols not in s_df")
  # save s_df before recoding factors
  pre_recode <- s_df
  # recode all strobe cols in s_df
  for(i in strobe_cols_names){
    # set all cols to factor
    s_df[[i]] <- as.factor(s_df[[i]])
    #recode all factor levels
    levels(s_df[[i]]) <- list(Yes = "Yes", PartiallyExternal = "PartiallyExternal", Partially = "Partially", No = "No", Unsure = "Unsure", na = "na")
  }
  
  # check recode successful
  
  x <- c()
  
  for(i in strobe_cols_names){
    y <- all.equal(as.character(s_df[[i]]), as.character(pre_recode[[i]]))
    x <- c(x, y)
  }
  # check all cols identical as they were before recoding
  if(sum(x) != length(x) | length(x) == 0) stop("factor recoded cols in s_df are different to before recoding")
  # check factors created
  classes <- sapply(s_df, class)
  if(!any(classes == "factor")) stop("no factors created")
  return(s_df)
}

s_df <- factorise_strobe()


#######################
# divide strobe into ternary ####
########################

recode_strobe <- function(style = c("binary", "ternary")){
  # check style arg correct
  if(!style %in% c("binary", "ternary")) stop("style arg incorrect")
  
  # recode s_df
  x <- s_df
  
  # convert all cols into character variables
  x[] <- lapply(x, as.character)
  
  # get ids of all strobe cols
  id <- grep(strobe_pattern, colnames(x)) 
  
  # check all are characters
  for(i in id){
    if(is.character(x[, i]) == F) stop(i, "isn't a character")
  }
  # recode into binary
  if(style == "binary"){
    for(i in id){
      x[, i] <-  gsub("^Yes$", "1", x[, i]) %>%
        gsub("^No$|^Partially$|^Unsure$|^PartiallyExternal$", "0", .) %>%
        gsub("na", NA, .) %>%
        as.numeric(.)
      if(is.numeric(x[, i]) == F) stop(i, "not numeric")
    }
  }
  # recode into ternary
  if(style == "ternary") {
    for(i in id){
      x[, i] <-  gsub("^Yes$|^PartiallyExternal$", "1", x[, i]) %>%
        gsub("^Partially$", "0.5", .) %>%
        gsub("^No$|^Unsure$", "0", .) %>%
        gsub("na", NA, .) %>%
        as.numeric(.)
      if(is.numeric(x[, i]) == F) stop(i, "not numeric")
    }
  }
  #check strobe cols
  if(!all(strobe_cols_names %in% colnames(x))) stop("some strobe cols not in s_df_tri")
  
  return(x)
}

s_df_bin <- recode_strobe(style = "binary")
s_df_tri <- recode_strobe(style = "ternary")

#######################
# calculate strobe scores for ternary  ####
######################## 


sum_items <- function(df, style = c("binary", "ternary")){
  # check style arg correct
  if(!style %in% c("binary", "ternary")) stop("style arg incorrect")
  
  pre <- ncol(df)
  for(i in strobe_stem){
    # save dataframe of variables for same strobe item so can sum rows
    x <- df[,grep(i, colnames(df))]
    # create sum col name
    sum_name <- paste(i, "sum", sep = "_")
    # sum rows in x if it is a dataframe 
    if(!is.numeric(x)){
      sum_var <- rowSums(x, na.rm = T) / rowSums(!is.na(x))
    } else {
      # if not dataframe then no need to sum rows because only 1 col
      sum_var <- x
    }
    # check no rows dropped during summing
    if(length(sum_var) != nrow(df)) stop("missing some assessments")
    # make scores binary or ternary 
    if(style == "binary"){
      # force into binary by rounding down any less than 1 to indicate all yes/not yes
      sum_var[sum_var < 1] <- 0
    }
    if(style == "ternary"){
      # force into ternary by rounding any not 0 or 1 numbers to 0.5 to indicate partialness
      sum_var[sum_var >0 & sum_var < 1] <- 0.5
    }
    # add sum to df under sum_name
    df[[sum_name]] <- sum_var
  }
  
  
  if(ncol(df) != pre + length(unique(strobe_stem)))
    stop("cols don't equal number of n cols before function plus number strobe items")
  
  # drop all columns that are not summs composite strobe items and strobe items without subdivisions
  df <- df[, which(colnames(df) %in% c("article_id", grep("_sum$", colnames(df), value = T)))]
  
  # remove NaN values because rowSums will have introduced them
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  df[is.nan(df)] <- NA
  
  # remove _sum from all colnames so matches strobe stems
  colnames(df) <- gsub("_.*", "", colnames(df))
  
  return(df)
}

tri_sum <- sum_items(s_df_tri, style = "ternary")
bin_sum <- sum_items(s_df_bin, style = "binary")



#####################
# create bar chart data ###
#######################

create_bar_data <- function(df){
  bar_data <- df[, which(colnames(df) %in% strobe_stem)]
  if(ncol(bar_data) != length(strobe_stem)) stop("some strobe items missing")
  # clean col names so will match names of strobe items in strobe guidance
  colnames(bar_data) <- colnames(bar_data) %>%
    gsub("^s|__sum|starred", "", .) 
  
  # change all to character to create table one
  bar_data[] <- lapply(bar_data, as.character)
  
  bar_data <- tidyr::pivot_longer(bar_data, colnames(bar_data), names_to = "strobe_item", values_to = "value") 
  x <- table(bar_data$strobe_item, bar_data$value, useNA = "always")
  y <- prop.table(table(bar_data$strobe_item, bar_data$value), 1) %>%
    round(., 2)
  # convert to dataframe
  x <- as.data.frame.matrix(x)
  y <- as.data.frame.matrix(y)
  
  # row names will contain the x values given to table(x, y), move these to a column >
  # remove the X added
  x$strobe_item <- gsub("^X", "", row.names(x))
  row.names(x) <- NULL
  
  y$strobe_item <- gsub("^X", "", row.names(y))
  row.names(y) <- NULL
  
  # useNA in x means there will be an blank column name for the NA values, rename this as won't pivot if any missing
  colnames(x)[is.na(colnames(x))] <- "na"
  # useNA also creates an "NA" row, remove this
  x <- x[-grep("NA", x$strobe_item), ]
  
  # save response cols so can pivot them
  x_cols <- colnames(x)[-which(colnames(x) %in% "strobe_item")]
  
  # create response column
  x <- pivot_longer(x, all_of(x_cols), names_to = "response", values_to = "count")
  
  # save response cols so can pivot them
  y_cols <- colnames(y)[-which(colnames(y) %in% "strobe_item")]
  
  # create response column
  y <- pivot_longer(y, all_of(y_cols), names_to = "response", values_to = "percent")
  
  # check strobe items identical so will merge properly
  if(!identical(unique(x$strobe_item), unique(y$strobe_item))) stop("x and y strobe items different")
  
  bar_data <- full_join(x, y, by = c("strobe_item", "response"), suffix = c(".count", ".percent"))
  
  # reorder
  bar_data <- bar_data[mixedorder(as.character(bar_data$strobe_item)),]
  
  return(bar_data)
}

bar_data <- create_bar_data(tri_sum)

#############################
# export data to create bar chart labels ####
##########################

create_bar_labels_csv <- function(){
  strobe_qs <- read.csv("outputs/extraction_dict.csv", stringsAsFactors = F, encoding = "UTF-8") %>%
    .[, colnames(.) %in% c("question", "variable")] %>%
    # select those that are strobe items 
    .[grep("s[[:digit:]]{1,2}", .$variable), ] %>%
    .[-grep("\\_ev|\\_star", .$variable), ]
  
  strobe_qs$question <- sub("[^0-9]*", "", strobe_qs$question)
  strobe_qs$strobe_item <- sub("\\_.*", "", strobe_qs$variable)
  write.csv(strobe_qs, "outputs/bar_labels.csv", row.names = F, fileEncoding = "UTF-8")
}

create_bar_labels_csv()

##############
## MANUAL ####
############

# MANUALLY INSTRUCTIONS
# added text describing the strobe item the bar_labels.csv

###########################
# import labels and clean ####
########################


create_labels <- function(){
  
  # import general as these contain labels at the strobe item level (not design specific)
  labels <- read.csv("data/bar_labels.csv", encoding = "UTF-8", stringsAsFactors = F, na.strings = "") %>%
    .[, colnames(.) %in% c("variable", "item_level_label")]
  
  # clean variable names so matches bar_chart_freq colnames (i.e. no subdivisions and no design specific questions) >
  # easiest to do this by cleaning non-cohort specific quesitons first then cohort excluding 14c_coh since this contains no roman numerials and is not a sub division
  labels$variable <- gsub("_.*|starred|s", "", labels$variable)

  # remove duplicated rows now variables cleaned
  labels <- labels[!duplicated(labels),]
  # sort
  labels <- labels[mixedorder(as.character(labels$variable)),]
  
  # select applic col so can merge in
  applic_df <- bar_data[bar_data$response == "na", which(colnames(bar_data) %in% c("strobe_item", "count"))]
  
  # add applic col to labels rows only so can remove empty labels that are empty and wont appear in bar chart
  labels <- left_join(labels, applic_df, by = c("variable" = "strobe_item"))
  
  # add number of applicable items to labels so can see how many items were relevant in bar
  if(identical(labels$variable, 
               as.character(unique(bar_data$strobe_item)))){
    # paste variable together with description , separate by space so can gsub later
    labels$x_labels <- paste(labels$variable, labels$item_level_label, sep = " ") %>%
      # order
      .[gtools::mixedorder(.)]%>%
      # wrap
      stringr::str_wrap(., width = 55) %>%
      # add n = applicable on a newline
      paste(., " (n=", nrow(df) - labels$count, ")", sep = "")
  } else {
    stop("not identical")
  }
  return(labels)
}

labels <- create_labels()


#######################
## bar chart for all ####
##################
strobe_levels <-  bar_data$strobe_item[!duplicated(bar_data$strobe_item)]
# set levels as mixed order so order preserved in ggplot
bar_data$strobe_item<- factor(bar_data$strobe_item, levels = 
                                strobe_levels[gtools::mixedorder(strobe_levels)])

bar_data$response <- as.factor(bar_data$response)
levels(bar_data$response) <- c("No", "Partially", "Yes", "Not Applicable")

# check labels identical to strobe item numbers in labels$x_labels (gsub at the space) to ensure labels will map to chart
if(!identical(gsub(" .*", "", labels$x_labels), as.character(strobe_levels))) stop("labels won't map")

png("Rplot9.png", width = 1300, height = 1500)

# strobe item on x axis and plot % yes
bar_data %>%ggplot(aes(x=strobe_item, y=count, fill = response)) + 
  # blue fill
  geom_bar(stat="identity") +
  theme_classic() +
  theme(text = element_text(size=30))+
  scale_x_discrete(labels= labels$x_labels) +
  scale_y_continuous(breaks = seq(0, nrow(df), by = 10)) + 
  coord_flip() +
  xlab("STROBE Item") +
  ylab("Count") +
  ggtitle("Figure 1. STROBE completion")+
  scale_fill_brewer(palette = "Spectral") +
  #geom_text(aes(label = count), vjust = 0, size = 8) +
  theme(plot.title = element_text(size = 22)) 
dev.off()


#####################
# calculate irr for NA and applicable ####
################################

inter <- read.csv("outputs/data-extraction-form-conflicted.csv", na.strings = "")

create_kappa_df <- function(df){
  
  # save stems of variables that were not double coded (and therefore not conflicted)
  # kd alone extracted some variables so save all these variables single/unique variables
  single <- c("email","country","ukb_app","keywords","coi", "article_id") %>%
    #  qualtrics metadata, comments & title substring vars are unique to each coder so stems duplicated but not colnames >
    c(., "distribution_channel", "duration_in_seconds", "end_date", "finished", "progress", "recorded_date", "response_id", "response_type", "start_date", "user_language") %>%
    # remove those unique to each coder
    c(., "comments", "title_clean", "title_sub", "title")
  
  
  stem <- grep("\\.", colnames(df), value = T) %>%
    gsub("\\..*", "", .) %>%
    unique() %>%
    # remove single coded items since these can't be conflicted
    .[!. %in% single] %>%
    # remove predict because we already resolved these conflicts
    .[!. %in% "predict"] %>%
    # remove other variables that are not discrete
    .[!. %in% c("other_guidelines", "reg_id")] %>%
    # add '.' onto end so will match full word
    paste0(., "\\.")
  
  kappa <- c()
  z <- c()
  p_value <- c()
  variable <- c()
  
  for(i in stem){
    x <- df[, grep(i, colnames(df))]
    y <- kappa2(x, weight = "unweighted")
    variable <- c(variable, i)
    kappa <- c(kappa, y$value)
    z <- c(z, y$statistic)
    p_value <- c(p_value, y$p.value)
  }
  
  irr_df <- data.frame(variable = variable, kappa= kappa, z= z, p_value = p_value)
  
}


# divide df into into NA and applicable

divide_na <- function(){
  for(i in colnames(inter)){
    x <- inter[[i]]
    x[x != "NA"] <- "A"
    inter[[i]] <- x
  }
  return(inter)
}

inter_na <- divide_na()
na_kappa <- create_kappa_df(inter_na)

#######################
# calculate strobe scores for ternary for inter  ####
######################## 

# split into kd and coder2 so can sum strobe items for each then rejoin
kd <- inter[, c("article_id", grep("\\.kd", colnames(inter), value = T))]
coder2 <- inter[, c("article_id", grep("\\.coder2", colnames(inter), value = T))]

sum_items <- function(df, coder_suffix){
  # set "NA" and "Unsure" as missing so calculations only apply to reporting judgements
  df[df == "NA" | df == "Unsure"] <- NA
  strobe_cols_names <- grep("^s[[:digit:]]{1,2}", colnames(df), value =T)
  
  # remove everything after _ or suffix
  strobe_stem <- gsub("_.*|\\..*", "", strobe_cols_names) %>%
    unique()
  
  pre <- ncol(df)
  
  for(i in strobe_stem){
    
    # save dataframe of variables for same strobe item so can sum rows
    x <- df[,grep(i, colnames(df))]
    # create sum col name
    sum_name <- paste0(i, "_sum", coder_suffix)
    # create empty sum_var to add values to in for loop
    sum_var <- c()
    # sum rows in x if it is a dataframe 
    if(!is.character(x)){
      for(j in 1:nrow(x)){
        if(all(x[j, ] %in% c("Yes", "Partially-External"), na.rm = T)) {
          sum_var[j] <- "Yes"
        } else {
          if(!all(x[j, ] %in% c("Yes", "Partially-External"), na.rm = T) & !all(x[j, ] == "No", na.rm = T)) {
            sum_var[j] <- "Partially"
          } else {
            if(all(x[j, ] == "No", na.rm = T)) {
              sum_var[j] <- "No"
            } else {
              stop(j, " in ", i, " doesn't contain just yes, pe, partially or no")
            }
          }
        }
      }
    } else {
      # if not dataframe then no need to sum rows because only 1 col
      sum_var <- x
    }
    # check no rows dropped during summing
    if(length(sum_var) != nrow(df)) stop("missing some assessments")
    # add sum to df under sum_name
    df[[sum_name]] <- sum_var
  }
  if(ncol(df) != pre + length(unique(strobe_stem)))
    stop("cols don't equal number of n cols before function plus number strobe items")
  
  # drop all cols that are not strobe sums & article id so can merge on id
  df <- df[, which(colnames(df) %in% c("article_id", grep("_sum", colnames(df), value =T)))]
  
  return(df)
}

kd <- sum_items(kd, ".kd")
coder2 <- sum_items(coder2, ".coder2")

inter_item <- full_join(kd, coder2, by = "article_id") %>%
  # reorder
  .[ , gtools::mixedorder(colnames(.))] %>%
  # remove article_id
  .[, -which(colnames(.) %in% "article_id")]

kappa_item <- create_kappa_df(inter_item)
