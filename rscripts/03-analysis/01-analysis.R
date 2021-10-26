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

save_strobe_div <- function(){
  # extract strobe items that have sub divisions (created by dividing up double questions) >
  # these should only be questions with roman numerals and/or are design specific >
  # all these questions have a "_" in them
  strobe_div <- strobe_cols_names[grepl("_", strobe_cols_names) == T] %>%
    # this will retrieve one strobe item which has no subdivisions > 
    # 14starredc_coh has a _ in because is design specific but that item is only relevant to cohort so is an item in itself
    # remove this non-div item
    .[-grep("s14starredc_coh", .)]
  
  # check no duplicate items
  if(sum(duplicated(strobe_div)) != 0) stop("duplicates in strobe_div")
  
  # extract strobe items without sub divisions
  strobe_comp <- strobe_cols_names[!strobe_cols_names %in% strobe_div]
  
  if(length(strobe_div) + length(strobe_comp) != length(strobe_cols_names)){
    stop("number of strobe items with divisions plus those without don't equal number of strobe cols in s_df_tri")
  }
  return(strobe_div)
}

strobe_div <- save_strobe_div()

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
recode_strobe <- function(){
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

s_df <- recode_strobe()


#######################
# recode strobe into ternary ####
########################

recode_ternary <- function(){
  
  # recode s_df into numerical binary - yes or not yes
  s_df_tri <- s_df
  
  # convert all cols into character variables
  s_df_tri[] <- lapply(s_df, as.character)
  
  # get ids of all strobe cols
  id <- grep(strobe_pattern, colnames(s_df_tri)) 
  
  # recode strobe options into yes and no
  na_pre <- sum(is.na(s_df_tri))
  
  for(i in id){
    if(is.character(s_df_tri[, i]) == F) stop(i, "isn't a character")
    s_df_tri[, i] <-  gsub("^Yes$|^PartiallyExternal$", "1", s_df_tri[, i]) %>%
      gsub("^Partially$", "0.5", .) %>%
      gsub("^No$|^Unsure$", "0", .) %>%
      gsub("na", NA, .) %>%
      as.numeric(.)
    if(is.numeric(s_df_tri[, i]) == F) stop(i, "not numeric")
  }
  
  #if(sum(is.na(s_df_tri)) != na_pre) stop("recoding changed number of NAs")
  return(s_df_tri)
}

s_df_tri <- recode_ternary()

#######################
# calculate strobe scores for ternary  ####
######################## 


sum_items_tri <- function(){
  #check strobe cols
  if(!all(strobe_cols_names %in% colnames(s_df_tri))) stop("some strobe cols not in s_df_tri")

  # vector of numbers in strobe col names
  strobe_div_items  <- strobe_div %>%
    # remove everything after first "_"
    gsub("_.*","",.) %>%
    # add extra _ to easily remove __sum later
    paste(., "_", sep = "")
  
  # all strobe_div_items should be multiples of the elements in strobe_div >
  # because they should all be multiple sub division of of strobe_div elements
  # check all strobe_div_items have duplicates
  if(!all(strobe_div_items %in% strobe_div_items[duplicated(strobe_div_items)])){
    stop("some div items not sub-divisions")
  }
  
  pre <- ncol(s_df_tri)
  
  for(i in strobe_div_items){
    # save dataframe of variables for same strobe item so can sum rows
    x <- s_df_tri[,grep(i, colnames(s_df_tri))]
    # create sum col name
    sum_name <- paste(i, "sum", sep = "_")
    # sum rows in x dataframe 
    sum_var <- rowSums(x, na.rm = T) / rowSums(!is.na(x))
    # check no rows dropped during summing
    if(length(sum_var) != nrow(df)) stop("missing some assessments")
    # force into binary by rounding down any less than 1
    sum_var[sum_var >0 & sum_var < 1] <- 0.5
    # add sum to df under sum_name
    s_df_tri[[sum_name]] <- sum_var
  }
  
  
  if(ncol(s_df_tri) != pre + length(unique(strobe_div_items)))
    stop("cols don't equal number of n cols before function plus number of sub-divided strobe items")
  
  return(s_df_tri)
}

s_df_tri <- sum_items_tri()

#######################
# recode strobe into binary ####
########################

recode_binary <- function(){
  
  # recode s_df into numerical binary - yes or not yes
  s_df_bin <- s_df
  
  # convert all cols into character variables
  s_df_bin[] <- lapply(s_df, as.character)
  
  # get ids of all strobe cols
  id <- grep(strobe_pattern, colnames(s_df_bin)) 
  
  # recode strobe options into yes and no
  na_pre <- sum(is.na(s_df_bin))
  
  for(i in id){
    if(is.character(s_df_bin[, i]) == F) stop(i, "isn't a character")
    s_df_bin[, i] <-  gsub("^Yes$", "1", s_df_bin[, i]) %>%
      gsub("^No$|^Partially$|^Unsure$|^PartiallyExternal$", "0", .) %>%
      gsub("na", NA, .) %>%
      as.numeric(.)
    if(is.numeric(s_df_bin[, i]) == F) stop(i, "not numeric")
  }
  
  #if(sum(is.na(s_df_bin)) != na_pre) stop("recoding changed number of NAs")
  return(s_df_bin)
}

s_df_bin <- recode_binary()


#######################
# calculate strobe scores for binary  ####
######################## 


sum_items_bin <- function(){
  #check strobe cols
  if(!all(strobe_cols_names %in% colnames(s_df_bin))) stop("some strobe cols not in s_df_bin")
  
  # vector of numbers in strobe col names
  strobe_div_items  <- strobe_div %>%
    # remove everything after first "_"
    gsub("_.*","",.) %>%
    # add extra _ to easily remove __sum later
    paste(., "_", sep = "")
  
  # all strobe_div_items should be multiples of the elements in strobe_div >
  # because they should all be multiple sub division of of strobe_div elements
  # check all strobe_div_items have duplicates
  if(!all(strobe_div_items %in% strobe_div_items[duplicated(strobe_div_items)])){
    stop("some div items not sub-divisions")
  }
  
  pre <- ncol(s_df_bin)
  
  for(i in strobe_div_items){
    # save dataframe of variables for same strobe item so can sum rows
    x <- s_df_bin[,grep(i, colnames(s_df_bin))]
    # create sum col name
    sum_name <- paste(i, "sum", sep = "_")
    # sum rows in x dataframe 
    sum_var <- rowSums(x, na.rm = T) / rowSums(!is.na(x))
    # check no rows dropped during summing
    if(length(sum_var) != nrow(df)) stop("missing some assessments")
    # force into binary by rounding down any less than 1
    sum_var[sum_var < 1] <- 0
    # add sum to df under sum_name
    s_df_bin[[sum_name]] <- sum_var
  }
  
  
  if(ncol(s_df_bin) != pre + length(unique(strobe_div_items)))
    stop("cols don't equal number of n cols before function plus number of sub-divided strobe items")
  
  return(s_df_bin)
}
s_df_bin <- sum_items_bin()

#####################
# create bar chart data ###
#######################

create_bar_data <- function(df){
  # select composite strobe items and strobe items without subdivisions
  bar_data <- df[, -which(colnames(df) %in% c("article_id", strobe_div))]
  
  # remove NaN values
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  bar_data[is.nan(bar_data)] <- NA
  
  # clean col names
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

bar_data <- create_bar_data(s_df_tri)

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
  labels$variable <- gsub("_i.*|_v.*|starred|s|_cc|_cs", "", labels$variable)
  labels$variable[!grepl("14c_coh", labels$variable)]  <- gsub("_coh", "", labels$variable[!grepl("14c_coh", labels$variable)])
  
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
levels(bar_data$response) <- c("No", "Partially", "Yes", "Missing")

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

####################
# check bar chart ####
###################

check_bar_data <- function(){
  check <- s_df
  
  # recode all strobe cols in check
  for(i in strobe_cols_names){
    # set all cols to factor
    check[[i]] <- as.factor(check[[i]])
    #recode all factor levels
    levels(check[[i]]) <- list(Yes = "Yes", Other = c("PartiallyExternal", "Partially", "No", "Unsure"))
  }
  
  check_freq <- NULL
  
  # narrow all_freq
  for(i in colnames(check)[colnames(check) != "article_id"]){
    x <- data.frame(strobe = i, 
                    percent = prop.table(table(check[[i]], useNA = "no")), 
                    count = table(check[[i]], useNA = "no"),
                    applic = sum(!is.na(check[[i]])))
    check_freq <- rbind(check_freq, x)
    row.names(check_freq) <- c()
  }
  
  # check all strobe items have same order of yes and other so can remove counts and percents safely for "Other" values
  if(identical(check_freq$percent.Var1, check_freq$count.Var1) ==F ){
    stop("percent & count cols don't contain same order of Yes and Other ")
  }
  
  # check all strobe items have percent for Yes and Other values
  if(identical(unique(check_freq$strobe[check_freq$percent.Var1 == "Yes"]),
               unique(check_freq$strobe[check_freq$percent.Var1 == "Other"])) ==F){
    stop("not all strobe items have yes and other percents")
  } else {
    # if passed checks remove Other values
    test <- check_freq[check_freq$percent.Var1 != "Other",  ]
    if(identical(test$strobe, unique(check_freq$strobe)) == T){
      # if stobe col identical after removing other values
      check_freq <- test
      # remove test
      rm(test)
    } else{
      stop("strobe col different after removing Other values")
    }
  }
  
  if(identical(check_freq$percent.Var1, check_freq$count.Var1)){
    colnames(check_freq)[colnames(check_freq) == "percent.Freq"] <- "percent_yes"
    colnames(check_freq)[which(colnames(check_freq) == "count.Freq")] <- "count_yes"
    check_freq$percent.Var1 <- NULL
    check_freq$count.Var1 <- NULL
  }else{
    stop("percent.Var1 and count.Var1 not identical")
  }
  
  # create percent col of percent yes out of number applicable to ensure percent calculated correctly
  check_freq$test <- check_freq$count_yes / check_freq$applic
  
  if(identical(check_freq$test, check_freq$percent_yes)){
    # remove test col if identical
    check_freq$test <- NULL
  } else {
    # stop if not
    stop("test percent col different to percent col")
  }
  
}

check_bar_data()

#######################
# frequencies of strobe ####
######################

create_wide_data <- function(){
  strobe_freq_wide <- NULL
  
  for(i in strobe_cols_names){
    x <- prop.table(table(s_df[[i]], useNA = "always"))
    if(nrow(x) != length(names(x))) stop(i, " does not have 7 values in x")
    x_df <- data.frame(strobe = i,x[1],x[2],x[3],x[4],x[5], x[6], x[7])
    colnames(x_df) <- c("strobe", names(x))
    strobe_freq_wide <- rbind(strobe_freq_wide, x_df)
    row.names(strobe_freq_wide) <- c()
  }
  return(strobe_freq_wide)
}

strobe_freq_wide <- create_wide_data()

create_narrow_data <- function(){
  strobe_freq_narr <- NULL
  
  # narrow strobe_freq
  for(i in strobe_cols_names){
    s_df[[i]] <- as.factor(s_df[[i]])
    levels(s_df[[i]]) <- list(Yes = "Yes", PartiallyExternal = "Partially-External",
                              Partially = "Partially", No = "No", Unsure = "Unsure")
    x <- data.frame(strobe = i,prop.table(table(s_df[[i]], useNA = "always")))
    strobe_freq_narr <- rbind(strobe_freq_narr, x)
    row.names(strobe_freq_narr) <- c()
  }
  return(strobe_freq_narr)
}

strobe_freq_narr <- create_narrow_data()

strobe_freq_narr %>%ggplot(aes(fill=Var1, y=Freq, x=strobe)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  scale_fill_manual(values= c("#009E73","#E69F00",  "#F0E442", "#D55E00", "#0072B2", "#999999")) +
  theme(axis.text.x=element_text(angle=45,hjust=1))

#####################################
# frequencies for dicotomised strobe items ####
######################################

# dichotomise strobe item responses into yes and no

s_bin <- s_df

strobe_bin_freq <- NULL

for(i in strobe_cols_names){
  s_bin[[i]] <- as.factor(s_bin[[i]])
  levels(s_bin[[i]]) <- list("Yes"=c("Partially", "PartiallyExternal", "Yes"), "No"=c("Unsure", "No"))
  x <- data.frame(i, prop.table(table(s_bin[[i]])))
  strobe_bin_freq <- rbind(strobe_bin_freq, x)
}

row.names(strobe_yes) <-c() 

strobe_yes <- strobe_bin_freq[strobe_bin_freq$Var1 == "Yes",]

strobe_yes$Var1 <- NULL

colnames(strobe_yes)[colnames(strobe_yes) == "Freq"] <- "Yes (%)"
colnames(strobe_yes)[colnames(strobe_yes) == "i"] <- "STROBE item"

#######################################
#### add completion scores for items #####
########################################

bar_data_tot <- bar_data

# change bar data to numeric
bar_data_tot[] <- lapply(bar_data_tot, as.numeric) 

# count row sums without NA (returns NA if contains any NAs)

count_na <- function(x) sum(is.na(x))

bar_data_tot <- bar_data_tot %>%
  mutate(sums = rowSums(., na.rm = T),
         count_na = apply(., 1, count_na))
bar_data_tot$applic <- 32 - bar_data_tot$count_na

bar_data_tot$score <- bar_data_tot$sums/ bar_data_tot$applic
