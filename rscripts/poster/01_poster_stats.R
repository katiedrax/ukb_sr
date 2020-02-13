############
# libraries ####
#############
library(dplyr)
library(ggplot2)
library(tableone)
library(gtools)

######################
# import  ####
######################

# most variables are unordered factors (nominal with set responses) so read in with stringsAsFactors = T
df_orig <- read.csv("outputs/clean_poster.csv", encoding = "UTF-8")

#########
# recode ####
########

# replace remove - from "Partially-External", R doesn't like punctuation in factor levels

df <- data.frame(lapply(df_orig, function(x) {
  gsub("Partially-External", "PartiallyExternal", x)
  }))

# check removed
if(length(which(df == "Partially-External") != 0)) stop("some Partially-External remain")

###################
# add presence cols ####
#####################

# replace non-strobe cols indicating if non-strobe items present

df$ukb_app_pres <- !is.na(df$ukb_app)
df$email_pres <- !is.na(df$email)
df$keywords_pres <- !is.na(df$keywords)
df$coi_pres <- !is.na(df$coi)

############
# drop non-strobe cols ####
#############

# drop cols just created presence cols for

df <- select(df, -c("ukb_app", "email", "keywords", "coi"))

# remove predict col if empty
if(sum(is.na(df$predict)) == nrow(df)){
  df$predict <- NULL
}else{
  stop("predict col not empty")
}


# drop access_article

if(all(df$access_article == "Yes")){
  df$access_article <- NULL
} else {
  stop("access_article not just yes")
}

# drop not needed cols by name

df <- select(df, -c("title", "comments", "ukb_credit_ev", "journal", "strobe_ev"))


# vector of original column names for checking later

if(sum(duplicated(colnames(df))) == 0){
  df_cols <- colnames(df)
} else {
  stop("duplicated col names")
}


# find empty cols
empty <- colnames(df[colSums(!is.na(df)) == 0]) %>%
  # select those that aren't strobe items
  .[grepl("^X[[:digit:]]{1,2}", .) == F]

# drop empty cols

df <- select(df, -c(empty))

###################################
# split into strobe and non-strobe ####
#################################

# find all strobe cols
strobe_cols <- grep("^X[[:digit:]]{1,2}", colnames(df))

# check number of strobe cols is 99
if(length(!is.na(strobe_cols)) != 99) stop("wrong num of strobe_cols")

if(all(abs(diff(strobe_cols))) == T){
  # if strobe_cols are sequential separate df into strobe items and non-strobe items
  s_df <- select(df, "article_id", strobe_cols)
  not_s_df <- select(df, -strobe_cols)
}else {
  stop("strobe col positions not sequential")
}

# check total ncol of s_df and not_s_df is same as original df +1 because article_id col is in both
if(ncol(s_df) + ncol(not_s_df) != ncol(df) +1) stop("wrong number of cols in strobe or not_strobe dfs")


#####################################
# find not applicable strobe items #####
####################################

# save number of s_df cols before remove NA so can check later

all_s_df_cols <- ncol(s_df)

# creat empty vector for next for loop
na_cols <- c()

# save strobe col names that were not applicable for any articles
for(i in colnames(s_df)){
  if(all(is.na(s_df[[i]]))){
    # if s_df$i contains only NA save i to the na_cols vector
    na_cols <- c(na_cols, i)
  }
}

# remove empty cols by name
#s_df <- s_df[, !colnames(s_df)%in%na_cols]

# check cols safely removed
#if(length(na_cols) +length(colnames(s_df)) != all_s_df_cols) stop("some cols unaccounted for after removing NA cols")

###############################
# subset star strobe items  ####
##############################

# separate star into one df even though they are design specific >
# this is because the advice is the same (show information separately for 2 groups)>
# it's just that the 'groups' differ for the designs >
# cases & controls for cc and exposed & unexposed for coh & cs designs

if(identical(grep("star_", colnames(s_df)), grep("\\_star\\_", colnames(s_df)))){
  star <- grep("star_", colnames(s_df))
  # subset star cols
  s_star_df <- s_df[,star]
  # remove star cols from s_df
  s_df <- s_df[, -star]
} else {
  stop("star not identical")
}

if(all(c(unique(colnames(s_df)), unique(colnames(s_star_df)), unique(colnames(not_s_df))) %in% colnames(df)) == F){
  stop("df subsets have diff colnames to df")
}

#########################
# recode strobe factors ####
########################

# find all strobe cols
strobe_cols <- grep("^X[[:digit:]]{1,2}", colnames(s_df), value =T)

# save s_df before recoding factors
pre_recode <- s_df

# recode all strobe cols in s_df
for(i in strobe_cols){
  # set all cols to factor
  s_df[[i]] <- as.factor(s_df[[i]])
  #recode all factor levels
  levels(s_df[[i]]) <- list(Yes = "Yes", PartiallyExternal = "PartiallyExternal", Partially = "Partially", No = "No", Unsure = "Unsure")
}

# check recode successful

x <- NULL

for(i in strobe_cols){
  x <- c(x, all.equal(as.character(s_df[[i]]), 
                     as.character(pre_recode[[i]])))
}

if(sum(x) == length(x)){
  rm(x)
  rm(pre_recode)
} else {
  stop("factor recoded cols in s_df are different to before recoding")
}


###################
# frequencies non-strobe ####
##################

for(i in colnames(df)[colnames(df) %in% colnames(not_s_df)]) {
  df[,i] <- as.character(df[,i])
}

exc <- c("article_id", grep("^X[[:digit:]]{1,2}", colnames(df), value =T))


cats <- df[, sapply(df, class) == 'character'] %>%
  colnames(.)%>%
  .[!(. %in% exc)]

fact <- df[, sapply(df, class) == 'factor']%>%
  colnames(.)


tab1_levs <- CreateTableOne(vars = colnames(df)[(colnames(df) %in% exc) == F], data = df, factorVars = cats, includeNA = F)%>%
  print(., noSpaces = T, showAllLevels = T)
tab1_collaps <- CreateTableOne(vars = colnames(df)[(colnames(df) %in% exc) == F], data = df, factorVars = cats, includeNA =F )%>%
  print(., noSpaces = T)

write.csv(tab1_levs, "outputs/table1_all_levels.csv")
write.csv(tab1_collaps, "outputs/table1_no_levels.csv")


#######################
# strobe scores ####
########################

# recode s_df into numerical binary - yes or not yes

s_df_bin <- s_df

s_df_bin[] <- lapply(s_df, as.character)

# get ids of all strobe cols
id <- grep("^X[[:digit:]]{1,2}", colnames(s_df_bin)) 

# recode strobe options into yes and no

na_pre <- sum(is.na(s_df_bin))

for(i in id){
  if(is.character(s_df_bin[, i]) == F) stop(i, "isn't a character")
  s_df_bin[, i] <-  gsub("^Yes$", 1, s_df_bin[, i]) %>%
    gsub("^No$|^Partially$|^Unsure$|^PartiallyExternal$", 0, .) %>%
    as.numeric(.)
  if(is.numeric(s_df_bin[, i]) == F) stop(i, "not numeric")
}


if(sum(is.na(s_df_bin)) != na_pre) stop("recoding changed number of NAs")


# get names of all strobe cols
strobe_cols <- grep("^X[[:digit:]]{1,2}", colnames(s_df_bin), value = T) 

# vector of numbers in strobe col names
strobe_nums <- strobe_cols %>%
  # remove all punctuation and letters
  gsub("[[:punct:]]|[[:alpha:]]", "", .)%>%
  # remove duplicates
  unique()

# check all strobe_nums are in 1-22
if(identical(unique(gsub("[[:alpha:]]", "", strobe_nums)), as.character(c(1:22))) == F) stop("strobe_nums don't equal 1-22")
if(sum(duplicated(strobe_nums)) != 0) stop("duplicate strobe_nums")

# extract strobe items that have sub divisions (created by dividing up double questions) >
# these should only be questions with roman numerals in them after a "_" and those that are design specific
strobe_div <- strobe_cols[grepl("_i|_v|coh|cs|cc", strobe_cols) == T] %>%
  # this will retrieve one strobe item which has no subdivisions > 
  # remove this non-div item
  .[-grep("X14starredc", .)]

# check no duplicate items
if(sum(duplicated(strobe_div)) != 0) stop("duplicates in strobe_div")

# extract strobe items without sub divisions
strobe_comp <- strobe_cols[grepl("_i|_v|coh|cs|cc", strobe_cols) == F] %>%
  unique()
# this will fail to retrieve one strobe item which has no subdivisions > 
# add this non-div item
strobe_comp <- c(strobe_comp, "X14starredc")

if(length(strobe_div) + length(strobe_comp) != sum(grepl("^X[[:digit:]]{1,2}", colnames(s_df_bin)))){
  stop("number of strobe items with divisions plus those without don't equal number of strobe cols in s_df_bin")
}

# vector of numbers in strobe col names
strobe_div_items  <- strobe_div %>%
  # remove everything after first "_"
  gsub("\\_.*","",.) %>%
  # add extra _ to easily remove __sum later
  paste(., "_", sep = "")
  

# all strobe_div_items should be multiples of the elements in strobe_div >
# because they should all be multiple sub division of of strobe_div elements
# check all strobe_div_items have duplicates
if(sum(strobe_div_items %in% strobe_div_items[duplicated(strobe_div_items)]) != length(strobe_div_items)){
  stop("some div items not sub-divisions")
}

# BUG this commented section does not work - error = arguments different lengths BUT does work if run twice sequentially - why? No idea!
#a <- NULL
#for(i in strobe_div_items){
  #x <- s_df_bin[,grep(i, colnames(s_df_bin))]
  #sum <- paste(i, "sum", sep = "_")
  #x[[sum]] <- rowSums(x, na.rm = T) / rowSums(!is.na(x))
  #if(nrow(x) != 32) stop("not 32")
  #a <- cbind(a, x)
#}


for(i in strobe_div_items){
  x <- s_df_bin[,grep(i, colnames(s_df_bin))]
  sum <- paste(i, "sum", sep = "_")
  x[[sum]] <- rowSums(x, na.rm = T) / rowSums(!is.na(x))
  if(nrow(x) != 32) stop("not 32")
  x <- select(x, sum)
  x[x < 1] <- 0
  s_df_bin[[sum]] <- x[[sum]]
}

##############
# bar chart data ####
############
# select composite strobe items and strobe items without subdivisions
bar_data <- select(s_df_bin, -c("article_id", strobe_div))

# remove NaN values
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

bar_data[is.nan(bar_data)] <- NA

# clean col names
colnames(bar_data) <- colnames(bar_data) %>%
  gsub("^X|__sum|starred", "", .) 

# change all to character to create table one
bar_data[] <- lapply(bar_data, as.character)

# create table one showing all levels and export
CreateTableOne(vars = colnames(bar_data), data = bar_data,  factorVars = colnames(bar_data), includeNA = F)%>%
  print(., noSpaces = T, showAllLevels = T) %>%
  # export
  write.csv(., "outputs/bar_data.csv")

# find number that was applicable for each strobe item (this is the number that were not na in each column)
applic <- sapply(bar_data, function(x) sum(!is.na(x)))

applic <- data.frame(strobe = names(applic), applic)


# create table one and save as a data.frame
bar_data_freq <- CreateTableOne(vars = colnames(bar_data), data = bar_data,  factorVars = colnames(bar_data), includeNA = F)%>%
  print(., noSpaces = T) %>%
  as.data.frame()

# check dims

if(ncol(bar_data_freq) != 1){
  stop("wrong number of cols in bar_data_freq")
} else {
  print("bar_data_freq contains 1 col")
}

#####################
# create strobe_item col ###
#######################

# CreateTableOne creates a dataframe of one column with vars vector in row names and the response the frequency values in Overall relate to >
# means strobe items are in row names and need to be extract >
# save row names as column name
bar_data_freq$row_name <- row.names(bar_data_freq)

# remove row.names
row.names(bar_data_freq) <- c()

# row.names will contain vars vector plus row indicating number of obeservations in data (row.name = n) >
# remove the row containing the number of observations

nrow <- which(bar_data_freq$row_name == "n")

if(nrow == 1){
  #remove n_row if is first row
  test <- bar_data_freq[-nrow, ]
  if(nrow(test) == nrow(bar_data_freq) - 1){
    # change bar_data_freq to test is correct number of rows
    bar_data_freq <- test
    # remove test
    rm(test)
  }
} else {
  stop(nrow, "! = 1")
}

# extract response (1 = yes, 0 = not yes) from row name
bar_data_freq$response <- gsub(".*\\= | \\(%\\)", "", bar_data_freq$row_name) 

# extract strobe item from row name (located before =)
bar_data_freq$strobe_item <- gsub(" \\=.*", "", bar_data_freq$row_name)


# check strobe items correctly extracted

if(all(bar_data_freq$strobe_item %in% as.character(colnames(bar_data))) ==F){
  stop("some strobe_items in bar_data_freq not in bar_data col names")
}


####################
# add in applic col ####
##################

# want to know how many of the strobe items were applicable
# any that weren't applicable to any would have been dropped by CreateTableOne because all na
# vector of cols dropped during CreateTableOne 
dropped <- sapply(bar_data, function(x) (sum(is.na(x)))) %>%
  .[.==nrow(bar_data)] %>%
  names()

if(identical(sort(dropped), sort(c("6b", "10")))){
  print("dropped is all expected empty columns")
} else {
  stop("dropped different to expected empty columns")
}

# vector of cols in bar_data but not bar_data_freq (i.e. dropped during table 1 creation)
no_freq <- colnames(bar_data)[which(!colnames(bar_data) %in% bar_data_freq$strobe_item)]

# check dropped vector matches no_freq vector
if(identical(sort(no_freq), sort(dropped)) == F){
  stop("cols in dropped are not the only dropped cols")
}

# find number applicable 

applic <- sapply(bar_data, function(x) sum(!is.na(x)))
# turn into dataframe
applic <- data.frame(strobe = names(applic), applic)

if(identical(as.character(applic$strobe), bar_data_freq$strobe_item) == F){
  # check those that are in applic but not bar_data_freq are just those dropped
  mismatch <- setdiff(applic$strobe, bar_data_freq$strobe_item)
  if(identical(sort(mismatch), sort(dropped))){
    applic$strobe <- as.character(applic$strobe)
    bar_data_freq$strobe_item <- as.character(bar_data_freq$strobe_item)
  } else {
    stop("strobe items in applic but not in bar_data_freq are not those dropped by CreateTableOne")
  }
}

# merge applic and bar_data_freq

test <- full_join(bar_data_freq, applic, by = c("strobe_item" = "strobe"))

if(identical(test$strobe_item[gtools::mixedorder(test$strobe_item)], 
             colnames(bar_data)[gtools::mixedorder(colnames(bar_data))])){
  # if strobe items in test are same as strobe items in bar_data (need to sort both by mixedorder to check identical) >
  # assign test to bar_data_freq
  bar_data_freq <- test
}
###########################
# extract percent and count ####
##########################

# extract percentage from Overal column (contained in brackets)
bar_data_freq$percent <- gsub(".*\\(|\\)", "", bar_data_freq$Overall) %>%
  # convert to numeric
  as.numeric()

# extract n from Overall column (located before brackets)
bar_data_freq$n <- gsub(" \\(.*", "", bar_data_freq$Overall) %>%
  as.numeric()

bar_data_freq$response <- as.numeric(bar_data_freq$response)

# drop overall and row_name cols now successfully separated
bar_data_freq <- select(bar_data_freq, -c("Overall", "row_name"))

if(is.numeric(bar_data_freq$response)){
  # find percent for no responses
  no <- which(bar_data_freq$response == 0)
}

if(bar_data_freq$percent[no] == 100){
  bar_data_freq$percent[no] <- 0
  bar_data_freq$response[no] <- 1
}

if(sum(bar_data_freq$response, na.rm = T) == sum(!is.na(bar_data_freq$response))){
  # remove response col if it only contains 1s
  bar_data_freq$response <- NULL
  colnames(bar_data_freq)[which(colnames(bar_data_freq) == "percent")] <- "percent_yes"
} else {
  stop("not all responses = 1")
}
#############################
# export data to create bar chart labels ####
##########################

strobe_qs <- read.csv("outputs/extraction_dictionary.csv", stringsAsFactors = F, encoding = "UTF-8") %>%
  select(., c("question", "variable"))

# select those that are strobe items
strobe_qs <- strobe_qs[grep("X[[:digit:]]{1,2}", strobe_qs$variable), ]
strobe_qs <- strobe_qs[-grep("\\_ev|\\_star", strobe_qs$variable), ]

strobe_qs$question <- sub("[^0-9]*", "", strobe_qs$question)
strobe_qs$strobe_item <- sub("\\_.*", "", strobe_qs$variable)
write.csv(strobe_qs, "outputs/strobe_dict.csv", row.names = F, fileEncoding = "UTF-8")

##############
## MANUAL ####
############

# MANUALLY INSTRUCTIONS
# added text describing the strobe item TO THE strobe_dict.csv AND SAVED IT AS bar_labels.csv

###########################
# import labels and clean ####
########################

# import general as these contain generic labels for all strobe items (not design specific)
labels <- read.csv("bar_labels_general.csv", encoding = "UTF-8", stringsAsFactors = F, na.strings = "") %>%
  select(c("variable", "bar_label"))

# clean variable names so matches bar_chart_freq colnames (i.e. no subdivisions and no design specific questions) >
# easiest to do this by cleaning non-cohort specific quesitons first then cohort excluding 14c_coh since this contains no roman numerials and is not a sub division
labels$variable <- gsub("_i.*|_v.*|starred|X|_cc|_cs", "", labels$variable)
labels$variable[!grepl("14c_coh", labels$variable)]  <- gsub("_coh", "", labels$variable[!grepl("14c_coh", labels$variable)])

# remove duplicated rows now variables cleaned

labels <- labels[!duplicated(labels),]
labels <- labels[mixedorder(as.character(labels$variable)),]

bar_data_freq <- bar_data_freq[mixedorder(as.character(bar_data_freq$strobe_item)),]

if(identical(labels$variable, as.character(bar_data_freq$strobe_item))){
  # if labels = colnames for bar_data_freq data add applic col to labels
  labels$applic <- bar_data_freq$applic
} else {
  stop("not identical")
}



#######################
## bar chart for all ####
##################
# drop strobe items that didn't apply to any articles so won't be displayed in bar chart
all_na <- bar_data_freq[which(bar_data_freq$applic == 0), ]

if((is.na(all_na$percent_yes) && is.na(all_na$n)) == F) stop("all_na not empty")

bar_data_freq <- bar_data_freq[which(bar_data_freq$applic != 0), ]

# drop empties from label as well so labels will map to chart but save incase
labels_na <- labels[which(labels$applic == 0), ]
labels <- labels[which(labels$applic != 0), ]

# create labels
if(identical(labels$variable, 
             as.character(bar_data_freq$strobe_item))){
  # if labels = colnames for bar_data_freq data add applic col to labels
  labels$applic <- bar_data_freq$applic
  # paste variable together with description , separate by space so can gsub later
  labels$x_labels <- paste(labels$variable, labels$bar_label, sep = " ") %>%
    # order
    .[gtools::mixedorder(.)]%>%
    # wrap
    stringr::str_wrap(., width = 55) %>%
    # add n = applicable on a newline
    paste(., " (n=", labels$applic, ")", sep = "")
} else {
  stop("not identical")
}


# set levels as mixed order so order preserved in ggplot
bar_data_freq$strobe_item<- factor(bar_data_freq$strobe_item, levels = 
                                     bar_data_freq$strobe_item[gtools::mixedorder(bar_data_freq$strobe_item)])

# check labels identical to strobe item numbers in labels$x_labels (gsub at the space) to ensure labels will map to chart
if(identical(gsub(" .*", "", labels$x_labels), as.character(bar_data_freq$strobe_item)) == F) stop("labels won't map")

png("Rplot9.png", width = 1200, height = 1500)
# strobe item on x axis and plot % yes
bar_data_freq %>%ggplot(aes(x=strobe_item, y=percent_yes)) + 
  # blue fill
  geom_bar(stat="identity", fill = "#A5ECEE") +
  theme_classic() +
  theme(text = element_text(size=30))+
  scale_x_discrete(labels= labels$x_labels) +
  coord_flip() +
  xlab("STROBE Item") +
  ylab("Yes (%)") +
  ggtitle("Figure 1. STROBE completion")+
  theme(plot.title = element_text(size = 22)) 
dev.off()

###################
# separate bar charts ####
#################
# manuallly type out vectors of intro & methods items and result & discussion items


intro_meth <- c("1a", "1b", "2", "3", "4", "5", "6a", "6b", "7", "8", "9", "10", "11", "12a","12b", "12c", "12d", "12e") %>%
  .[mixedorder(.)]

# separate into results and discussion items

results_diss <- c("13a", "13b", "13c", "14a", "14b", "14c_coh", "15", "16a",
                "16b", "16c", "17", "18", "19", "20", "21", "22") %>%
  .[mixedorder(.)]

# remove those that match strobe items in all_na

if(sum(results_diss %in% all_na$strobe_item) != 0){
  results_diss <- results_diss[-which(results_diss %in% all_na$strobe_item)]
} else {
  print("no results_diss items all empty")
}

if(sum(intro_meth %in% all_na$strobe_item) != 0){
  intro_meth <- intro_meth[-which(intro_meth %in% all_na$strobe_item)]
}else {
  print("no intro_meth items all empty")
}

if(identical(c(intro_meth, results_diss), as.character(bar_data_freq$strobe_item))){
  # if all colnames in strobe items create separate dfs for intro & methods items 
  intro_meth_df <- bar_data_freq[which(bar_data_freq$strobe_item %in% intro_meth), ]
  # same for results & discussion
  result_diss_df <- bar_data_freq[which(bar_data_freq$strobe_item %in% results_diss), ]
} else {
  stop("colnames in intro_meth and results_diss different to strobe items in bar_data_freq")
}
# create labels

intro_meth_labels <- labels$x_labels[which(labels$variable %in% intro_meth_df$strobe_item)]
result_diss_labels <- labels$x_labels[which(labels$variable %in% result_diss_df$strobe_item)]
################################
# plot separate bar charts ####
############################
# set levels as mixed order so order preserved in ggplot
intro_meth_df$strobe_item<- factor(intro_meth_df$strobe_item, levels = 
                                     intro_meth_df$strobe_item[gtools::mixedorder(intro_meth_df$strobe_item)])


# set levels as mixed order so order preserved in ggplot
result_diss_df$strobe_item<- factor(results_diss$strobe_item, levels = 
                                    results_diss$strobe_item[gtools::mixedorder(results_diss$strobe_item)])

# check labels identical to strobe item numbers in labels$x_labels (gsub at the space) to ensure labels will map to chart
if(identical(gsub(" .*", "", intro_meth_labels), as.character(intro_meth_df$strobe_item)) == F) stop("labels won't map")

# strobe item on x axis and plot % yes
intro_meth_df %>%ggplot(aes(x=strobe_item, y=percent_yes)) + 
  # blue fill
  geom_bar(stat="identity", fill = "#A5ECEE") +
  theme_classic() +
  # use labels created previously
  scale_x_discrete(labels= intro_meth_labels) +
  # leave xlab blank otherwise it will appear
  xlab("")+
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  ylab("Yes (%)") +
  ggtitle("Figure 1. STROBE introduction and method items") 


# check labels identical to strobe item numbers in labels$x_labels (gsub at the space) to ensure labels will map to chart
if(identical(gsub(" .*", "", result_diss_labels), as.character(result_diss_df$strobe_item)) == F) stop("labels won't map")

# strobe item on x axis and plot % yes
result_diss_df %>%ggplot(aes(x=strobe_item, y=percent_yes)) + 
  # blue fill
  geom_bar(stat="identity", fill = "#cba5ee") +
  theme_classic() +
  # use labels created previously
  scale_x_discrete(labels= result_diss_labels) +
  # leave xlab blank otherwise it will appear
  xlab("")+
  ylab("Yes (%)") +
  ggtitle("Figure 2. STROBE results and discussion items") 




####################
# check bar chart ####
###################

check <- s_df
# recode all strobe cols in check
for(i in strobe_cols){
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


#######################
# frequencies of strobe ####
######################


strobe_freq_wide <- NULL

for(i in strobe_cols){
  x <- prop.table(table(s_df[[i]], useNA = "always"))
  x_df <- data.frame(strobe = i,x[1],x[2],x[3],x[4],x[5], x[6])
  colnames(x_df) <- c("strobe", names(x))
  strobe_freq_wide <- rbind(strobe_freq_wide, x_df)
  row.names(strobe_freq_wide) <- c()
}

strobe_freq_narr <- NULL

# narrow strobe_freq
for(i in strobe_cols){
  s_df[[i]] <- as.factor(s_df[[i]])
  levels(s_df[[i]]) <- list(Yes = "Yes", PartiallyExternal = "Partially-External",
                            Partially = "Partially", No = "No", Unsure = "Unsure")
  x <- data.frame(strobe = i,prop.table(table(s_df[[i]], useNA = "always")))
  strobe_freq_narr <- rbind(strobe_freq_narr, x)
  row.names(strobe_freq_narr) <- c()
}


strobe_freq_narr %>%ggplot(aes(fill=Var1, y=Freq, x=strobe)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  scale_fill_manual(values= c("#009E73","#E69F00",  "#F0E442", "#D55E00", "#0072B2", "#999999")) +
  theme(axis.text.x=element_text(angle=45,hjust=1))

###########
# ncols ####
#########

cols <- c(colnames(s_df), colnames(coh_df), colnames(cc_df), colnames(cs_df), colnames(not_s_df), colnames(s_star_df), na_cols)

cols <- cols[!duplicated(cols)]

if(length(cols) != length(df_cols)) stop("different number of columns in all dfs compared to cleaned csv")

#####################################
# frequencies for dicotomised strobe items ####
######################################

# dichotomise strobe item responses into yes and no

s_bin <- s_df

strobe_bin_freq <- NULL

for(i in strobe_cols){
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
