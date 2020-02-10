############
# libraries ####
#############
library(dplyr)
library(ggplot2)


######################
# import & correct ####
######################

# most variables are unordered factors (nominal with set responses) so read in with stringsAsFactors = T
df <- read.csv("outputs/clean_poster.csv", encoding = "UTF-8")

# vector of original column names for checking later

df_cols <- colnames(df)


# replace remove - from "Partially-External", R doesn't like punctuation in factor levels

df <- data.frame(lapply(df, function(x) {
  gsub("Partially-External", "PartiallyExternal", x)
  }))

# check removed
if(length(which(df == "Partially-External") != 0)) stop("some Partially-External remain")

# replace non-strobe cols indicating if non-strobe items present



df$ukb_app_pres <- !is.na(df$ukb_app)
df$email_pres <- !is.na(df$email)
df$country_pres <- !is.na(df$country)
df$keywords_pres <- !is.na(df$keywords)
df$coi_pres <- !is.na(df$coi)

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

#######################
# strobe item completion ####
########################

# get names of all strobe cols
strobe_cols <- grep("^X[[:digit:]]{1,2}", colnames(s_df), value = T) 

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
# these should only be questions with roman numerals in them after a "_"
strobe_div <- strobe_cols[grepl("_i|_v", strobe_cols) == T] %>%
  unique() 

# extract strobe items without sub divisions
strobe_comp <- strobe_cols[grepl("_i|_v", strobe_cols) == F] %>%
  unique() 

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
  levels(s_df[[i]]) <- list(Yes = "Yes", PartiallyExternal = "PartiallyExternal",
                            Partially = "Partially", No = "No", Unsure = "Unsure")
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
# frequencies all ####
##################

library(tableone)

for(i in colnames(df)[colnames(df) %in% colnames(not_s_df)]) {
  df[,i] <- as.character(df[,i])
}

helpers <- c("article_id", "title", "strobe_ev", "ukb_credit_ev", "reg_ev", "strobe.1", "reg_id",
             "comments", "keywords", "email", "country", "ukb_app", "coi", "predict")

cats <- df[, sapply(df, class) == 'character'] %>%
  colnames(.)%>%
  .[!(. %in% helpers)]

fact <- df[, sapply(df, class) == 'factor']%>%
  colnames(.)


tab1 <- CreateTableOne(vars = colnames(df)[!(colnames(df) %in% helpers)], data = df, factorVars = cats, includeNA = T )%>%
  print(., noSpaces = T, showAllLevels = T)

write.csv(tab1, "outputs/table1_raw.csv")

######################
# frequencies strobe ####
#####################

all_freq_narr <- NULL

# narrow all_freq
for(i in colnames(df)){
  x <- data.frame(strobe = i,prop.table(table(df[[i]], useNA = "always")))
  all_freq_narr <- rbind(all_freq_narr, x)
  row.names(all_freq_narr) <- c()
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
