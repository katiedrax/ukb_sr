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

# manually checked strobe item 10. should all be NA because all studies used all eligible ppts
df$X10 <- NA

#manually checked strobe item 6 and 12_d_cc, should all be NA because no matched articles
match_cols <- grep("X6_b|X12_d_cc", colnames(df), value = T)

# make all cols in match_cols NA
for(i in match_cols){
  df[[i]] <- NA
  if(all(is.na(df[[i]])) == F) stop("all isn't NA in ", i)
}

###################################
# split into strobe and non-strobe ####
#################################

# find all strobe cols
strobe_cols <- grep("^X[1-9]{1,2}", colnames(df))


if(length(strobe_cols) == 0) stop("strobe_cols empty")

if(all(abs(diff(strobe_cols))) == T){
  s_df <- select(df, "article_id", strobe_cols)
  not_s_df <- select(df, -strobe_cols)
}else {
  stop("strobe col positions not sequential")
}

if(ncol(s_df) + ncol(not_s_df) != ncol(df) +1) stop("wrong number of cols in strobe or not_strobe dfs")

#####################################
# find not applicable strobe items #####
####################################

# save number of s_df cols before remove NA so can check later

all_s_df_cols <- ncol(s_df)

# save and remove strobe items that were not applicable for any articles

na_cols <- c()

for(i in colnames(s_df)){
  if(all(is.na(s_df[[i]]))){
    na_cols <- c(na_cols, i)
    s_df[[i]] <- NULL
  }
}

if(length(na_cols) +length(colnames(s_df)) != all_s_df_cols) stop("some cols unaccounted for after removing NA cols")

############################################
# split strobe df by stars and designs ####
##########################################

# separate star first since even though they are design specific >
# this is because the advice is the same (show information separately for 2 groups)>
# it's just that the 'groups' differ for the designs >
# cases & controls for cc and exposed & unexposed for coh & cs designs

if(identical(grep("star_", colnames(s_df)), grep("\\_star\\_", colnames(s_df)))){
  star <- grep("star_", colnames(s_df))
  s_star_df <- s_df[,star]
  s_df <- s_df[, -star]
} else {
  stop("star not identical")
}

# select cohort specific cols
if(length(grep("coh", colnames(s_df))) == length(grep("\\_coh", colnames(s_df)))){
  coh <- grep("coh", colnames(s_df))
  coh_df <- s_df[, coh]
} else {
  stop("coh different lengths")
}

# select case control specific cols
if(length(grep("cc", colnames(s_df))) == length(grep("\\_cc", colnames(s_df)))){
  cc <- grep("cc", colnames(s_df))
  cc_df <- s_df[, cc]
}else {
  stop("cc different lengths")
}

# select cs specific cols
if(length(grep("cs", colnames(s_df))) == length(grep("\\_cs", colnames(s_df)))){
  cs <- grep("cs", colnames(s_df))
  cs_df <- s_df[, cs]
}else {
  stop("cs different lengths")
}
design_specific_cols <- c(cs,cc,coh) %>%
  unique(.)

s_df <- s_df[, -design_specific_cols]

#######################
# frequencies of strobe ####
######################

# find all strobe cols, if 
strobe_cols <- grep("^X[1-9]{1,2}", colnames(s_df), value =T)

strobe_freq_wide <- NULL

# wide strobe_freq
for(i in strobe_cols){
  s_df[[i]] <- as.factor(s_df[[i]])
  levels(s_df[[i]]) <- list(Yes = "Yes", PartiallyExternal = "Partially-External",
                            Partially = "Partially", No = "No", Unsure = "Unsure")
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
