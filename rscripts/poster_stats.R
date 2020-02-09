############
# libraries ####
#############
library(dplyr)
library(psychTools)
#################################
# split into strobe and non-strobe ####
#################################

df <- read.csv("outputs/clean_poster.csv", encoding = "UTF-8")

strobe_cols <- grep("^X[1-9]{1,2}", colnames(df))

if(length(strobe_cols) == 0) stop("strobe_cols empty")

if(all(abs(diff(strobe_cols))) == T){
  s_df <- select(df, "article_id", strobe_cols)
  not_s_df <- select(df, -strobe_cols)
}else {
  stop("strobe col positions not sequential")
}

if(ncol(s_df) + ncol(not_s_df) != ncol(df) +1) stop("wrong number of cols in strobe or not_strobe dfs")


n_unique <- stack(sapply(df, function(x) length(unique(x)))) 

sapply(s_df, table)

