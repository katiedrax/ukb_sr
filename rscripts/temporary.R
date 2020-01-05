library(dplyr)

##########
# import ####
##########

temp <- read.csv("outputs/unresolved_designs.csv", stringsAsFactors = F, encoding = "UTF-8")

##############
# functions ####
#############

# function to check conflicts


find_cons <- function(df,a,b){
  if(is.null(a)) stop("a doesn't exist")
  if(is.null(b)) stop("b doesn't exist")
  # a & b = two columns to be compared
  # df = dataframe columns are in
  # create df of katie's and Mark's columns that don't match and include NAs in matching
  x <- df[which((!is.na(a) & !is.na(b) & a==b | is.na(a) & is.na(b)) == F),]
  # if conflicts - save x and present warning
  if(nrow(x) >0){
    warning("conflicts")
    return(x)
  } else {
    # if no conflicts print confirmation
    print("no conflicts")
  }
}

# merge and rename column if two columns identical
rename_col <- function(df, a, b, b_new_name){
  # a = column you want to delete
  # b = column you want to keep and rename
  # check if there's conflicts
  if(identical(df[[a]], df[[b]])){
    # remove a column
    df[[a]] <- NULL
    # rename b
    colnames(df)[colnames(df) == b] <- b_new_name
    return(df)
  } else {
    # if still conflicts stop merge
    stop("still conflicts so can't merge columns")
  }
}



#####################
# sort conflicts ####
########################

# find conflicts again
cons_design <- find_cons(temp, temp$design_all.kd, temp$design_all.mg)
cons_supp <- find_cons(temp, temp$access_supp.kd, temp$access_supp.mg)
cons_art <- find_cons(temp, temp$access_article.kd, temp$access_article.mg)

# set all designs of articles with some inaccessible materials to TBC

temp$design_all.kd[(temp$access_article.kd != "Yes" & temp$access_article.mg != "Yes") | 
                     (temp$access_supp.kd == "Present but not accessible" & temp$access_supp.mg == "Present but not accessible")] <- "TBC"

temp$design_all.mg[(temp$access_article.kd != "Yes" & temp$access_article.mg != "Yes") | 
                     (temp$access_supp.kd == "Present but not accessible" & temp$access_supp.mg == "Present but not accessible")] <- "TBC"

# set all designs of articles currently in conflict to  TBC

temp$design_all.kd[temp$article_id %in% cons_design$article_id] <- "TBC"
temp$design_all.mg[temp$article_id %in% cons_design$article_id] <- "TBC"

# try to rename again

temp <- rename_col(temp, "access_article.kd", "access_article.mg", "access_article")
temp <- rename_col(temp, "access_supp.kd", "access_supp.mg", "access_supp")
temp <- rename_col(temp, "design_all.kd", "design_all.mg", "design")

#  no conflicts remain so don't need any columns that were used to resolve conflicts

cols <- colnames(temp)[grepl("comments|design_|resol|design.|title_sub", colnames(temp)) == F]
temp <- temp[, colnames(temp)%in% cols]

# order by number 
temp <- temp[order(temp$num),]
###########################
# export first 80 articles ####
############################

first_lot <- temp[temp$num %in% 1:80, ]

set.seed(1)
first_lot$extractor_1 <- rep("Katie", length(first_lot$article_id))
first_lot$extractor_2 <- sample(c(rep("Becky", 30), rep("Benji", 20), rep("Mark", 30)), replace = F)
first_lot <- select(first_lot, num, extractor_1, extractor_2, everything())



####################################
# export list of conflicts for becky ####
#####################################

# get titles of conflicts to be resolved by Becky

b <- df$article_id[df$resol_becky == T & is.na(df$resol_correct_design)]%>%
  .[!is.na(.)]

becky_it <- df %>%
  select(., article_id, title) %>%
  .[.$article_id %in% b, ]

write.csv(becky_it, "../../Desktop/to becky.csv", row.names = F, fileEncoding = "UTF-8")

