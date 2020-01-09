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

#########
# exclude ####
#######

# exclude any designs that need excluding because they are not epidemiological articles

nepi <- grep("exclude_nepi", temp$design)
temp <- temp[-nepi, ]
# export list of not epi articles
write.csv(temp[nepi, ], "outputs/not_epi.csv", fileEncoding = "UTF-8", row.names = F)

###########################
# get first 80 articles ####
############################

if(exists("cons_design")|exists("cons_supp") | exists("cons_art")|sum(is.na(temp$design)) != 0){
  stop("still conflicts and missing designs")
} else {
  #no conflicts remain so don't need any columns that were used to resolve conflicts
  cols <- colnames(temp)[grepl("comments|design_|resol|design.|title_sub", colnames(temp)) == F]
  # select first 80 random numbers
  first_80 <- temp[temp$num %in% 1:80, colnames(temp) %in% cols] %>%
    #order by number
    .[order(.$num),]
}

# randomly assign Becky, Benji or Mark as second extractor - first lot Benji promised to do 20 articles, Becky 30 and Mark 30
set.seed(1)
first_80$extractor_1 <- rep("Katie", length(first_80$article_id))
first_80$extractor_2 <- sample(c(rep("Becky", 30), rep("Benji", 20), rep("Mark", 30)), replace = F)
first_80 <- select(first_80, num, extractor_1, extractor_2, everything())

table(first_80$design, useNA = "always")

if(sum(first_80$extractor_2 == "Mark") == 30 & sum(first_80$extractor_2 == "Benji") == 20 & sum(first_80$extractor_2 == "Becky") == 30) {
  print("Mark and Becky have 30 and Benji has 20")
  } else {
    stop("too many/few articles assigned to some extractors")
}

##################
# merge first_80 ####
##################

# df of all articles in csv_clean_epi.csv on OSF

articles_df <- read.csv("https://osf.io/8uy9w/?action=download", encoding = "UTF-8", stringsAsFactors = F)%>%
  select(-c("title", "abstract", "class", "rayyan"))

# merge with articles_df so extractors can use DOI and journal name to help locate article

if(nrow(anti_join(first_80, articles_df, by = c("article_id" = "id"))) == 0){
  first_80_merged <- full_join(first_80, articles_df, by = c("article_id" = "id"))%>%
    # drop nas in num col
    .[!is.na(.$num),]
} else{
  stop("some articles can't merge")
}

######################
# export first_80_merged ####
######################

if(identical(first_80_merged$article_id, first_80$article_id)){
  write.csv(first_80_merged, "outputs/first_80.csv", fileEncoding = "UTF-8", row.names = F) 
  } else {
    stop("article_id not identical in merge")
  }

####################################
# export list of conflicts for becky ####
#####################################

# get titles of conflicts to be resolved by Becky

b <- temp$article_id[temp$resol_becky == T & is.na(temp$resol_correct_design)]%>%
  .[!is.na(.)]

becky_it <- df %>%
  select(., article_id, title) %>%
  .[.$article_id %in% b, ]

write.csv(becky_it, "../../Desktop/to becky.csv", row.names = F, fileEncoding = "UTF-8")

