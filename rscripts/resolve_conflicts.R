library(dplyr)

df <- read.csv("outputs/clean_designs.csv", stringsAsFactors = F, encoding = "UTF-8")

# function to check conflicts


find_cons <- function(df,a,b){
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

#############
# get articles ####
################

# df of all articles in csv_clean_epi.csv on OSF

articles_df <- read.csv("https://osf.io/8uy9w/?action=download", encoding = "UTF-8", stringsAsFactors = F)

# vector of all article id's in csv_clean_epi.csv on oSF

articles <- articles_df$id

########################
# find article_access conflicts ####
#########################

# find article access conflicts
cons_art <- find_cons(df, df$access_article.kd, df$access_article.mg)%>%
  select(c(article_id, access_article.kd, access_article.mg, title)) %>%
  print()

# mark could not find article for Tyree2015bank0732 so he incorrectly answered "Not present"
# if cons_art only contain Tyree2015bank0732 correct Mark's responses to "Present but not accessible"
if("Tyrre2015bank0732" %in% cons_art$article_id){
  df$access_article.mg[df$article_id == "Tyrre2015bank0732"] <- "Present but not accessible"
} else {
  stop("Tyree2015bank0732 has no article access conflict")
}

# rename access_article if no conflicts

df <- rename_col(df, "access_article.kd", "access_article.mg", "access_article")

###############################
# find supp material (SM) access conflicts ####
###############################

# add column indicating if SM has been tracked down for articles with "Present but not availiable" SM

df$supp_location <- NA

# find conflicts in access to SM

cons_supp <- find_cons(df, df$access_supp.kd, df$access_supp.mg)%>%
  select(c(article_id, access_supp.kd, access_supp.mg, title))

# conflicts in SM access do not require official resolution between Mark and Katie
# manually check if cons_supp are or aren't accessible then resolve here

# searching showed Orteg2017bank97-5, Malon2017mple7639 and Stile2017bankx080 have no SM
# correct Mark's responses

no_sm <- c("Orteg2017bank97-5", "Malon2017mple7639", "Stile2017bankx080")
df$access_supp.mg[df$article_id %in% no_sm] <- "Not present"

# Tyrre2013tudyt220's SM is not accessible via journal online version but is via PMC

df$access_supp.mg[df$article_id == "Tyrre2013tudyt220"] <- "Present but not accessible"

# mark could not find SM for Tyree2015bank0732 so he incorrectly answered "Not present"
# correct Mark's responses to "Present but not accessible"
df$access_supp.mg[df$article_id == "Tyrre2015bank0732"] <- "Present but not accessible"


# searching showed SM of these articles were present and accessible

yes_sm <- c("Papag2019omen2359", "Celis2017tudy1456", "Keids2016tion0196", 
            "Magnu2017tics0712", "Piuma2018bankx186", "Pyrko2018ords1603", "Tyrre2014ease0041")

df$access_supp.mg[df$article_id %in% yes_sm] <- "Yes"

# searching showed Cummi2016bank53-X SM was not accessible
# correct Katie's & Mark's response

df$access_supp.mg[df$article_id == "Cummi2016bank53-X"] <- "Present but not accessible"
df$access_supp.kd[df$article_id == "Cummi2016bank53-X"] <- "Present but not accessible"

# rename col if no conflicts

df <- rename_col(df, "access_supp.kd", "access_supp.mg", "access_supp")


###############
# add SM locations ####
#############

sm_to_find <- df[which(df$access_supp == "Present but not accessible"),] %>%
  select(c(article_id, title))

# add locations of SM for articles with inaccessible SM at the journal site
df$supp_location[df$article_id == "Tyrre2013tudyt220"] <- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3887570/"

# Pyrko2018ords1603 SM is hard to find so add location in case
df$supp_location[df$article_id == "Pyrko2018ords1603"] <- "In PDF version on journal site - https://s3-us-west-1.amazonaws.com/paperchase-aging/pdf/fvqCuvZKKeiWFDrzE.pdf"

###########################
# get design conflict resolutions ####
############################

# conflicts were resolved manually by Mark and Katie. Any conflicts Mark and Katie couldn't resolve were resolved by Becky
# resolutions from Mark and Katie's discussions are in two csv files
# read in these csvs

resolve_km_1 <- read.csv("conflicts_resolution/design_conflicts_1.csv", header = T, 
                      stringsAsFactors = F, na.strings = "") %>%
  # remove articles that weren't sent to Becky or are empty in all other resol columns
  .[(.$resol_becky) ==T | !is.na(.$resol_correct_design),] %>%
  # drop all columns other than those added during conflict resolution
  select(., grep("resol|article_id", colnames(.), value = T))


resolve_km_2 <- read.csv("conflicts_resolution/design_conflicts_2.csv", header = T, 
                      stringsAsFactors = F, na.strings = "") %>%
  # remove articles that weren't sent to Becky or are empty in all other resol columns
  .[(.$resol_becky) ==T | !is.na(.$resol_correct_design),] %>%
  # drop all columns other than those added during conflict resolution
  select(., grep("resol|article_id", colnames(.), value = T))

resolve_km <- rbind(resolve_km_1, resolve_km_2)

###################
# get becky design conflict resolutions ####
###################

#read in becky's resolved conflicts
becky <- read.csv("conflicts_resolution/becky_resolution.csv", header = T, na.strings = c("", " "),
                  stringsAsFactors = F, encoding = "UTF-8")

# rename article_id to remove any junk text added during import
colnames(becky) <- gsub(".*article_id.*", "article_id", colnames(becky))

# merge becky with resolve

resolved <- full_join(resolve_km, becky, by = "article_id")

# Becky's judgement is paramount so set the correct_designs value of an article to her judgement if she has given it for

resolved$resol_correct_design[!is.na(resolved$resol_correct_design_becky)] <- resolved$resol_correct_design_becky[!is.na(resolved$resol_correct_design_becky)]

# Becky's judgements have been added so remove resol_correct_design_becky column

resolved$resol_correct_design_becky <- NULL

#####################
# resolve design conflicts ####
####################

# merge df with resolved so can see what the correct designs of the conflicted articles are

df <- full_join(df, resolved, by = "article_id")

# find those that failed to merge
if(nrow(anti_join(resolved, df, by = "article_id")) >0){
  fail <- (anti_join(resolved, df, by = "article_id"))
  print(fail$article_id)
  stop("article_ids above failed to merge")
} else {
  print("merge successful")
}

# find conflicts

cons_design <- find_cons(df, df$design_all.kd, df$design_all.mg) %>%
  select(c(article_id, design_all.kd, design_all.mg, resol_correct_design, resol_becky), everything())

# McMen2018bank1375 is clearly a cohort design

df$resol_correct_design[df$article_id == "McMen2018bank1375"] <- "cohort"

# assign Sarka2018ants.009 to Becky to resolve

df$resol_becky[df$article_id == "Sarka2018ants.009"] <- T

# replace Katie & Mark's designs with correct designs

df$design_all.kd[!is.na(df$resol_correct_design)] <- df$resol_correct_design[!is.na(df$resol_correct_design)]
df$design_all.mg[!is.na(df$resol_correct_design)] <- df$resol_correct_design[!is.na(df$resol_correct_design)]

# if no conflicts rename design column

df <- rename_col(df, "design_all.kd", "design_all.mg", "design")

#####################
# TEMP sort conflicts ####
########################

# create temp df
temp <- df

# find conflicts again
cons_design <- find_cons(temp, temp$design_all.kd, temp$design_all.mg)
cons_sups <- find_cons(temp, temp$access_supp.kd, temp$access_supp.mg)
cons_art <- find_cons(temp, temp$access_article.kd, temp$access_article.mg)

# set all designs of articles with some inaccessible materials to TBC

temp$design_all.kd[(temp$access_article.kd != "Yes" & temp$access_article.mg != "Yes") | 
     (temp$access_supp.kd == "Present but not accessible" & temp$access_supp.mg == "Present but not accessible")] <- "TBC"

temp$design_all.mg[(temp$access_article.kd != "Yes" & temp$access_article.mg != "Yes") | 
               (temp$access_supp.kd == "Present but not accessible" & temp$access_supp.mg == "Present but not accessible")] <- "TBC"

# set all values to NA for article_ids mark have not assessed (this is to maintain blinding)

na_id <- temp$article_id[is.na(temp$design_all.kd) | is.na(temp$design_all.mg)]
cols <- colnames(temp)[colnames(temp) != "article_id"]
temp[temp$article_id %in% na_id, colnames(temp) %in% cols] <- NA

# set all designs of articles currently in conflict to  TBC

temp$design_all.kd[temp$article_id %in% cons_design$article_id] <- "TBC"
temp$design_all.mg[temp$article_id %in% cons_design$article_id] <- "TBC"

# try to rename again


temp <- rename_col(temp, "access_article.kd", "access_article.mg", "access_article")

temp <- rename_col(temp, "access_supp.kd", "access_supp.mg", "access_supp")
temp <- rename_col(temp, "design_all.kd", "design_all.mg", "design")

# drop al irrelevant columns

#  no conflicts remain so don't need any columns that were used to resolve conflicts
cols <- colnames(temp)[grepl("comments|design_|resol|design.", colnames(temp)) == F]

temp <- temp[, colnames(temp)%in% cols]
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
