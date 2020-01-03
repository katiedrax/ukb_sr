library(dplyr)

both <- read.csv("outputs/clean_designs.csv", stringsAsFactors = F, encoding = "UTF-8")

# function to check conflicts

find_cons <- function(df,kd_col,mg_col){
  # create df of katie's and Mark's columns that don't match
  x <- df[which(kd_col != mg_col),]
  # if conflicts return df and present warning
  if(nrow(x) >0){
    warning("conflicts")
    return(x)
  } else {
    # if no conflicts print confirmation
    print("no conflicts")
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
article_cons <- find_cons(both, both$access_article.kd, both$access_article.mg)%>%
  select(c(article_id, access_article.kd, access_article.mg, title)) %>%
  print()

# mark could not find article for Tyree2015bank0732 so he incorrectly answered "Not present"
# if article_cons only contain Tyree2015bank0732 correct Mark's responses to "Present but not accessible"
if(article_cons$article_id == "Tyrre2015bank0732"){
  both$access_article.mg[both$article_id == "Tyrre2015bank0732"] <- "Present but not accessible"
} else {
  stop("Tyree2015bank0732 not the only article access conflict")
}

# check article conflicts again

check_art <- find_cons(both, both$access_article.kd, both$access_article.mg)

if(check_art == "no conflicts"){
  # add any access_article values in Katie's column that are missing in Marks so these aren't lost
  both$access_article.mg[is.na(both$access_article.mg)] <- both$access_article.kd[is.na(both$access_article.mg)]
  # if no conflicts remove one access_supp column
  both$access_article.kd <- NULL
  # rename the remaining column as access_supp
  colnames(both)[colnames(both) == "access_article.mg"] <- "access_article"
} else {
  stop("still has access_supp conflicts")
}

###############################
# find supp material (SM) access conflicts ####
###############################

# add column indicating if SM has been tracked down for articles with "Present but not availiable" SM

both$supp_location <- NA

# find conflicts in access to SM

supp_cons <- find_cons(both, both$access_supp.kd, both$access_supp.mg)%>%
  select(c(article_id, access_supp.kd, access_supp.mg, title))

# conflicts in SM access do not require official resolution between Mark and Katie
# manually check if supp_cons are or aren't accessible then resolve here

# searching showed Orteg2017bank97-5, Malon2017mple7639 and Stile2017bankx080 have no SM
# correct Mark's responses

no_sm <- c("Orteg2017bank97-5", "Malon2017mple7639", "Stile2017bankx080")
both$access_supp.mg[both$article_id %in% no_sm] <- "Not present"

# Tyrre2013tudyt220's SM is not accessible via journal online version but is via PMC

both$access_supp.mg[both$article_id == "Tyrre2013tudyt220"] <- "Present but not accessible"

# mark could not find SM for Tyree2015bank0732 so he incorrectly answered "Not present"
# correct Mark's responses to "Present but not accessible"
both$access_supp.mg[both$article_id == "Tyrre2015bank0732"] <- "Present but not accessible"


# searching showed SM of these articles were present and accessible

yes_sm <- c("Papag2019omen2359", "Celis2017tudy1456", "Keids2016tion0196", 
            "Magnu2017tics0712", "Piuma2018bankx186", "Pyrko2018ords1603", "Tyrre2014ease0041")

both$access_supp.mg[both$article_id %in% yes_sm] <- "Yes"

# searching showed Cummi2016bank53-X was not accessible
# correct Katie's & Mark's response

both$access_supp.mg[both$article_id == "Cummi2016bank53-X"] <- "Present but not accessible"
both$access_supp.kd[both$article_id == "Cummi2016bank53-X"] <- "Present but not accessible"

# check conflicts again 
check_supp <- find_cons(both, both$access_supp.kd, both$access_supp.mg)

if(check_supp == "no conflicts"){
  # if not conflicts remove katie's access_supp column
  # add any access_supp values in Katie's column that are missing in Marks so these aren't lost
  both$access_supp.mg[is.na(both$access_supp.mg)] <- both$access_supp.kd[is.na(both$access_supp.mg)]
  # then remove Katies access_supp column
  both$access_supp.kd <- NULL
  # rename the remaining column as access_supp
  colnames(both)[colnames(both) == "access_supp.mg"] <- "access_supp"
} else {
  stop("still access_supp conflicts")
}

###############
# locate SM ####
#############

sm_to_find <- both[which(both$access_supp == "Present but not accessible"),] %>%
  select(c(article_id, title))

# add locations of SM for articles with inaccessible SM at the journal site
both$supp_location[both$article_id == "Tyrre2013tudyt220"] <- "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3887570/"

# Pyrko2018ords1603 SM is hard to find so add location in case
both$supp_location[both$article_id == "Pyrko2018ords1603"] <- "In PDF version on journal site - https://s3-us-west-1.amazonaws.com/paperchase-aging/pdf/fvqCuvZKKeiWFDrzE.pdf"


#########################
# find design conflicts####
##########################

conflicts <- find_cons(both, both$design_all.kd, both$design_all.mg) %>%
  select(c(article_id, design_all.kd, design_all.mg), everything())

#############
# export ####
##########

# export design conflicts
write.csv(conflicts, "outputs/conflicts.csv", row.names = F, fileEncoding = "UTF-8")

#######################################################

###################################
# read in conflict resolution csvs#
###################################

# These csvs were used by Mark & Katie to resolve conflicts in the design choices


resolve_km_1 <- read.csv("conflicts_resolution/design_conflicts_1.csv", header = T, 
                      stringsAsFactors = F, na.strings = "")

resolve_km_2 <- read.csv("conflicts_resolution/design_conflicts_2.csv", header = T, 
                      stringsAsFactors = F, na.strings = "")


# remove articles that weren't sent to Becky or are empty in all other resol columns

resolve_km_1 <- resolve_km_1[(resolve_km_1$resol_becky) ==T | !is.na(resolve_km_1$resol_correct_design) | !is.na(resolve_km_1$resol_correct_supp_access),]  
resolve_km_2 <- resolve_km_2[resolve_km_2$resol_becky ==T | !is.na(resolve_km_2$resol_correct_design),]  

# to merge on article_id drop all columns other than those added during conflict resolution

resolve_km_1 <- select(resolve_km_1, grep("resol|article_id", colnames(resolve_km_1), value = T))
resolve_km_2 <- select(resolve_km_2, grep("resol|article_id", colnames(resolve_km_2), value = T))
resolve_km_2$resol_correct_supp_access <- NA

resolve_km <- rbind(resolve_km_1, resolve_km_2)

###################
# merge in becky ####
###################

#read in becky's resolved conflicts
becky <- read.csv("conflicts_resolution/becky_resolution.csv", header = T, stringsAsFactors = F, encoding = "UTF-8")

# rename article_id to remove any junk text added during import
colnames(becky) <- gsub(".*article_id.*", "article_id", colnames(becky))

# merge becky with resolve

resolved <- full_join(resolve_km, becky, by = "article_id")

#####################
# merge with conflicts ####
####################

# read in conflicts

conflicts <- read.csv("outputs/conflicts.csv", encoding = "UTF-8", stringsAsFactors = F)

# merge conflicts with resolved so can see what the correct designs of the conflicted articles are

df <- full_join(resolved, conflicts, by = "article_id")

# find those that failed to merge
if(length(anti_join(resolved, conflicts, by = "article_id")) >0){
  fail <- (anti_join(resolved, conflicts, by = "article_id"))
  print(fail$article_id)
  stop("article_ids above failed to merge")
}


# McMen2018bank1375 is clearly a cohort design

df$resol_correct_design[df$article_id == "McMen2018bank1375"] <- "cohort"

# assign Sarka2018ants.009 to Becky to resolve

df$resol_becky[df$article_id == "Sarka2018ants.009"] <- T

df$correct_design_becky[df$correct_design_becky == ""] <- NA

# get titles of conflicts to be resolved by Becky

b <- df$article_id[df$resol_becky == T & is.na(df$correct_design_becky)]%>%
  .[!is.na(.)]

becky_it <- df %>%
  select(., article_id, title.kd, title.mg) %>%
  .[.$article_id %in% b, ]

write.csv(becky_it, "../../Desktop/to becky.csv", row.names = F, fileEncoding = "UTF-8")
