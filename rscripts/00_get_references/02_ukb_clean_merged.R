# Description

# Clean the merged datafile containing the included references, their citations and their scopus profiles

##############
#### packages####
##############

library(tidyverse)

################
#### Import ####
#################

df <- read.csv("outputs/merged.csv", stringsAsFactors = F, encoding = "UTF-8")

################
#### remove all not in refs ####
################

# included references will have a 'title' value. Remove all rows that don't

df <- df[!is.na(df$title), ]

############################
#### set missing values ####
#########################

# rename Access Type

df <- rename(df, open_access = Access.Type)

# check renaming 

"open_access" %in% colnames(df) == TRUE

# check values in open_access

table(df$open_access, useNA = "always")

# rename open_access values

df$open_access[df$open_access == "Open Access"] <- "Yes"

df$open_access[df$open_access == ""] <- "No"

# check open_access values again

table(df$open_access, useNA = "always")

# set blank values to NA

df[df == ""] <- NA

########################
#### remove cols ####
#################

# remove empty cols

all_na <- sapply(df, function(x) all(is.na(x)))

if (sum(all_na) > 0) {
  df <- df[!all_na]
} else {
  print("no empty cols")
}

# find almost empty cols

almost_na <-which(sapply(df, function(x) sum(is.na(x))) >500)

# drop almost empty cols that don't contain funding text

df <- select(df, -c(start_page, alt_journal))

# export

write.csv(df, "outputs/cleaned.csv", row.names =F)
