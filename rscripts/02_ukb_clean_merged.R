# Description

# Clean the merged datafile containing the included references, their citations and their scopus profiles

################
#### Import ####
#################

df <- read.csv("outputs/merged_cleaned.csv", stringsAsFactors = F, encoding = F)

################
#### clean ####
################

# remove empty cols

all_na <- sapply(df, function(x) all(is.na(x)))

df <- df[!all_na]

# remove scopus result that is not in refs 

df_cit_scop <- df_cit_scop[!is.na(df_cit_scop$V3), ]
