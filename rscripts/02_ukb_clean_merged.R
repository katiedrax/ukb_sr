# Description

# Clean the merged datafile containing the included references, their citations and their scopus profiles

################
#### Import ####
#################

df <- read.csv("outputs/merged.csv", stringsAsFactors = F, encoding = "UTF-8")

################
#### clean ####
################

# remove empty cols

all_na <- sapply(df, function(x) all(is.na(x)))

df <- df[!all_na]

# remove scopus result that is not in refs 

df <- df[!is.na(df$title), ]

# remove title_1 col - this is a duplication of the title col that endnote seems to export

if (length(setdiff(df$title, df$title_1)) & length(setdiff(df$title_1, df$title)) > 0){
  stop("title_1 not dup of title")
} else {
  df <- select(df, -c(title_1))
}
