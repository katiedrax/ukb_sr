#############
# libraries ####
#############

library(dplyr)
library(stringr)

###############
# functions ####
##############

source("rscripts/functions/clean-string-fun.R")

##########
# import####
##########

# assign input file 

input <- "data/data_extraction/Data+Extraction+Form_4+February+2020_20.22.csv"

# Import first two rows of the  Qualtrics csv export with first row as headers

header <- read.csv(input, encoding = "UTF-8", nrows = 2, stringsAsFactors = F)

# vector of cols automatically outputted by Qualtrics 

qual_cols <- c("Start Date", "End Date", "Status", "Recorded Date", "Response Id", "Progress",
               "Duration (in seconds)", "Finished", "Distribution Channel", "User Language")

qual_cols_head <- word(qual_cols, start = 1)

###############
# check header ####
###############

# copy of header to check
test <- header

# remove all qual_cols if header contains them this makes checking easier
if(sum(grepl(paste(qual_cols_head, collapse = "|"), colnames(test)[1:10])) == 10){
  test <- test[, -c(1:10)]
} else{
  stop("header doesn't contain qual_cols")
}

# question names were not set in qualtrics  >
# so without qual_cols, test col names should all contain question numbers in the format Q[1-2 digit number] >
# warn user if this isn't true

if(sum(grepl("Q[1-9]{1,2}", colnames(test))) != ncol(test)) stop("row 1 contains more than question numbers")

# row 1 should contain full question text >
# KD added variable names to all question text by putting "[variable]." at start of all questions >
# so values in row 1 (except those that were 'matrix' questions) should have this format >
# warn user if row 1 contains very few values that start "^(\w+)\."

if(sum(grepl("^\\w+\\.", test[1,])) < 10) stop("row 1 doesn't contain variable names")

# row 2 should contain import ids

if(sum(grepl("ImportId", test[2,])) != ncol(test)) stop("check header row 2 contains import ids")

################
# extract variable names ####
############

# if passed all checks in check header section create dataframe to export as data dictionary >
# extract variable names contained in row 1 and match them to their full question text in row 1 >
# the extracted variable names can then be used as a header

# dict dataframe by creating column of questions, unname so column names won't include labels
dict <- as.data.frame(t(unname(header[1, ])))
row.names(dict) <- c()

colnames(dict)[colnames(dict) == "1"] <- "question"

# extract variables from questions that start "[variable]."

var.match <- grep("^\\w+\\.", dict$question, value = T) %>%
  # remove the columns containing text entries (qualtrics exports these as the question plus "- Text" at the end)
  .[-grep("\\- Text", .)]

# find matches
extract_match <- function(patterns, string){
  if(is.null(patterns) | !is.character(patterns)) stop("pattern empty or not character")
  if(is.null(string) | !is.character(string)) stop("string empty or not character")
  a <- data.frame(match = character(0), string = character(0))
  for (i in match){
    out <- data.frame(pattern = i, string = grep(i, string, value = T))
    a <- rbind(a, out)
  }
  if(nrow(a) == length(match)) {
    return(a)
  } else {
    warning("more than one match per value in string")
    return(a)
  }
}

# extract matches for variable names at the start of a string in the format "[variable]."
m <- regexpr("^\\w+\\.",var.match)
match <- regmatches(var.match, m)%>%
  #escape dots so will be searched for in for-loop
  gsub("\\.", "\\\\.", .)

# extract matches for strobe variable names which are a 'matrix' questions >
# their format is "[matrix question text] - [strobe item number]."
var.extract <- extract_match(match, var.match)

m <- regexpr("- [1-9]{1,}.",dict$question)
match <- regmatches(dict$question, m)
