#############
# libraries ####
#############

library(dplyr)
library(stringr)

###############
# functions ####
##############

source("rscripts/functions/clean-string-fun.R")


# find variable names embedded in a question string
extract_var <- function(patterns, string){
  # check patterns input correct
  if(is.null(patterns) | !is.character(patterns)) stop("pattern empty or not character")
  # check string input correct
  if(is.null(string) | !is.character(string)) stop("string empty or not character")
  # create empty dataframe with variable and question as columns
  a <- data.frame(variable = character(0), question = character(0))
  # for loop to search for each pattern in string
  for (i in patterns){
    # output rows that list the strings that matched i in patterns
    out <- data.frame(variable = i, question = grep(i, string, value = T), stringsAsFactors = F)
    # save row
    a <- rbind(a, out)
  }
  # check all string matches are unique and warn if not
  if(length(unique(a$question)) == length(a$question)) {
    return(a)
  } else {
    warning("duplicate values in question column")
    return(a)
  }
}

find_matches <- function(pattern, string){
  # get regexpr data for regmatches
  m <- regexpr(pattern ,string)
  # extract substrings that match pattern
  match <- regmatches(string, m)%>%
    #escape punctuation so will be searched if matches pasted into regex
    gsub("([[:punct:]])", "\\\\\\1", .) %>%
    # select only unique matches to avoid duplicates
    unique(.)
  return(match)
}

##########
# import####
##########

# assign input file 

input <- "data/data_extraction/Data+Extraction+Form_10+February+2020_18.20.csv"

# Import first three rows of the  Qualtrics csv export

header <- read.csv(input, encoding = "UTF-8", nrows = 3, stringsAsFactors = F, header = F)

# vector of question text associated with cols automatically outputted by Qualtrics 

qual_text <- c("Start Date", "End Date", "Response Type", "Progress", "Duration (in seconds)",
                    "Finished", "Recorded Date", "Response ID", "Distribution Channel", "User Language")

# vector of variable names associated with cols automatically outputted by Qualtrics
qual_vars <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)",
               "Finished", "RecordedDate", "ResponseId", "DistributionChannel", "UserLanguage")

# vector of topic columns - not sure what these are but they seem pointless
topic_cols <- "Q22_89_TEXT"
###############
# check header ####
###############

# copy of header to check
test <- header

# remove all qual_vars if row 1 contains them this makes checking easier
if(sum(qual_vars %in% test[1, 1:10]) == 10){
  test <- test[, -c(1:10)]
} else {
  stop("header doesn't contain qual_vars")
}

# question names were not set in qualtrics  >
# so without qual_text, test col names should all contain question numbers in the format Q[1-2 digit number] >
# warn user if this isn't true

if(sum(grepl("Q[[:digit:]]{1,2}", test[1, ])) != ncol(test)) stop("row 1 contains more than question numbers")

# row 2 should contain full question text >
# KD added variable names to all question text by putting "[variable]." at start of all questions >
# so values in row 2 (except those that were 'matrix' questions) should have this format >
# warn user if row 2 contains very few values that start "^(\w+)\."

if(sum(grepl("^\\w+\\.", test[2,])) < 10) stop("row 2 doesn't contain variable names")

# row 2 should contain import ids

if(sum(grepl("ImportId", test[3,])) != ncol(test)) stop("check header row 2 contains import ids")

################
# extract "variable." variables ####
############

# if passed all checks in check header section create dataframe to export as data dictionary >
# extract variable names contained in row 2 and match them to their full question text in row 2 >
# the extracted variable names can then be used as a header

# dict dataframe by creating column of questions, unname so column names won't include labels
qs <- as.data.frame(t(unname(header[2, ])), stringsAsFactors = F)
row.names(qs) <- c()

if(ncol(qs) == 1){
  colnames(qs)[1] <- "question"
}

# extract variables from questions that start "[variable]."

var_dot <- grep("^\\w+\\.", qs$question, value = T)

# find substrings that match "[variable]." format for questions in var_dot 

match <- find_matches("^\\w+\\.",var_dot)

# extract match and original string for var_dot_names

var_dot_names <- extract_var(match, var_dot)

# column with "strobe. ... - Text" in question text is the text response for "other reporting guidelines" option in strobe col 

var_dot_names$variable[grep("strobe\\..*- Text", var_dot_names$question)] <- "other_guidelines"

# check no duplicates

if(sum(duplicated(var_dot_names$variable)) != 0){
  stop("var_dot_names$variable contains ", sum(duplicated(var_dot_names$variable)), " duplicates")
}


##########################
# extract strobe variables ####
###########################

# 'matrix' strobe item questions all have the format "[matrix question text] - [strobe item number]. [strobe item name]" >
# the strobe item number is the variable name >
# find strobe item questions by those that contain a strobe item number

var_strobe <- grep("\\- [[:digit:]]{1,}.*\\.", qs$question, value = T)

# find matches for var_strobe

match <- find_matches("\\- [[:digit:]]{1,}.*\\.",var_strobe)

var_strobe_names <- extract_var(match, var_strobe)

# R adds X automatically to header values starting with a number >
# add X to start of colnames starting with number, this will mean X isn't added later on when cleaned csv is read in

var_strobe_names$variable <- paste("X", var_strobe_names$variable, sep = "")


#########################
# create data dictionary ####
########################

# extract_var extracts variable names embedded from a string and puts them in a row with their original string >
# combine these data frames to create a data dictonary

dict <- rbind(var_strobe_names, var_dot_names)

# extract data frame of qual columns
#  first 10 values of header row 1 contains variable names of qualtrics columns >
# first 10 values of header row 2 contains the full qualtrics column names >
# combine both to create dataframe for data dictonary

qual_dict <- data.frame(variable = as.character(header[1, 1:10]), 
                        question = as.character(header[2, 1:10]), stringsAsFactors = F)

# topic col dict

topic_dict <- data.frame(variable = grep("Q22_89_TEXT", header[1, ], value = T), 
                         question = grep("Q22_89_TEXT", header[2, ], value = T), stringsAsFactors = F)


# clean variable names by removing any extra text after dot
dict$variable <- gsub("\\..*","",dict$variable) %>%
  # remove backslashes added for matching plus spaces and hyphens
  gsub("\\\\|\\-| ", "", .)

# combine dict and qual_dict if both column names are same
if(identical(sort(colnames(dict)), sort(colnames(qual_dict)))){
  dict <- rbind(dict, qual_dict)
}

# combine dict and topic_dict if column names are identical

if(identical(sort(colnames(dict)), sort(colnames(topic_dict)))){
  dict <- rbind(dict, topic_dict)
}

#############################
# find strobe evidence variables ####
###########################

# append ev to all variable names of strobe evidence variables >
# These are strobe variable with "- Text" in the question text) >
# non-strobe variables containing Text may be text response options for something that isn't evidence >
# e.g. name of other reporting guidelines used

# create ev column 
dict$ev <- rep(NA)

# find strobe evidence variables
dict$ev[grepl("^X[[:digit:]]{1,2}", dict$variable)] <- grepl("\\- Text$", dict$question[grepl("^X[[:digit:]]{1,2}", dict$variable)])

# check created
if(all(is.na(dict$ev))) stop("ev cols not found")

# combine ev and variable cols
dict$variable <- paste(dict$variable, dict$ev, sep = "_") %>%
  # remove false and NA from variables names that aren't strobe evidence variables
  gsub("\\_FALSE|\\_NA", "", .) %>%
  # append ev to all strobe evidence variable names
  gsub("\\_TRUE", "_ev", .)

# drop ev col as this has now been used to append variables
dict$ev <- NULL

##########
# check ####
##########

# dict$question should now be identical to row 2 in header  >
# check this is true >

if(identical(sort(as.character(dict$question)), sort(as.character(header[2, ]))) == F) stop("not identical")

# check all strobe items are in dictionary by importing manually created dictionary of strobe variables

strobe_var <- read.csv("qualtrics_forms/strobe_adapted_items.csv", encoding = "UTF-8",stringsAsFactors = F)

if(sum(paste("X", strobe_var$Adapted.item.no, sep = "") %in% dict$variable) != length(strobe_var$Adapted.item.no)) stop("incorrect number of strobe items")


########
# merge ####
#########

# need to create key for each variable name so can replace header when read in data extraction csv >
# read in data extraction csv without a header so the colnames will be sequential variable numbers

header_real <- read.csv(input, stringsAsFactors = F, encoding = "UTF-8", header = F, nrows = 3)
  
# transpose header so all rows become character vectors
t_header <- as.data.frame(t(header_real), stringsAsFactors = F, row.names = F) 
# add header back in as this is removed during the transposition
t_header$import_num <- colnames(header_real) %>%
  # remove "V" so can be ordered easily
  gsub("V", "", .)%>%
  # set to numeric
  as.numeric(.)

# join

if(nrow(anti_join(dict, t_header, by = c("question"= "V2"))) == 0){
  dict_final <- full_join(dict, t_header, by = c("question"= "V2"))
} else {
  stop("some values won't join")
}

# drop import id col

dict_final$V3 <- NULL

# rename row 1 as qual_q_num indicating these are the variable names qualtrics exports
colnames(dict_final)[colnames(dict_final) == "V1"] <- "qual_var_name"

# sort by import number

dict_final <- dict_final[order(dict_final$import_num), ]

# check colnames import_nums are sequential

if(all(abs(diff(dict_final$import_num)) != 1)) stop("import_num not sequential")
if(all(abs(diff(dict_final$import_num[grep("X[[:digit:]]{1,2}", dict_final$variable)])) != 1)) stop("strobe variables' import_num not sequential ")

#######
# export ####
#########

if(sum(duplicated(dict_final$variable)) == 0){
  write.csv(dict_final, "outputs/extraction_dictionary.csv", row.names = F, fileEncoding = "UTF-8")
} else {
  stop("duplicated variables")
}
