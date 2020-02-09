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

input <- "data/data_extraction/Data+Extraction+Form_9+February+2020_11.33.csv"

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

if(sum(grepl("Q[1-9]{1,2}", test[1, ])) != ncol(test)) stop("row 1 contains more than question numbers")

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

##########################
# extract strobe variables ####
###########################

# 'matrix' strobe item questions all have the format "[matrix question text] - [strobe item number]. [strobe item name]" >
# the strobe item number is the variable name >
# find strobe item questions by those that contain a strobe item number

var_strobe <- grep("\\- [1-9]{1,}.*\\.", qs$question, value = T)

# find matches for var_strobe

match <- find_matches("\\- [1-9]{1,}.*\\.",var_strobe)

var_strobe_names <- extract_var(match, var_strobe)

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

# find evidence questions (these are questions containing the word - Text)

dict$ev <- grepl("\\- Text$", dict$question)

# append ev to all variables that indicate evidence questions

dict$variable <- paste(dict$variable, dict$ev, sep = "_") %>%
  gsub("\\_FALSE", "", .) %>%
  gsub("\\_TRUE", "_ev", .)

# drop ev col as this has now been used to append variables
dict$ev <- NULL

# dict$question should now be identical to row 2 in header  >
# check this is true >

if(identical(sort(as.character(dict$question)), sort(as.character(header[2, ]))) == F) stop("not identical")

########
# merge ####
#########

# need to create key for each variable name so can replace header when read in data extraction csv >
# read in data extraction csv without a header so the colnames will be sequential variable numbers

header_real <- read.csv(input, stringsAsFactors = F, encoding = "UTF-8", header = F, nrows = 3)
  
# transpose header so all rows become character vectors
t_header <- as.data.frame(t(header_real), stringsAsFactors = F, row.names = F) 
# add header back in as this is removed during the transposition
t_header$import_num <- colnames(header_real)

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
#######
# excel ####
#########

write.csv(dict_final, "outputs/extraction_dictionary.csv", row.names = F, fileEncoding = "UTF-8")
