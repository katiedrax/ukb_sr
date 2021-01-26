#############
# libraries ####
#############

library(magrittr)
library(stringr)

##########
# import####
##########

# assign input file 

input <- "data/data_extraction_form.csv"

# Import data
# read in "" as NA to avoid recognising Not Applicable (NA) values as missing
df <- read.csv(input, encoding = "UTF-8", stringsAsFactors = F, header = F, na.strings = "")

##############
# drop qualtric cols I don't want ####
################

# assign rownames so can merge qualtrics cols back in if necessary
rownames(df) <- seq(1:nrow(df))

# vector of question text associated with cols automatically outputted by Qualtrics 

qualtric_text <- c("Start Date", "End Date", "Response Type", "Progress", "Duration (in seconds)",
               "Finished", "Recorded Date", "Response ID", "Distribution Channel", "User Language")

# vector of my variable names for qualtrics metadata columns
qualtric_vars <- gsub("[[:punct:]]", "", qualtric_text) %>%
  gsub("[[:space:]]", "_", .) %>%
  tolower()

# row 2 should contain question text and qualtrics metadata should be in first 10 columns >
# drop all qualtric_text if in first 10

if(identical(qualtric_text, as.character(df[2, 1:10]))){
  qual_df <- df
  colnames(qual_df)[1:10] <- qualtric_vars
  id <- grep("^article_id", qual_df[2, ])
  colnames(qual_df)[id] <- "article_id"
  df <- df[, -c(1:10)]
  qual_df[, colnames(qual_df)]
  vital_cols <- c("article_id", qualtric_vars)
  if(all(vital_cols %in% colnames(qual_df)) == F) stop("qualtric_vars and article_id not all in vital_cols")
  qual_df <- qual_df[, colnames(qual_df) %in% vital_cols]
} else {
  stop("df doesn't contain qualtric_vars")
}

###############
# check first 3 rows ####
###############

# question names were not set in qualtrics  >
# so without qualtric_text, df col names should all contain question numbers in the format Q[1-2 digit number] >
# warn user if this isn't true

if(sum(grepl("Q[[:digit:]]{1,2}", df[1, ])) != ncol(df)){
  stop("row 1 contains more than question numbers")
} else {
  rownames(df)[1] <- "q_num"
}

# row 2 should contain full question text >
# KD added variable names to all question text by putting "[variable]." at start of all questions >
# so values in row 2 (except those that were 'matrix' questions) should have this format >
# warn user if row 2 contains very few values that start "^(\w+)\."

if(sum(grepl("^\\w+\\.", df[2,])) < 10){
  stop("row 2 doesn't contain variable names")
} else {
  rownames(df)[2] <- "q_text"
}

# row 3 should contain import ids

if(sum(grepl("ImportId", df[3,])) != ncol(df)){
  stop("check df row 2 contains import ids")
} else {
  rownames(df)[3] <- "import_id"
}

###############
#drop topic cols ####
#########
# not sure what these are but they seem pointless Qualtrics output since they are empty so >
# find topic cols, check they are empty and remove them if they are
# create vector of topic columns

topic_col <- grep("topic", df[2,], ignore.case = T)

# check all topic cols empty besides first three rows containing q_num, q_text and 

for(i in topic_col){
  if(sum(!is.na(df[, i])) != 3) stop(i, "has more than 3 values")
}


# drop random topic cols if both empty besides first three rows

df <- df[, -topic_col]


################
# extract variable names ####
############
# I added variable names to the start of all questions so the variable name for each question is in q_text >
# we presented strobe items as a 'matrix' in qualtrics so all have the format "[matrix question text] - [strobe variable]. [strobe item name]" >
# I put the variable name for non-strobe items at the start of the question so "[variable].">
# find strobe and non-strobe cols

strb_cols <- grep("\\- [[:digit:]]{1,}.*\\.", df["q_text", ])
var_dot <- grep("^\\w+\\.", df["q_text", ])

# extract strobe and non-strobe variables from q_text, do not simplify so list is same length as ncol(df) when unlist

var <- str_extract_all(df["q_text", ], "^\\w+\\.|\\- [[:digit:]]{1,}.*\\.", simplify = F)

# unlist

if(length(var) == ncol(df)){
  # if same length unlist var but replace all NULL characters with NA so will be kept 
  var <- unlist(lapply(var,function(x) if(identical(x,character(0))) NA else x))
} else {
  stop("var and df are different lengths")
}

# remove everything before digit or after .

var <- gsub("\\..*|- ", "", var)

# check all strobe col variables start with a digit

if(all(grepl("^[[:digit:]]", var[strb_cols]) == F)) stop("not all strobe start with digit")

if(identical(length(var), ncol(df))){
  colnames(df) <- var
} else {
  stop("wrong length")
}

# check column's name is in it's q_text value

a <- 1:length(var)

for(i in 1:length(var)){
  # extract and save q_text value if contains colname
  a[i] <- grep(colnames(df)[i], df["q_text", i], value = T)
}

# if all q_text values contain colnames then a should be identical to the q_text row, check this
if(identical(a, as.character(df["q_text", ])) == F) stop("var wrong")

##################
# edit header ####
###################

# affix all strobe variables with "s" to show they are strobe

colnames(df)[strb_cols] <- paste("s", colnames(df)[strb_cols], sep = "")

# Qualtrics allows you to add text boxes to multiple-choice questions >
# text boxes for STROBE items were for evidence >
# it indicates this by appending "_TEXT" to the question number or "- Text" to the question text >
# find cols of question numbers containing _TEXT so can append 

text_col <- grep("_TEXT", df["q_num",])

# append text to colnames(df) elements

colnames(df)[text_col] <- paste(colnames(df)[text_col], "_ev", sep = "")

# there should be two strobe_ev variables now >
# "strobe_ev." q_text gives evidence for use of strobe, "strobe. ... - Text" q_text details other reporting guidelines

if(sum(colnames(df) == "strobe_ev" ) == 2){
  # if are dups get q_text col containing "strobe. ... - Text"
  other_col <- grep("strobe\\..*- Text", df["q_text",])
  if(length(other_col) != 1) stop("wrong cols") 
  colnames(df)[other_col] <- "other_guidelines"
} else {
  stop("not dups")
}

#################
# make df dict and remove qualitrics rows ####
######################

# make df dictionary so list variables with original question text and the numbers qualtrics assigned them
dict <- data.frame(variable = colnames(df), question = as.character(df["q_text", ]),
                   qualtric_var = as.character(df["q_num", ]), stringsAsFactors = F)

# drop qualtrics rows

df <- df[!rownames(df) %in% c("q_num", "q_text", "import_id"), ]

# create cols for resolving conflicts


x <- paste0(colnames(df)[grepl("_ev$", colnames(df)) == F], "_correct")
correct_cols <- data.frame(matrix(ncol = length(x), nrow = nrow(df)))
colnames(correct_cols) <- x

#add in correct_cols

df <- cbind(df, correct_cols)

########################
# add qualtric variables in ####
##########################

# add qualtric_vars back in on rownames

qual_df$rownames <- rownames(qual_df)
df$rownames <- rownames(df)

mismatch <- dplyr::anti_join(qual_df, df,  by = c("rownames", "article_id"))

if(identical(as.character(c(1,2,3)), as.character(mismatch$rownames))){
  # add in qualtric_vars with
  df <- merge(df, qual_df, by = c("rownames", "article_id"))
  # remove merge col as no longer needed
  df$rownames <- NULL
}

#######
# export ####
#########

# order columns using mixedorder so strobe items keep numerical order

df <- df[ , gtools::mixedorder(colnames(df))]

# export df if passes checks

if(sum(!is.na(colnames(df))) == ncol(df)){
  # check Not applicable values are characters, check this in 7_iii strobe item which should contain lots of Not APplicables
  if("NA" %in% df$s7_iii == F) stop("not applicables aren't characters")
  # check no colnames start with number cause R doesn't like it
  if(sum(grepl("^[[:digit:]]", colnames(df))) != 0) stop("some colnames(df)s start with 0")
  # check no duplicates
  if(sum(duplicated(colnames(df))) != 0) stop("colnames(df) contains ", sum(duplicated(colnames(df))), " duplicates")
  # export NAs as blanks so don't confuse Not Applicable (NA) response
  write.csv(df, "outputs/clean_extraction_form.csv", row.names = F, fileEncoding = "UTF-8", na = "")
} else {
  stop("wrong length")
}

# export extraction dictionary
write.csv(dict, "outputs/extraction_dict.csv", row.names = F, fileEncoding = "UTF-8", na = "")
