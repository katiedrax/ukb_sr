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

df <- read.csv(input, encoding = "UTF-8", stringsAsFactors = F, header = F, na.strings = "")


###############
# check format ####
###############

# create function to check correctly exported

check_format <- function(qualtrics_df, anon = logical(), qs_named = logical()){
  # Qualtrics exports 3 header rows >
  # row 1 = "internal Qualtrics ID" of each field, i.e. question numbers >
  # row 2 = "field name", i.e. question text >
  # row 3 = import ids
  # observations start on row 4
  #create copy of dataframe
  df_copy <- qualtrics_df
  # save vector of qualtrics metadata
  if(anon == T){
    qual_vars <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)",
                   "Finished", "RecordedDate", "ResponseId", "DistributionChannel", "UserLanguage")
  } else {
    qual_vars <-c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)",
                  "Finished", "RecordedDate", "ResponseId", "DistributionChannel", "UserLanguage", 
                  "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalDataReference", "LocationLatitude", "LocationLongitude")
  }
  # check qs not named
  if(qs_named == T){
    stop("function can't clean dataframes with named questions")
  }  else {
    # save row 1 as vector for checking
    row1 <- as.character(df_copy[1, ])
  }
  # if question not named (and just automatically numbered) then row 1 should contain qualtrics metadata names and question numbers (Q[1-2 digit number])
  if(all(qual_vars %in% row1) == F) stop("row 1 missing some qualtrics metadata variables")
  # check just contains question numbers without qualtrics vars
  row1_no_qual <- row1[row1 %in% qual_vars == F]
  if(!identical(sum(grepl("Q[[:digit:]]{1,}", row1_no_qual)), length(row1_no_qual))){
    stop("row 1 contains more than question numbers")
  } else {
    # rename rowname for checking later
    rownames(df_copy)[1] <- "q_num"
  }
  # row 2 should contain text, none should be empty
  row2 <- as.character(df_copy[2,])
  if(any(identical(nchar(row2), 0))) stop("some empty values in row 2")
  rownames(df_copy)[2] <- "q_text"
  # row 3 should contain import ids
  row3 <- as.character(df_copy[3, ])
  if(!identical(sum(grepl("ImportId",  row3)), ncol(df_copy))){
    stop("check df row 2 contains import ids")
  } else {
    rownames(df_copy)[3] <- "import_id"
  }
  # qualtrics sometimes exports "Topic" columns, save vector if present
  topic_col <- grep("topic", as.character(df_copy[2,]), ignore.case = T)
  # check if topic_col empty
  if(identical(length(topic_col), 0)){
    # if is empty tell user
    message("dataframe contains no topic columns")
  } else {
    # if are topic cols check they are empty besides the first 3 header row values
    for(i in topic_col){
      if(sum(!is.na(df_copy[, i])) != 3){
        message("col ",i, " is a topic column and contains respondent data")
        }else {
          # tell user if df contains empty topic cols
          message("col ", i, " is an empty topic column")
        }
    }
  }
  return(df_copy)
}


# drop random topic cols if both empty besides first three rows

df <- check_format(df, anon=T, qs_named = F)

# row 2 should contain full question text >
# KD added variable names to all question text by putting "[variable]." at start of all questions >
# so values in row 2 (except those that were 'matrix' questions) should have this format >
# warn user if row 2 contains very few values that start "^(\w+)\."

if(sum(grepl("^\\w+\\.", df[2,])) < ncol(df) * 0.05) stop("very few values in row 2 contain variable names")


