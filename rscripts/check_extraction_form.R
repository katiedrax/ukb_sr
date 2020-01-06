# Description ####

# Imports dataset containing test responses to the Data Extraction Form >
# Exports csv containing article's designs and all design specific cols >
# Manually check export that design specific cols are empty for article's not containing that design

library(dplyr)

# skip question name row so question row will be header
df <- read.csv("qualtrics_forms/Data+Extraction+Form_6+January+2020_17.10.csv", 
               skip = 1, encoding = "UTF-8", stringsAsFactors = F)


# vector of columns qualtrics automatically exports (always first 10 if response autonomised)
qual_cols <- colnames(df)[1:10]

# remove qual_cols

df <- select(df, -qual_cols)

# find design specific cols

design_qs <- grepl("sectional|case.control|cohort", colnames(df), ignore.case = T)

# variable name is in question text so remove question text from colnames that is not the name
colnames(df) <- tolower(colnames(df))
num <- grepl("\\d", colnames(df))
colnames(df)[num] <- gsub("^[^\\d]+", "", colnames(df)[num], perl = T)
colnames(df) <- gsub("(?<=\\.)(.*?)(?=text)", "", colnames(df), perl = T)
colnames(df) <- gsub("\\.\\..*$", "", colnames(df), perl = T)


# remove row automatically outputted by qualtrics
id <- grep("import", df$article_id, ignore.case = T)
df <- df[-id, ]


#### remove cols not needed for design specific test ####

# remove "Text" cols
text <- grep(".Text", colnames(df))

test_df <- df[, -text]

# remove cols that aren't design specific

test_df <- test_df[, design_qs]

test_df <- select(test_df, test_df$, everything())

# cols for cohort & cross-sectional designs ####

# check df cols only contain questions for cohort and cross-sectional and no other two designs
grep("^(?=.*cohort)(?=.*cross).*$", colnames(df), ignore.case = T, perl = T)
grep("^(?=.*cohort)(?=.*case).*$", colnames(df), ignore.case = T, perl = T)
grep("^(?=.*cross)(?=.*case).*$", colnames(df), ignore.case = T, perl = T)

# select columns for cross-sectional and cohort designs

coh_x <- grep("^(?=.*cohort)(?=.*cross).*$", colnames(df), ignore.case = T, perl = T)
x <- grep("sectional", colnames(df)[-coh_x], ignore.case = T)
cc <- grep("control", colnames(df), ignore.case = T)
coh <- grep("cohort", colnames(df)[-coh_x], ignore.case = T)




