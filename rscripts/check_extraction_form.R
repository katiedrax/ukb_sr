# Description ####

# Imports dataset containing test responses to the Data Extraction Form >
# Exports csv containing article's designs and all design specific cols >
# Manually check export that design specific cols are empty for article's not containing that design

library(dplyr)

# skip question name row so question row will be header
df <- read.csv("qualtrics_forms/Data+Extraction+Form_6+January+2020_17.10.csv", 
               skip = 1, encoding = "UTF-8", stringsAsFactors = F, na.strings = c("", " "))


# vector of columns qualtrics automatically exports (always first 10 if response autonomised)
qual_cols <- colnames(df)[1:10]

# rename article id

colnames(df)[grep("article_id", colnames(df))] <- "article_id"
colnames(df)[grep("^designs", colnames(df))] <- "designs"
# remove qual_cols

df <- select(df, -qual_cols)

# find design specific cols

design_qs <- grepl("sectional|case.control|cohort", colnames(df), ignore.case = T)

# remove row automatically outputted by qualtrics
id <- grep("import", df$article_id, ignore.case = T)
df <- df[-id, ]

#### remove cols not needed for design specific test ####

# remove "Text" cols
text <- grep("text", colnames(df), ignore.case = T)

# remove cols that aren't design specific

test_df <- df[, design_qs]

# cols for cohort & cross-sectional designs ####

# check  cols only contain questions for cohort and cross-sectional and no other two designs
length(grep("^(?=.*_coh)(?=.*_cs).*$", colnames(df), ignore.case = T, perl = T) >0)
length(grep("^(?=.*_coh)(?=.*_cc).*$", colnames(df), ignore.case = T, perl = T) >0)
length(grep("^(?=.*_cs)(?=.*_cc).*$", colnames(df), ignore.case = T, perl = T) >0)

# select columns for cross-sectional and cohort designs

coh_cs <- colnames(df)[grep("^(?=.*_coh)(?=.*_cs).*$", colnames(df), ignore.case = T, perl = T)]
cs <- grep("_cs", colnames(df)[-coh_x], ignore.case = T, value =T)
cc <- grep("_cc", colnames(df), ignore.case = T, value =T)
coh <- grep("_coh", colnames(df)[-coh_x], ignore.case = T, value = T)

coh_cs_ful <- colnames(df)[grep("^(?=.*cohort)(?=.*cross).*$", colnames(df), ignore.case = T, perl = T)]
cs_ful <- grep("cross", colnames(df)[-coh_x], ignore.case = T, value =T)
cc_ful <- grep("case", colnames(df), ignore.case = T, value =T)
coh_ful <- grep("cohort", colnames(df)[-coh_x], ignore.case = T, value = T)

all.equal(sort(coh_cs), sort(coh_cs_ful))
all.equal(sort(cs), sort(cs_ful))
all.equal(sort(cc), sort(cc_ful))
all.equal(sort(coh), sort(coh_ful))

coh_art <- df[df$designs == "Cohort", ]
cs_art <- df[df$designs == "Cross-sectional", ]
cc_art <- df[df$designs == "Case-control",]

a <- sapply(coh_art, function(x) all(is.na(x)))

responses <- c("Yes", "No", "Partially", "NA", "Partially-External", "Unsure")

sapply(coh_art, function(x) setdiff(na.omit(x), responses))

all(is.na(x))
is.na(c(df$article_id..Copy.and.paste.in.the.article.ID.from.the.csv.file..do.not.put.in.quotes., df$title..Copy.and.paste.the.article.s.title.from.the.online.version.of.the.article..do.not.put.in.quotes.))

# variable name is in question text so remove question text from colnames that is not the name
colnames(df) <- tolower(colnames(df))
num <- grepl("\\d", colnames(df))
colnames(df)[num] <- gsub("^[^\\d]+", "", colnames(df)[num], perl = T)
colnames(df) <- gsub("(?<=\\.)(.*?)(?=text)", "", colnames(df), perl = T)
colnames(df) <- gsub("\\.\\..*$", "", colnames(df), perl = T)


