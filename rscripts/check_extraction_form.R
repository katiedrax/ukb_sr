# Description ####

# Imports dataset containing test responses to the Data Extraction Form >
# Exports csv containing article's designs and all design specific cols >
# Manually check export that design specific cols are empty for article's not containing that design

library(dplyr)

# skip question name row so question row will be header
df <- read.csv("data/Data+Extraction+Form_2+December+2019_17.58.csv", skip = 1)

# save column names from raw df

df_names <- colnames(df)

#### remove unnecessary cols ####

# vector of columns qualtrics automatically exports
qual_cols <- colnames(df)[1:17]

# remove qual_cols

df <- select(df, -qual_cols)

#### remove rows ####

# remove row automatically outputted by qualtrics
id <- grep("import", df$Article.ID...copy.and.paste.in.the.article.ID.from.the.csv.file..close.Excel, ignore.case = T)
df <- df[-1, ]

# remove real responses 

#### separate out strobe it
id <- grep("^[a-zA-Z]{0,6}$", df$Your.initials)
df <- df[-id, ]

#### remove cols not needed for test ####

# remove "Text" cols
text <- grep("Text", colnames(df))

test_df <- df[, -text]

# remove cols that aren't design specific
design_qs <- grep("sectional|case.control|cohort", colnames(test_df), ignore.case = T)

test_df <- test_df[, design_qs]

test_df$designs <- df$What.study.designs.do.the.authors.use.to.analyse.the.UK.Biobank.data.

test_df <- select(test_df, designs, everything())

write.csv(test_df, "outputs/test_design_cols.csv")

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

