# libraries ####

library(dplyr)
library(stringr)

# import####

# create header vector from first row in data
header <- read.csv(file = "data/Classifying+epi+study+designs_4+December+2019_00.29.csv", encoding = "UTF-8")
header <- colnames(header)

# qualtrics automatically exports "import id"s in row 2 and full question text in row 3 >
# import data skipping first three rows to ensure rows 2 and 3 aren't imported by that row 1 does become header

df <- read.csv("data/Classifying+epi+study+designs_4+December+2019_00.29.csv", skip = 3,
               encoding = "UTF-8", header = F, col.names = header, na.strings = c("", " "), stringsAsFactors = F)

# remove qual cols ####

# vector of cols automatically outputted by qualtrics (always first 17 cols)

qual_cols <- colnames(df)[1:17]

# remove qual_cols

df <- select(df, -qual_cols)

#sort by article id

df <- df[order(df$article_id), ]

# paste design judgements into design

df$design <- paste(df$design, df$design_judg, sep = ",")

# drop design_judg col

df$design_judg <- NULL

# remove NAs added by paste 
df$design[df$design == "NA,NA"] <- NA
df$design <- gsub("no_statement,|NA,|,NA", "", df$design)

df$design[grep("cross-sectional,cohort", df$design)] <- "cohort,cross-sectional"

# add logical values for designs

#df$design_cc <- grepl("case-control", df$design) | grepl("case-control", df$design_judg)
#df$design_x <- grepl("cross-sectional", df$design) | grepl("cross-sectional", df$design_judg)
#df$design_coh <- grepl("cohort", df$design) | grepl("cohort", df$design_judg)
#df$design_other <- grepl("other", df$design_judg)


# clean intitials

df$initials <- tolower(df$initials)
df$initials <- gsub("[[:punct:]]", "", df$initials)

# find conflicts ####

kd <- df[df$initials == "kd", ]
mg <- df[df$initials == "mg", ]

both <- full_join(kd, mg, by = "article_id", suffix = c(".kd", ".mg"))

conflict <- both[which(both$design.kd != both$design.mg),]%>%
  select(c(design.kd, design.mg), everything())

no_conflict <- both[which(both$design.kd == both$design.mg),]%>%
  select(c(design.kd, design.mg), everything())


# export ####

write.csv(df, "outputs/clean_designs.csv", row.names = F)
