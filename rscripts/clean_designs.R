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
               encoding = "UTF-8", header = F, col.names = header, na.strings = c("", " "))

# remove qual cols ####

# vector of cols automatically outputted by qualtrics (always first 17 cols)

qual_cols <- colnames(df)[1:17]

# remove qual_cols

df <- select(df, -qual_cols)
