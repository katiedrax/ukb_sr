# Description

# This code cleans the csv containing all included refereneces from Rayyan

#########
#### packages####
#########

library(tidyverse)

###################
#### import included refs ####
###################

#read in refs

df <- read.csv("data/04_csv.csv", header = F, stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", "NA", " "))

##############
#### check rows ####
#############

# check number of rows is 564

if (nrow(df) != 564){
  stop("wrong num of refs")
} else {
  print("564 included refs")
}

# remove empty rows

df <- df[rowSums(is.na(df)) != ncol(df),]

# check number of rows == 564 again

if (nrow(df) != 564){
  stop("wrong num of refs")
} else {
  print("564 included refs")
}

# remove empty cols so headers will be added correctly

all_na <- sapply(df, function(x) all(is.na(x)))

df <- df[!all_na]

############
#### headers ####
###########

# create header vector

headers <- c("authors", "year", "title", "journal", "volume", "issue",  "pages", "start_page", 
             "epub_date", "date", "title_dup", "alternate_journal", "issn", "legal_note", 
             "pmcid", "custom_3",  "accession_num", "keywords",  "abstract",  "notes", "url", 
             "author_address", "no_head", "doi")

# check there are enough headers

if(length(headers) != ncol(df)){
  stop("too few/many headers")
} else {
  print("enough headers for cols")
}

# reset col numbers

colnames(df) <- headers
colnames(df)

################
#### clean cols ####
###############

# check contents of col without endnote header

df[, "no_head"]

# if col with no header contains a doi add contents it's to doi col

df$doi <- coalesce(df$doi, df$no_head)

# remove unneeded cols
df <- df %>% select(-c("start_page", "epub_date", "date", "title_dup", "alternate_journal", 
                    "issn", "keywords", "notes", "no_head", "author_address")) %>%
  # rename col
  rename(class_manual = custom_3)

# check colnames again

colnames(df)

##############
#### check dois ####
###############

# check all dois contain 10.

df$doi[!grepl("^10", df$doi)]

# check if any dois have anything extra coming before doi

df$doi[!grepl("^10", df$doi)]

# check all rows have a unique doi

if ((length(unique(na.omit(df$doi))) == nrow(df)) == FALSE){
  stop("dois wrong/missing")
} else {
  print("dois OK")
}

# check if any dois are missing

no_doi <- df[is.na(df$doi), c("title", "authors")]

# add missing doi

df$doi[df$title == "Analysis of shared heritability in common disorders of the brain"] <- "10.1126/science.aap8757"

# check all rows have a unique doi again

if ((length(unique(na.omit(df$doi))) == nrow(df)) == FALSE){
  stop("dois wrong/missing")
} else {
  print("dois OK")
}


######################
#### extract classifications ####
#########################

# id which values contain two rayyan labels

df$legal_note <- ifelse(grepl("RAYYAN-LABELS.*RAYYAN-LABELS", df$legal_note), 
                        gsub("RAYYAN-LABELS:", "2labels", df$legal_note), 
                        gsub("RAYYAN-LABELS:", "1labels", df$legal_note))

# indicate which values contain two exclusion labels

df$legal_note <- ifelse(grepl("RAYYAN-EXCLUSION-REASONS.*RAYYAN-EXCLUSION-REASONS", df$legal_note), 
                        gsub("RAYYAN-EXCLUSION-REASONS:", "2excl", df$legal_note), 
                        gsub("RAYYAN-EXCLUSION-REASONS:", "1excl", df$legal_note))


# separate cols with two exclusion reasons 

nmax <- max(str_count(df$legal_note, "2excl") + 1, na.rm = TRUE)

df <- separate(df, legal_note, into = paste0("excl", seq_len(nmax)), sep = "2excl", fill = "right", remove = F)

# separate cols with one exclusion reason

nmax <- max(str_count(df$excl1, "1excl") + 1, na.rm = TRUE)

df <- separate(df, excl1, into = paste0("exc", seq_len(nmax)), sep = "1excl", fill = "right")

# drop col containing exclusion reasons from screening

df$excl2 <- NULL

# coalese classifications

df$exc2 <- coalesce(df$exc2, df$excl3)

# drop coalesced col

df$excl3 <- NULL

# rename cols

df <- rename(df, rayyan = exc1, class = exc2)

# separate cols with two labels

nmax <- max(str_count(df$rayyan, "2labels") + 1, na.rm = TRUE)

df <- separate(df, rayyan, into = paste0("lab", seq_len(nmax)), sep = "2labels", fill = "right")

# drop col containing labels from screening

df$lab2 <- NULL

# coalesce lab 3 into class

df$class <- coalesce(df$class,df$lab3)

# drop coalesced col

df$lab3 <- NULL

# rename column for ease

df <- rename(df, rayyan = lab1)

# separate cols with one label

nmax <- max(str_count(df$rayyan, "1labels") + 1, na.rm = TRUE)

df <- separate(df, rayyan, into = paste0("labb", seq_len(nmax)), sep = "1labels", fill = "right")

# coalesce labels into class

df$class <- coalesce(df$class, df$labb2) %>%
  # trim whitespace
  trimws(which = "both")

# drop other rayyan info produced by separations

df$labb1 <- NULL
df$labb2 <- NULL


########################
#### check classifications ####
##################

# check class doesn't contain anything other than classification labels

if(length(grep("\\{ |= | RAYYAN", df$class)) > 0){
  stop("class contains non-classes")
}

# replace incorrect class value

df$class[grep("2labels mr", df$class)] <- "mr"


# check class doesn't contain anything other than classification labels again

if(length(grep("\\{ |= | RAYYAN", df$class)) > 0){
  stop("class contains non-classes")
}

table(df$class)

#rename legal note

df <- rename(df, rayyan = legal_note)

# subsitute & for _

df$class <- gsub(" & ", "_", df$class)

# remove machine learning from class as this was not definied as an 'other' classification

df$class <- gsub(",machine learning", "", df$class)


# extrac class_manual (assigned manually to separate exports) & class (extracted in code above from rayyan metadata)

class_df <- df[, c("class", "class_manual")]


# check any differences

if(length(class_df[which(class_df$class != class_df$class_manual),]) > 0){
  stop(class_df[which(class_df$class != class_df$class_manual),])
} else (
  print("no diffs in classifications")
)

# drop class_manual if same as class

df$class_manual <- NULL

# check correct number of classifications

if((sum(df$class == "descriptive") == 6) &
   (sum(df$class == "epi") == 178) &
   (sum(df$class =="imaging") == 50) &
   (sum(df$class =="method") == 6) &
   (sum(df$class =="genetic") == 225) &
   (sum(df$class =="mr") == 70) &
   (sum(df$class =="genetic_imaging") ==15) &
   (sum(df$class =="mr_epi") == 2)  &
   (sum(df$class =="method_imaging") == 2) &
   (sum(df$class =="genetic_imaging_method") ==1) &
   (sum(df$class =="nepi") == 9)){
  print("correct num of classifications")
} else {
  stop("classifications wrong")
}

###################
#### separate url ####
####################

# drop any extra urls (/// was used to replace carriage returns when exporting csv from endnote)

df <- separate(df, url, into = "url", sep = "///", fill = "right", extra = "drop")

############################
#### create article id ####
############################

# extract first string after a "." in authors
first <- gsub("\\,.*","", df$authors)

# remove everything that comes before first "." in "first"
first <- gsub("^.*\\.","", first)

# remove spaces from first name
first <- gsub(" ", "", first)

first <- first %>%
  #remove whitespace
  trimws(which = "both") %>%
  #extract first 5 characters from first author's last name
  substr(1, 5)

# extract last 3 characters of doi
do <- substr(df$doi, nchar(df$doi)-4+1, nchar(df$doi))

# extract first 3 characters of title
ti <- substr(df$title, 1, 3)

# paste it all together

id <- paste(first, df$year, ti, do, sep = "")

# assign article id if unique

if((length(unique(na.omit(id))) == nrow(df)) == TRUE){
  df$id <- id
} else {
  stop("article ids not unique")
}

#########
#### export ####
#########

write.csv(df, "outputs/csv_clean.csv", row.names = F, fileEncoding = "UTF-8")