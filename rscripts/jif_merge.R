# added year column to all journals 
library(dplyr)

add_year <- function(file, pattern, year){
  source <- read.csv(file, stringsAsFactors = F, header = F, nrows = 1, na.strings = c(" ", "")) %>%
    .[1, 1]
  x <- read.csv(file, stringsAsFactors = F, skip = 1, na.strings = c(" ", ""))
  if(is.data.frame(x) == F) stop("not dataframe")
  if(grepl(pattern, source)  && "Journal.Impact.Factor" %in% colnames(x)){
    colnames(x)[colnames(x) == "Journal.Impact.Factor"] <- paste("jif", year, sep = "")
  } else {
    stop("no jif")
  }
  x <- x[!duplicated(x),]
  if(length(grep("Copyright|you agree to the", x$Rank) != 2)){
    text <- grep("Copyright|you agree to the", x$Rank)
    x <- x[-text, ]
    } else {
      stop("no Rank values contain copyright and/or terms of use text")
      }
    return(x)
    }

jif12 <- add_year("data/jif/jif_2012.csv", "Year: 2012", 2012)
jif13 <- add_year("data/jif/jif_2013.csv", "Year: 2013", 2013)
jif14 <- add_year("data/jif/jif_2014.csv", "Year: 2014", 2014)
jif15 <- add_year("data/jif/jif_2015.csv", "Year: 2015", 2015)
jif16 <- add_year("data/jif/jif_2016.csv", "Year: 2016", 2016)
jif17 <- add_year("data/jif/jif_2017.csv", "Year: 2017", 2017)
jif18 <- add_year("data/jif/jif_2018.csv", "Year: 2018", 2018)


all <- full_join(jif12, jif13[ , c("Full.Journal.Title", "jif2013")], by = "Full.Journal.Title") %>%
  full_join(., jif14[ , c("Full.Journal.Title", "jif2014")], by = "Full.Journal.Title") %>%
  full_join(., jif15[ , c("Full.Journal.Title", "jif2015")], by = "Full.Journal.Title") %>%
  full_join(., jif16[ , c("Full.Journal.Title", "jif2016")], by = "Full.Journal.Title") %>%
  full_join(., jif17[ , c("Full.Journal.Title", "jif2017")], by = "Full.Journal.Title") %>%
  full_join(., jif18[ , c("Full.Journal.Title", "jif2018")], by = "Full.Journal.Title") %>%
  .[complete.cases(.), ]

########
# export ####
#########

write.csv(all, "data/jif/all_years.csv", fileEncoding = "UTF-8", row.names = F)

