# added year column to all journals 
library(dplyr)

add_year <- function(file, pattern, year){
  source <- read.csv(file, stringsAsFactors = F, header = F, nrows = 1, na.strings = c(" ", "")) %>%
    .[1, 1]
  x <- read.csv(file, stringsAsFactors = F, skip = 1, na.strings = c(" ", ""))
  if(grepl(pattern, source) & is.data.frame(x)){
    x$year <- rep(year, nrow(x))
    } else {
      stop("not dataframe")
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

all <- rbind(jif12, jif13, jif14, jif15, jif16, jif17, jif18)
all$Full.Journal.Title <- tolower(all$Full.Journal.Title)
all$JCR.Abbreviated.Title <- tolower(all$JCR.Abbreviated.Title)

first <- read.csv("outputs/first_80.csv") %>%
  select(., c("journal", "year"))%>%
  .[!duplicated(.),]

journals <- left_join(journals, all, by = c("journal" = "JCR.Abbreviated.Title", "year"))
