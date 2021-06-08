temp <- list.files(path = "conflicts_resolution/extraction", pattern ="*.csv")
temp <- paste0("conflicts_resolution/extraction/", temp)
data_obs <- paste0("df", 1:length(temp))
for(i in 1:length(temp)){
  assign(data_obs[i], read.csv(temp[i], stringsAsFactors = F, encoding = "UTF-8", na.strings = c("", " "), header = T))
}
list <- list(df1=df1, df2=df2, df3=df3, df4=df4, df5=df5, df6=df6, df7=df7)

drop <- function(df){
  # drop empty cols
  df <- df[, colSums(is.na(df)) != nrow(df)]
  # drop empty rows
  df <- df[rowSums(is.na(df)) != ncol(df),]
  if(sum(grepl("^\\(w+)\\.", df[1,])) < 10){
  stop("row 1 doesn't contain variable names")
    } else {
  rownames(df)[1] <- "q_text"
    }
  return(df)
}

df1 <- drop(df1)
df2<- drop(df2)
df3 <- drop(df3)
df4 <-drop(df4)
df5 <-drop(df5)
df6 <-drop(df6)
df7 <-drop(df7)

####

# I added variable names to the start of all questions so the variable name for each question is in q_text >
# we presented strobe items as a 'matrix' in qualtrics so all have the format "[matrix question text] - [strobe variable]. [strobe item name]" >
# I put the variable name for non-strobe items at the start of the question so "[variable].">
# find strobe and non-strobe cols
x  <- function(df){
  orig <- df["q_text", ]
  strb_cols <- grep("\\- [[:digit:]]{1,}.*\\.", df["q_text", ])
  var_dot <- grep("^\\w+\\.", df["q_text", ])
}


# extract strobe and non-strobe variables from q_text, do not simplify so list is same length as ncol(df7) when unlist

var <- str_extract_all(df7["q_text", ], "^\\w+\\.|\\- [[:digit:]]{1,}.*\\.", simplify = F)

# unlist

if(length(var) == ncol(df7)){
  # if same length unlist var but replace all NULL characters with NA so will be kept 
  var <- unlist(lapply(var,function(x) if(identical(x,character(0))) NA else x))
} else {
  stop("var and df7 are different lengths")
}

# remove everything before digit or after .

var <- gsub("\\..*|- ", "", var)

# check all strobe col variables start with a digit

if(all(grepl("^[[:digit:]]", var[strb_cols]) == F)) stop("not all strobe start with digit")

if(identical(length(var), ncol(df7))){
  colnames(df7) <- var
} else {
  stop("wrong length")
}

# check column's name is in it's q_text value

a <- 1:length(var)

for(i in 1:length(var)){
  # extract and save q_text value if contains colname
  a[i] <- grep(colnames(df7)[i], df7["q_text", i], value = T)
}

# if all q_text values contain colnames then a should be identical to the q_text row, check this
if(identical(a, as.character(df7["q_text", ])) == F) stop("var wrong")

for(i in names(list)){
  write.csv(list[[i]], paste0(i,".csv"), row.names = F, na = c("", " "))
}

# find unresolved ####

# check first column is article id
all(sapply(list, function(x)names(x)[1]) == "article_id")

conflict_ids <- lapply(list, function(x) x[["article_id"]]) %>%
  unlist()

conflict_ids <- conflict_ids[!duplicated(conflict_ids)|!is.na(conflict_ids)]
names(conflict_ids) <- NULL

real <- read.csv("../../outputs/clean_extraction_form.csv", strip.white = T)

bw <- real[real$article_id == "Peter2018bank8507", ]
Peter2018bank8507
