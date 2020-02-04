# function to clean string 
clean_string <- function(string){
  # string: a vector of strings
  # remove any non-english character, numbers, spaces or punctuation and lower
  string <- gsub("[^\u0001-\u007F]+","", string)
  string <- gsub("[0-9]", "", string)
  string <- gsub("[[:punct:]]", "", string)
  string <- tolower(string)
  string <- gsub("[[:space:]]", "", string)
}
