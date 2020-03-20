# function to check conflicts
find_cons <- function(df,a,b){
  if(is.null(a)) stop("a doesn't exist")
  if(is.null(b)) stop("b doesn't exist")
  # a & b = two columns to be compared
  # df = dataframe columns are in
  # create df of katie's and Mark's columns that don't match and include NAs in matching
  x <- df[which((!is.na(a) & !is.na(b) & a==b | is.na(a) & is.na(b)) == F),]
  # if conflicts - save x and present warning
  if(nrow(x) >0){
    warning("conflicts")
    return(x)
  } else {
    # if no conflicts print confirmation
    print("no conflicts")
  }
}