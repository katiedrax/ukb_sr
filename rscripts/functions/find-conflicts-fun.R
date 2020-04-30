# function to check conflicts
find_cons <- function(df,a,b){
  if(is.null(a)) stop("a doesn't exist")
  if(is.null(b)) stop("b doesn't exist")
  # a  = variable to be compared from reviewer1
  # b = variable to be compared from reviewer2
  # a & b should be identical before the suffixes
  # df = dataframe columns are in >
  # df 
  # create df of reviewer1 and reviewer2's columns that don't match and include NAs in matching
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