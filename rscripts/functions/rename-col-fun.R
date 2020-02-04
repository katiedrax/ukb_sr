# merge and rename column if two columns identical
rename_col <- function(df, a, b, b_new_name){
  # a = column you want to delete
  # b = column you want to keep and rename
  # check if there's conflicts
  if(identical(df[[a]], df[[b]])){
    # remove a column
    df[[a]] <- NULL
    # rename b
    colnames(df)[colnames(df) == b] <- b_new_name
    return(df)
  } else {
    # if still conflicts stop merge
    stop("still conflicts so can't merge columns")
  }
}