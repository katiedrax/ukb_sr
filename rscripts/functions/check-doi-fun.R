check_doi <- function(doi_col){
  # check no dois missing
  if(sum(is.na(doi_col)) > 0) stop("dois missing") else
    # check all dois contain 10.
    if(length(doi_col[!grepl("10", doi_col)]) != 0) stop("some dois don't contain 10") else
      # check all dois start with 10
      if(length(doi_col[!grepl("^10", doi_col)]) != 0) stop("some dois don't start with 10") else
        # check no dois duplicated
        if(sum(duplicated(doi_col)) > 0) stop("duplicate dois") else
          # correct
          return("dois OK")
}