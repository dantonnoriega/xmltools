#' checks columns for data
has_data <- function(x) { sum(!is.na(x)) > 0 }

#' empty strings as NA
#' @source https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  if(class(x) == "character") ifelse(as.character(x)!="", x, NA) else x
}

