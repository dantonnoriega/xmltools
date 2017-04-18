#' function hacked from https://hopstat.wordpress.com/2014/01/14/faster-xml-conversion-to-data-frames/. fastest, most dependable, but requires xpath knowledge.
#' @import data.table
#' @param file xml or zip file imported using xml2
#' @param xpath xml path to file
#' @param is_xml if already xml, dont parse
#' @param dig whether to recursively search into nodes. better and faster to keep FALSE and explicitly write out xpath.
#' @export

xml_to_df <- function(file, xpath, is_xml = FALSE, dig = FALSE) {

  print(xpath)

  # import file or xml
  if(!is_xml) {
    doc <- xml2::read_xml(file) %>% # xml2 can handle zip files of data!
      XML::xmlInternalTreeParse()
  } else {
    if("xml_document" %in% class(file)) {
      doc <- file %>%
        XML::xmlInternalTreeParse()
    } else {
      doc <- file
    }
  }

  ## get the records for that form
  nodeset <- XML::getNodeSet(doc, xpath)

  ## if empty nodeset, just return empty data table
  if(length(nodeset) == 0) return(data.table::data.table())
  else {
    ## get the field names
    vars <- lapply(nodeset, names)

    ## get the total fields that are in any record
    fields = unique(unlist(vars))

    ## extract the values from all fields
    # single '/' is DIRECT CHILDREN; '//' is ALL matching CHILDREN
    # `XML::xpathSApply` will iterate through each VIABLE matching path;
    #   use '/' so that we go through each direct child one by one (no digging)
    dl = lapply(fields, function(x) {
      XML::xpathSApply(doc, paste0(xpath, "/", x), XML::xmlValue, recursive = dig, trim = TRUE)
    })

    # convert empty strings to NA
    dl <- lapply(dl, function(x)
      vapply(x, function(y)
        ifelse(length(y) == 0, NA_character_, y), character(1), USE.NAMES = FALSE))

    #drop empty columns
    drop <- vapply(dl, function(x) sum(!is.na(x)), integer(1)) == 0
    dl <- dl[!drop]
    fields <- fields[!drop]

    ## make logical matrix whether each record had that field
    indx_mat = t(sapply(vars, function(x) fields %in% x))
    DT = data.table::data.table(matrix(NA, nrow = nrow(indx_mat), ncol = ncol(indx_mat)))
    DT = DT[, lapply(.SD, as.character)] # convert to character
    names(DT) = fields

    ## fill in that data.table
    for (i in seq_along(dl)) {
      DT[indx_mat[,i], c(fields[i]) := list(dl[[i]])]
    }

    return(DT)
  }

}