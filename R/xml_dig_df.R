#' dig into a nodeset until we find top level data. that is, a node of lenght 0. extract any data with node == 0 (indexed TRUE)
#' @param nodeset any nodeset of object type xml2
#' @param dig option to keep digging beyond the first terminal node. if FALSE then only data from the first set of terminal nodes is returned
#' @param return_if_empty what to return if the node is empty. defaults to empty data.frame
#' @export
#' @return nested lists of dataframes

xml_dig_df <- function(nodeset, dig = FALSE, return_if_empty = data.frame()) {

  stopifnot(class(nodeset) == "xml_nodeset")

  if(length(nodeset) == 0) return(return_if_empty)
  else {
    node_names <- nodeset %>% xml2::xml_name()
    node_len <- nodeset %>% xml2::xml_length()

    # top level are nodes with node_len == 0
    terminal <- node_len == 0

    if(dig) {
      if(sum(terminal) == 0) { # no top level data
        nodeset <- lapply(nodeset, xml2::xml_children)
        lapply(nodeset, xml_dig_df)
      } else {
          DF <- nodeset[terminal] %>%
            xml2::xml_text() %>%
            t() %>%
            tibble::as_tibble( .name_repair="unique" )
          colnames(DF) <- node_names[terminal]
        if(sum(terminal) == length(terminal)) {
          return(DF)
        } else {
          nodeset <- nodeset[!terminal]
          nodeset <- lapply(nodeset, xml2::xml_children)
          append(list(DF), lapply(nodeset, xml_dig_df))
        }
      }
    } else {
      if(sum(terminal) == 0) { # no top level data
        nodeset <- lapply(nodeset, xml2::xml_children)
        lapply(nodeset, xml_dig_df, dig = FALSE)
      } else {
        DF <- nodeset[terminal] %>%
          xml2::xml_text() %>%
          t() %>%
          tibble::as_tibble( .name_repair="unique" )
        colnames(DF) <- node_names[terminal]
        return(DF)
      }
    }
  }
}
