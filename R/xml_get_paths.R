#' view the paths of xml tree. keeps order as one dives into each node. option to mark any terminal paths (paths that end on the last possible node).
#' @param doc an xml doc
#' @param mark_terminal string used to mark terminal nodes. NULL by default.
#' @param only_terminal_parent whether to return only the parent node of any terminal branch
#' @export

xml_get_paths <- function(doc, mark_terminal = ifelse(only_terminal_parent, ">>", NULL),
  only_terminal_parent = FALSE) {

  stopifnot("xml_document" %in% class(doc))

  root <- xml2::xml_root(doc)
  names_list <- list(xml2::xml_name(root))
  top_nodeset <- xml2::xml_children(root)
  paths <- path_dig(top_nodeset, mark_terminal = mark_terminal) %>%
    lapply(. %>% unlist)

  if(only_terminal_parent) {
    paths <- lapply(paths, function(x) {
      pattern <- paste0('^', mark_terminal)
      indx <- grepl(pattern, x)
      # get parent nodes and refine
      y <- gsub(pattern, '', x[indx]) %>%
        dirname() %>%
        unique()
      gsub('\\[[0-9]+\\]', '', y)
    })
  }

  return(paths)

}

#' @export
xml_get_path <- function(doc, mark_terminal = NULL) xml_get_paths(doc, markdown)


path_dig <- function(nodeset, ...) {

  args <- list(...)
  mark_terminal <- args$mark_terminal

  node_len <- nodeset %>%
    xml2::xml_length()

  # top level are nodes with ln == 0
  terminal <- node_len == 0

  if(sum(terminal) == 0) { # no top level data
      x <- xml2::xml_path(nodeset)
      nodeset <- lapply(nodeset, xml2::xml_children)
      mapply(function(i, j) list(i, path_dig(j, ...)),
        i = x, j = nodeset, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  } else {
    if(sum(terminal) == length(terminal)) {
      nodeset <- nodeset[terminal] # subset to only terminal nodes
      if(!is.null(mark_terminal)) {
        mark_terminal <- as.character(mark_terminal)
        x <- trimws(paste0(mark_terminal, xml2::xml_path(nodeset)))
      } else {
        x <- xml2::xml_path(nodeset)
      }
      return(x)
    } else {
      nodeset <- nodeset[!terminal] # subset
      x <- xml2::xml_path(nodeset)
      nodeset <- lapply(nodeset, xml2::xml_children)
      mapply(function(i, j) list(i, path_dig(j, ...)),
        i = x, j = nodeset, USE.NAMES = FALSE, SIMPLIFY = FALSE)
    }
  }
}
