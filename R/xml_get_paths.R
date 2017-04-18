#' view the paths of xml tree. keeps order as one dives into each node. option to mark any terminal paths (paths that end on the last possible node).
#' @param doc an xml doc
#' @param mark_terminal string used to mark terminal nodes. NULL by default.
#' @param only_terminal_parent whether to return only the parent node of any terminal branch
#' @export

xml_get_paths <- function(doc, mark_terminal = ifelse(only_terminal_parent, ">>", ""),
  only_terminal_parent = FALSE) {

  stopifnot(any(c("xml_nodeset","xml_document") %in% class(doc)))

  if("xml_document" %in% class(doc)) {
    root <- xml2::xml_root(doc)
    names_list <- list(xml2::xml_name(root))
    nodeset <- xml2::xml_children(root)
  } else {
    nodeset <- doc
  }

  paths <- path_dig(nodeset, mark_terminal = mark_terminal) %>%
    lapply(. %>% unlist)
  paths <- lapply(paths, gsub, pattern = '\\[[0-9]+\\]', replacement = '', character(0))

  if(only_terminal_parent) {
    paths <- lapply(paths, function(x) {
      pattern <- paste0('^', mark_terminal)
      indx <- grepl(pattern, x)
      # get parent nodes and refine
      gsub(pattern, '', x[indx]) %>%
        dirname() %>%
        unique()
    })
  }

  return(paths)

}

#' @export
xml_get_path <- function(doc, mark_terminal = ifelse(only_terminal_parent, ">>", ""),
  only_terminal_parent = FALSE) xml_get_paths(doc, markdown)


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
      terminal_nodes <- nodeset[terminal] # subset to only terminal nodes
      if(!is.null(mark_terminal)) {
        mark_terminal <- as.character(mark_terminal)
        x <- trimws(paste0(mark_terminal, xml2::xml_path(terminal_nodes)))
      } else {
        x <- xml2::xml_path(terminal_nodes)
      }
      return(x)
    } else {
      terminal_nodes <- nodeset[terminal]
      if(!is.null(mark_terminal)) {
        mark_terminal <- as.character(mark_terminal)
        x <- trimws(paste0(mark_terminal, xml2::xml_path(terminal_nodes)))
      } else {
        x <- xml2::xml_path(terminal_nodes)
      }
      nodeset <- nodeset[!terminal]
      y <- xml2::xml_path(nodeset)
      nodeset <- lapply(nodeset, xml2::xml_children)
      append(x, mapply(function(i, j) list(i, path_dig(j, ...)),
        i = y, j = nodeset, USE.NAMES = FALSE, SIMPLIFY = FALSE))
    }
  }
}
