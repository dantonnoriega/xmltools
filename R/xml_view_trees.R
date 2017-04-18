#' view xml simliar to the unix tree command line tool
#' @param xml an object of class "xml_nodeset" or "xml_document"
#' @param depth how deep to go. root is 0, next is 1, etc. NULL by default.
#' @export
xml_view_trees <- function(xml, depth = NULL) {

  stopifnot(any(c("xml_tree", "xml_tree_list", "xml_document", "xml_nodeset") %in% class(xml)))

  if(any(c("xml_nodeset","xml_document") %in% class(xml))) {
    xml <- xml_get_trees(xml, depth = depth)
  } else {
    if(!is.null(depth)) warning("Option `depth` is ignored for xml_tree and xml_tree_objects.")
  }

  if("xml_tree" %in% class(xml)) {
    out <- paste(xml, collapse = "\n")
    cat(out, "\n")
    invisible()
  }

  if("xml_tree_list" == class(xml)) {
    out <- lapply(xml, paste, collapse = "\n")
    if(length(xml) > 1) {
      out <- paste("\n\n", paste0("(",seq_along(out),")"), "------------\n\n", out)
    } else {
      out <- paste(out)
    }
    cat(out, sep = "\n")
    invisible()
  }

}

#' @export
xml_view_tree <- function(xml, depth = NULL) xml_view_trees(xml, depth = depth)

#' get a tree of xml simliar to the unix tree command line tool
#' @param doc an xml doc or nodeset
#' @param depth how deep to go. root is 0, next is 1, etc. NULL by default.
xml_get_trees <- function(doc, depth = NULL) {

  stopifnot(any(c("xml_document", "xml_nodeset") %in% class(doc)))

  if("xml_document" %in% class(doc)) {
    root <- xml2::xml_root(doc)
    nodeset <- xml2::xml_children(root)
  }

  if("xml_nodeset" %in% class(doc)) nodeset <- doc

  tree <- tree_dig(nodeset, depth, counter = 1) %>%
    parse_tree()
  tree <- lapply(tree, structure, class = "xml_tree")
  tree <- structure(tree, class = "xml_tree_list")

  return(tree)
}

xml_get_tree <- function(doc, depth = NULL) xml_get_trees(doc, depth = NULL)

tree_dig <- function(nodeset, depth, counter = 1) {

  if(!is.null(depth)) {
    if(counter > depth) return()
  }

  node_len <- nodeset %>%
    xml2::xml_length()

  # top level are nodes with ln == 0
  terminal <- node_len == 0

  x <- nodeset %>%
      xml2::xml_name() %>% # get all node names
      paste(counter, "├──", .)

  if(sum(terminal) == 0) { # no top level data
      x[length(x)] <- gsub("├──","└──", x[length(x)]) # mark the last branch
      nodeset <- lapply(nodeset, xml2::xml_children)
      mapply(function(i, j) append(list(i), tree_dig(j, depth, counter + 1)),
        i = x, j = nodeset, USE.NAMES = FALSE, SIMPLIFY = FALSE)
  } else {
    if(sum(terminal) == length(terminal)) {
      x <- x[terminal] # subset to only terminal nodes
      x[length(x)] <- gsub("├──","└──", x[length(x)]) # mark the last branch
      return(x)
    } else {
      y <- x[terminal] # get terminal nodes
      x <- x[!terminal] # get non terminal nodes
      x[length(x)] <- gsub("├──","└──", x[length(x)]) # mark the last branch
      nodeset <- nodeset[!terminal]
      nodeset <- lapply(nodeset, xml2::xml_children)
      append(list(y), mapply(function(i, j)
        append(list(i), tree_dig(j, depth, counter + 1)),
        i = x, j = nodeset, USE.NAMES = FALSE, SIMPLIFY = FALSE))
    }
  }
}

parse_tree <- function(tree) {
  trees <- lapply(tree, unlist) %>%
    lapply(. %>% add_padding())
  return(trees)
}

add_padding <- function(x) {

  padding <- function(p) paste(rep("  ", p-1), collapse = "")

  m <- regexpr('^[0-9]', x)
  num <- regmatches(x, m) %>%
    as.integer()
  x <- gsub('^[0-9] ', '', x)

  paste0(sapply(num, padding), x)

}
