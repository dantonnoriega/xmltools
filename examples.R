## devtools::install_github('ultinomics/xmltools')
## library(xmltools)
library(xmltools)

# USING ebay.xml ------------------------------------------------
# load the data
file <- system.file("extdata", "ebay.xml", package = "xmlExtras")
doc <- file %>%
  xml2::read_xml()
nodeset <- doc %>%
  xml2::xml_children() # get top level nodeset

# `xml_view_tree` structure
## we can get a tree for each node of the doc
doc %>%
  xml_view_tree()
doc %>% # we can also vary the depth
  xml_view_tree(depth = 2)

## easier to read and understand than `xml2::xml_structure()` and has the `depth` option
nodeset[1] %>% xml2::xml_structure()

## or, we can extract from nodesets
class(nodeset[1])
nodeset[1] %>%
  xml_view_trees()
nodeset[1] %>%
  xml_view_trees(depth=2)

## will not work with class "xml_node" (can't use lapply on those, apparently)
class(nodeset[[1]])
try(nodeset[[1]] %>%
  xml_view_tree()
)

## one can see all the paths per node of a doc
doc %>%
  xml_get_paths()

## can look at one nodeset
## NOTE that nodesets can vary, so looking at one doesn't mean you'll find all feasible paths

nodeset[1] %>%
  xml_get_paths()

nodeset[1] %>%
  xml_get_paths(mark_terminal = ">>") # can mark terminal nodes

## we can find all feasible paths then collapse

terminal <- doc %>% ## get all xpaths
  xml_get_paths()

xpaths <- terminal %>% ## collapse xpaths to unique only
  unlist() %>%
  unique()

## but what we really want is the parent node of terminal nodes.
## use the `only_terminal_parent = TRUE` to do this

terminal_parent <- doc %>% ## get all xpaths to parents of parent node
  xml_get_paths(only_terminal_parent = TRUE)

terminal_xpaths <- terminal_parent %>% ## collapse xpaths to unique only
  unlist() %>%
  unique()

## xmlToDataFrame works great on terminal nodes IF there are no non-terminal nodes any deeper.
## we extract a data frame for each parent of terminal nodes

df0 <- lapply(terminal_xpaths, function(x) {
  doc <- file %>% XML::xmlInternalTreeParse()
  nodeset <- XML::getNodeSet(doc, x)
  XML::xmlToDataFrame(nodeset, stringsAsFactors = FALSE) %>%
    dplyr::as_data_frame()
})

## problem with xmlToDataFrame is it keeps digging into other nodes recursively in "/root/listing"

xpaths[1] # /root/listing is terminal parent but xmlToDataFrame keeps digging

df0[[1]] %>%
  dplyr::select(seller_info) # not good; keeps diving into other nodes but fails to separate

xpaths[2]

df0[[2]] # works because the recursive dig down hits only the terminal nodes


# xml_to_df (XML package based)
## does not dig by default
## use the terminal xpaths to get data frames
terminal_xpaths

## we send each terminal xpath to `xml_to_df`.
## the file source is the parsed xml object `doc`, so we set `is_xml = TRUE`
## we do no want to dig, which quickly gets us the data we want for each terminal xpath `dig = FALSE` (default)
df1 <- lapply(terminal_xpaths, xml_to_df, file = doc, is_xml = TRUE, dig = FALSE) %>%
  dplyr::bind_cols()

# xml_dig_df (xml2 package based)
terminal_nodesets <- lapply(terminal_xpaths, xml2::xml_find_all, x = doc)
df2 <- terminal_nodesets %>%
  purrr::map(xml_dig_df) %>% ## does not dig by default
  purrr::map(dplyr::bind_rows) %>%
  dplyr::bind_cols() %>%
  dplyr::mutate_all(empty_as_na)

## they're the same!
identical(df1, data.table::as.data.table(df2))

# USING wsu.xml ------------------------------------------------
# larger file

# using xml_to_df
file <- "http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/courses/wsu.xml"
doc <- file %>%
  xml2::read_xml()
nodeset <- doc %>%
  xml2::xml_children()
length(nodeset) # lots of nodes!
nodeset[1] %>% # lets look at ONE node's tree
  xml_view_tree()

## takes a long time. likely can extract from a single node
# terminal_paths <- doc %>% ## get the xpath to parents of terminal node
#   xml_get_paths(only_terminal_parent = TRUE)

# lets assume that most nodes share the same structure
terminal_paths <- nodeset[1] %>%
  xml_get_paths(only_terminal_parent = TRUE)

terminal_xpaths <- terminal_paths %>% ## collapse xpaths to unique only
  unlist() %>%
  unique()

# xml_to_df (XML package based)
## note that we use file, not doc, hence is_xml = FALSE
df1 <- lapply(terminal_xpaths, xml_to_df, file = file, is_xml = FALSE, dig = FALSE) %>%
  dplyr::bind_cols()
df1

# xml_dig_df (xml2 package based)
## faster!
terminal_nodesets <- lapply(terminal_xpaths, xml2::xml_find_all, x = doc) # use xml docs, not nodesets! I think this is because it searches the 'root'.
df2 <- terminal_nodesets %>%
  purrr::map(xml_dig_df) %>%
  purrr::map(dplyr::bind_rows) %>%
  dplyr::bind_cols() %>%
  dplyr::mutate_all(empty_as_na)
df2

# they're the same!
identical(df1, data.table::as.data.table(df2))
