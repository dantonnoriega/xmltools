library(xmlExtras)

# USING ebay.xml ------------------------------------------------
# learn about the structure
file <- 'data-raw/ebay.xml'
doc <- file %>%
  xml2::read_xml()
tree <- doc %>%
  xml_get_trees()
nodeset <- doc %>%
  xml2::xml_children()

## structure
tree[[1]] %>% xml_view_tree()
nodeset[[1]] %>% xml2::xml_structure()
terminal <- doc %>% ## get the xpath to parents of terminal node
  xml_get_paths(only_terminal_parent = TRUE)
xpaths <- terminal %>% ## collapse xpaths to unique only
  unlist() %>%
  unique()

# xmlToDataFrame works great on terminal nodes.
# problem with xmlToDataFrame is it keeps digging.
df0 <- lapply(xpaths, function(x) {
  doc <- file %>% XML::xmlInternalTreeParse()
  nodeset <- XML::getNodeSet(doc, x)
  XML::xmlToDataFrame(nodeset, stringsAsFactors = FALSE) %>%
    dplyr::as_data_frame()
})

doc %>%
  xml_get_paths(mark_terminal = ">>") %>%
  '[['(1)

# want just:
#   "/root/listing/payment_types"
#   "/root/listing/shipping_info"
#   "/root/listing/buyer_protection_info"
xpaths[1] # /root/listing is terminal parent but xmlToDataFrame keeps digging
df0[[1]] # not good; keeps diving into other nodes
xpaths[2]
df0[[2]] # good because it could dive further

# xml_to_df (XML package based)
## does not dig by default
## use the terminal xpaths to get data frames
df1 <- lapply(xpaths, xml_to_df, file = doc, is_xml = TRUE, dig = FALSE) %>%
  dplyr::bind_cols()
df1

# xml_dig_df (xml2 package based)
## does not dig by default
tree[[1]] %>% xml_view_tree()
terminal_nodesets <- lapply(xpaths, xml2::xml_find_all, x = doc)
df2 <- terminal_nodesets %>%
  purrr::map(xml_dig_df) %>%
  purrr::map(dplyr::bind_rows) %>%
  dplyr::bind_cols()


# USING wsu.xml ------------------------------------------------
# larger file

# using xml_to_df
file <- 'data-raw/wsu.xml'
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

xpaths <- terminal_paths %>% ## collapse xpaths to unique only
  unlist() %>%
  unique()

# xml_to_df (XML package based)
## note that we use file, not doc, hence is_xml = FALSE
df1 <- lapply(xpaths, xml_to_df, file = file, is_xml = FALSE, dig = FALSE) %>%
  dplyr::bind_cols()
df1

# xml_dig_df (xml2 package based)
## faster!
terminal_nodesets <- lapply(xpaths, xml2::xml_find_all, x = doc) # use xml docs, not nodesets! I think this is because it searches the 'root'.
df2 <- terminal_nodesets %>%
  purrr::map(xml_dig_df) %>%
  purrr::map(dplyr::bind_rows) %>%
  dplyr::bind_cols() %>%
  dplyr::mutate_all(empty_as_na) # kill of NAs
df2

# they're the same!
identical(df1, data.table::as.data.table(df2))
