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

# using xml_to_df
top_df1  <- xml_to_df(doc, '//listing', is_xml = TRUE, dig   = FALSE)
## use the terminal xpaths to get data frames
term_df1 <- lapply(xpaths, xml_to_df, file     = doc, is_xml = TRUE, dig = FALSE) %>%
  dplyr::bind_cols()
df1 <- dplyr::bind_cols(top_df1, term_df1)

df1 %>%
  dplyr::select(-description) # omit description to see df better

# xml_dig
top_df2 <- nodeset %>%
  xml_dig(F) %>% # dont dig beyond the first terminal node
  dplyr::bind_rows()
## lets get terminal data
tree[[1]] %>% xml_view_tree()
lvl2 <- nodeset %>% # get children of each node in nodeset
  lapply(. %>%
    xml2::xml_children() %>%
    xml2::xml_children()) # use lapply so nodes are separated
term_df2 <- lvl2 %>%
  lapply(. %>%
    xml_dig() %>% # apply xml_dig to get set of nodesets
    dplyr::bind_cols()) %>%
  dplyr::bind_rows()

df2 <- dplyr::bind_cols(top_df2, term_df2)

df2 %>%
  dplyr::select(-description) # omit description to see df better


# USING wsu.xml ------------------------------------------------
# larger file

# using xml_to_df
file <- 'data-raw/wsu.xml'
doc <- file %>%
  xml2::read_xml()
df <- file %>% xml_to_df(xpath = "//course", is_xml = F)

nodeset <- file %>%
  xml2::read_xml() %>%
  xml2::xml_children()
nodeset %>% xml2::xml_path()
nodeset %>% xml2::xml_structure()
ns2 <- nodeset %>%
  lapply(. %>% xml2::xml_children())



nodeset %>% lapply(. %>% xml_to_df(xpath = "/course", is_xml=T))