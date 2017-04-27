
<!-- README.md is generated from README.Rmd. Please edit that file -->
Motivation for `xmltools`
=========================

There are 3 things I felt were missing from the two wonderful packages [`XML`](https://cran.r-project.org/web/packages/XML/index.html) and [`xml2`](https://github.com/hadley/xml2):

1.  A easier, more condensed way to see the structure of xml data.
    -   `xml2::xml_structure` provides a way to look at the structure, but I find that it is not very easy to read and takes up too much console space.
    -   `xmltools::xml_view_tree` is more condense and attempts to emulate the `tree` command line program.
2.  A quick way to determine all feasible xpaths and to identify terminal nodes. Data values of interest are contained in terminal nodes (nodes of "length zero" that do no dig any deeper). Quickly getting xpaths to the *parents* of these nodes makes extracting data much easier---and faster if you do not recursively dig deeper.
    -   `xmltools::xml_get_paths` can find all paths for a given nodeset or xml document. It has options to help tag terminal nodes (`mark_terminal`) and the option to return the parent of any terminal nodes (`mark_terminal_parent`).
3.  Other alternatives for converting xml data to data frames.
    -   `XML::xmlToDataFrame` exists but it seems to always dig recursively. This leads to some crappy data frames.
    -   I offer two alternatives, `xml_to_df` and `xml_dig_df`.
        -   `xml_to_df` uses the `XML` and `data.table` packages
        -   `xml_dig_df` is based of `xml2` and `tidyverse` packages.

Installation
============

Run the following.

``` r
devtools::install_github('ultinomics/xmltools')
library(xmltools)
```

Examples
========

Let's set up the first example using some ebay data from the [UW XML Data Repository](http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/www/repository.html). These data come as part of the package because I dropped the really annoying `description` field that makes the data hard to look at. (Parses it just fine!)

``` r
library(xmltools)

# USING ebay.xml ------------------------------------------------
# load the data
file <- system.file("extdata", "ebay.xml", package = "xmltools")
doc <- file %>%
  xml2::read_xml()
nodeset <- doc %>%
  xml2::xml_children() # get top level nodeset
```

View XML trees/structures
-------------------------

Let's look at the structure of the data. The function

``` r
# `xml_view_tree` structure
# we can get a tree for each node of the doc
doc %>% 
  xml_view_tree()
doc %>% # we can also vary the depth
  xml_view_tree(depth = 2)
```

 

``` r
# easier to read and understand than `xml2::xml_structure()` and has the `depth` option
nodeset[1] %>% xml2::xml_structure()
#> [[1]]
#> <listing>
#>   <seller_info>
#>     <seller_name>
#>       {text}
#>     <seller_rating>
#>       {text}
#>   <payment_types>
#>     {text}
#>   <shipping_info>
#>     {text}
#>   <buyer_protection_info>
#>     {text}
#>   <auction_info>
#>     <current_bid>
#>       {text}
#>     <time_left>
#>       {text}
#>     <high_bidder>
#>       <bidder_name>
#>         {text}
#>       <bidder_rating>
#>         {text}
#>     <num_items>
#>       {text}
#>     <num_bids>
#>       {text}
#>     <started_at>
#>       {text}
#>     <bid_increment>
#>       {text}
#>     <location>
#>       {text}
#>     <opened>
#>       {text}
#>     <closed>
#>       {text}
#>     <id_num>
#>       {text}
#>     <notes>
#>       {text}
#>   <bid_history>
#>     <highest_bid_amount>
#>       {text}
#>     <quantity>
#>       {text}
#>   <item_info>
#>     <memory>
#>       {text}
#>     <hard_drive>
#>       {text}
#>     <cpu>
#>       {text}
#>     <brand>
#>       {text}

## or, we can extract from nodesets
class(nodeset[1])
#> [1] "xml_nodeset"
nodeset[1] %>%
  xml_view_trees()
#> └── listing
#>   ├── payment_types
#>   ├── shipping_info
#>   ├── buyer_protection_info
#>   ├── seller_info
#>     ├── seller_name
#>     └── seller_rating
#>   ├── auction_info
#>     ├── current_bid
#>     ├── time_left
#>     ├── num_items
#>     ├── num_bids
#>     ├── started_at
#>     ├── bid_increment
#>     ├── location
#>     ├── opened
#>     ├── closed
#>     ├── id_num
#>     ├── notes
#>     └── high_bidder
#>       ├── bidder_name
#>       └── bidder_rating
#>   ├── bid_history
#>     ├── highest_bid_amount
#>     └── quantity
#>   └── item_info
#>     ├── memory
#>     ├── hard_drive
#>     ├── cpu
#>     └── brand
nodeset[1] %>%
  xml_view_trees(depth=2)
#> └── listing
#>   ├── payment_types
#>   ├── shipping_info
#>   ├── buyer_protection_info
#>   ├── seller_info
#>   ├── auction_info
#>   ├── bid_history
#>   └── item_info

## will not work with class "xml_node" (can't use lapply on those, apparently)
class(nodeset[[1]])
#> [1] "xml_node"
try(nodeset[[1]] %>%
  xml_view_tree()
)
```

Get Terminal Nodes
------------------

Terminal nodes in XMLs are nodes that do no have any "children". These nodes contain the information we generally want to extract into a tidy data frame.

I've found myself wanting easy access to all XML paths but could find no tool to do so easily and quickly. I especially wanted the xpaths to terminal nodes for any XML structure. This is accomplished using the `xml_get_paths` function.

``` r
# one can see all the paths per node of a doc
doc %>%
  xml_get_paths()
```

 

``` r
# can look at one nodeset
## NOTE that nodesets can vary, so looking at one doesn't mean you'll find all feasible paths

nodeset[1] %>%
  xml_get_paths()
#> [[1]]
#>  [1] "/root/listing"                                       
#>  [2] "/root/listing/payment_types"                         
#>  [3] "/root/listing/shipping_info"                         
#>  [4] "/root/listing/buyer_protection_info"                 
#>  [5] "/root/listing/seller_info"                           
#>  [6] "/root/listing/seller_info/seller_name"               
#>  [7] "/root/listing/seller_info/seller_rating"             
#>  [8] "/root/listing/auction_info"                          
#>  [9] "/root/listing/auction_info/current_bid"              
#> [10] "/root/listing/auction_info/time_left"                
#> [11] "/root/listing/auction_info/num_items"                
#> [12] "/root/listing/auction_info/num_bids"                 
#> [13] "/root/listing/auction_info/started_at"               
#> [14] "/root/listing/auction_info/bid_increment"            
#> [15] "/root/listing/auction_info/location"                 
#> [16] "/root/listing/auction_info/opened"                   
#> [17] "/root/listing/auction_info/closed"                   
#> [18] "/root/listing/auction_info/id_num"                   
#> [19] "/root/listing/auction_info/notes"                    
#> [20] "/root/listing/auction_info/high_bidder"              
#> [21] "/root/listing/auction_info/high_bidder/bidder_name"  
#> [22] "/root/listing/auction_info/high_bidder/bidder_rating"
#> [23] "/root/listing/bid_history"                           
#> [24] "/root/listing/bid_history/highest_bid_amount"        
#> [25] "/root/listing/bid_history/quantity"                  
#> [26] "/root/listing/item_info"                             
#> [27] "/root/listing/item_info/memory"                      
#> [28] "/root/listing/item_info/hard_drive"                  
#> [29] "/root/listing/item_info/cpu"                         
#> [30] "/root/listing/item_info/brand"

nodeset[1] %>%
  xml_get_paths(mark_terminal = ">>") # can mark terminal nodes
#> [[1]]
#>  [1] "/root/listing"                                         
#>  [2] ">>/root/listing/payment_types"                         
#>  [3] ">>/root/listing/shipping_info"                         
#>  [4] ">>/root/listing/buyer_protection_info"                 
#>  [5] "/root/listing/seller_info"                             
#>  [6] ">>/root/listing/seller_info/seller_name"               
#>  [7] ">>/root/listing/seller_info/seller_rating"             
#>  [8] "/root/listing/auction_info"                            
#>  [9] ">>/root/listing/auction_info/current_bid"              
#> [10] ">>/root/listing/auction_info/time_left"                
#> [11] ">>/root/listing/auction_info/num_items"                
#> [12] ">>/root/listing/auction_info/num_bids"                 
#> [13] ">>/root/listing/auction_info/started_at"               
#> [14] ">>/root/listing/auction_info/bid_increment"            
#> [15] ">>/root/listing/auction_info/location"                 
#> [16] ">>/root/listing/auction_info/opened"                   
#> [17] ">>/root/listing/auction_info/closed"                   
#> [18] ">>/root/listing/auction_info/id_num"                   
#> [19] ">>/root/listing/auction_info/notes"                    
#> [20] "/root/listing/auction_info/high_bidder"                
#> [21] ">>/root/listing/auction_info/high_bidder/bidder_name"  
#> [22] ">>/root/listing/auction_info/high_bidder/bidder_rating"
#> [23] "/root/listing/bid_history"                             
#> [24] ">>/root/listing/bid_history/highest_bid_amount"        
#> [25] ">>/root/listing/bid_history/quantity"                  
#> [26] "/root/listing/item_info"                               
#> [27] ">>/root/listing/item_info/memory"                      
#> [28] ">>/root/listing/item_info/hard_drive"                  
#> [29] ">>/root/listing/item_info/cpu"                         
#> [30] ">>/root/listing/item_info/brand"

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
```

Extracting XML Data to Tidy Data Frames
---------------------------------------

Next, we use the terminal xpaths above to extract the data we want.

First, I want to show the issue with using `XML::xmlToDataFrame`.

``` r
# xmlToDataFrame works great on terminal nodes IF there are no non-terminal nodes any deeper.
## we extract a data frame for each parent of terminal nodes

df0 <- lapply(terminal_xpaths, function(x) {
  doc <- file %>% XML::xmlInternalTreeParse()
  nodeset <- XML::getNodeSet(doc, x)
  XML::xmlToDataFrame(nodeset, stringsAsFactors = FALSE) %>%
    dplyr::as_data_frame()
})
```

There is data contained in the terminal nodes

    /root/listing/payment_types
    /root/listing/shipping_info
    /root/listing/buyer_protection_info

with the parent node

    /root/listing

But `XML::xmlToDataFrame` will keep digging into `/root/listing` and extract data from xpaths like `/root/listing/seller_info`. But it does so extracting data in a non tidy way. We can see this below comparing the data in `df0[[1]] %>% dplyr::select(seller_info)` vs `df0[[2]]`, which is data extracted from just from `/root/listing/seller_info` and deeper.

``` r
# problem with xmlToDataFrame is it keeps digging into other nodes recursively in "/root/listing"

xpaths[1] # /root/listing is terminal parent but xmlToDataFrame keeps digging
#> [1] "/root/listing"

df0[[1]] %>%
  dplyr::select(seller_info) # not good; keeps diving into other nodes but fails to separate
#> # A tibble: 5 × 1
#>             seller_info
#>                   <chr>
#> 1       cubsfantony 848
#> 2            ct-inc 403
#> 3            ct-inc 403
#> 4  bestbuys4systems  28
#> 5  sales@ctgcom.com 219

xpaths[2]
#> [1] "/root/listing/payment_types"

df0[[2]] # works because the recursive dig down hits only the terminal nodes
#> # A tibble: 5 × 2
#>         seller_name seller_rating
#>               <chr>         <chr>
#> 1       cubsfantony           848
#> 2            ct-inc           403
#> 3            ct-inc           403
#> 4 bestbuys4systems             28
#> 5  sales@ctgcom.com           219
```

The solution that worked for me was to write a function that, by default, does not dig into non-terminal nodes. That is, when given the xpath `/root/listing`, the function will only go into terminal nodes

    /root/listing/payment_types
    /root/listing/shipping_info
    /root/listing/buyer_protection_info

And ignore the other, non-terminal nodes unless instructed to do so otherwise by setting option `dig = TRUE`.

### The `xml_to_df` Function

The `xml_to_df` function is built on the `XML` package and `data.table`. By default, it does not dig into non-terminal nodes (`dig = FALSE`) when given any xpath. One can pass an xpath to an XML file (`is_xml = FALSE`) or an already parsed XML file (`is_xml = TRUE`).

``` r
# xml_to_df (XML package based)
## does not dig by default
## use the terminal xpaths to get data frames
terminal_xpaths
#> [1] "/root/listing"                         
#> [2] "/root/listing/seller_info"             
#> [3] "/root/listing/auction_info"            
#> [4] "/root/listing/auction_info/high_bidder"
#> [5] "/root/listing/bid_history"             
#> [6] "/root/listing/item_info"

## we send each terminal xpath to `xml_to_df`. 
## the file source is the parsed xml object `doc`, so we set `is_xml = TRUE`
## we do no want to dig, which quickly gets us the data we want for each terminal xpath `dig = FALSE` (default)
df1 <- lapply(terminal_xpaths, xml_to_df, file = doc, is_xml = TRUE, dig = FALSE) %>%
  dplyr::bind_cols()
```

### The `xml_dig_df` Function

The other fuction is `xml_dig_df` which is built on `xml2` and `tidyverse` packages.

The important distinction is that we first need to find all the terminal nodesets we want to parse. We find these using `xml2::xml_find_all` on each of the `terminal_xpaths` on the original parsed xml file (`doc`).

For each `terminal_nodeset`, we then apply `xml_dig_df`. For each nodeset, we will get single row of data, so we bind the results for each nodeset. The final data frame is created by column binding. (I convert all empty strings to `NA` for good measure.)

``` r
# xml_dig_df (xml2 package based)
terminal_nodesets <- lapply(terminal_xpaths, xml2::xml_find_all, x = doc)
df2 <- terminal_nodesets %>%
  purrr::map(xml_dig_df) %>% ## does not dig by default
  purrr::map(dplyr::bind_rows) %>%
  dplyr::bind_cols() %>%
  dplyr::mutate_all(empty_as_na)
```

``` r
# they're the same!
identical(df1, data.table::as.data.table(df2))
#> [1] TRUE
```

Example 2
=========

Below is another example to work through. The output is hidden but copy and paste the code or find the source file in this repository `examples.R`.

``` r
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
df1 <- lapply(xpaths, xml_to_df, file = file, is_xml = FALSE, dig = FALSE) %>%
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
```
