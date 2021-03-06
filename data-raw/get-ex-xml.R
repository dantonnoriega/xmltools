# edit the ebay data to remove description
url <- "http://aiweb.cs.washington.edu/research/projects/xmltk/xmldata/data/auctions/ebay.xml"
download.file(url, "data-raw/ebay.xml")

# remove description node
xml <- XML::xmlInternalTreeParse('data-raw/ebay.xml')
nodeset <- XML::getNodeSet(xml, '//listing//*') # get all top level nodes
nms <- sapply(nodeset, XML::xmlName)
# find nodes to drop by name then create an index
drop <- which(grepl('description', nms))
XML::removeNodes(nodeset[drop]) # drop nodes from pointer

# output
XML::saveXML(xml, "data-raw/ebay.xml")

system("mkdir inst/extdata")
system("cp data-raw/ebay.xml inst/extdata/ebay.xml")
