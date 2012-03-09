# maiden branch?
# get libraries
    library(RCurl)
    library(XML)
    library(stringr)
    source("htmlToText.R") 


# variables  NOTE - SHOULD INTEGRATE THIS INTO THE GTEXT FUNCTION
    link.threshold <- 3


gtext <- function(search.string){
# grab google search results
  search.results <- getForm("http://www.google.com/search", hl="en",
                    lr="", q = search.string, btnG="Search")
    # if you wan to parse this into a tree:
        # search.results.tree <- htmlTreeParse(search.results, useInternal = TRUE); rm(site)

# take out the top links from the search result
    # define the regex to get the links:
        reg1 <- "(?<=hlprwt\\(this\\,\\s\').+?(?=\'\\))"
        # 2012/03/06 - need a new mask to get the scraping right on google.  did
        # they change how results are returned?
        reg1a <- "  " 
    # extract weblinks from the google search page - use perl regex
        # create a matrix that stores the location information for the URLs
        site.loc <- gregexpr(reg1,search.results, perl = T)
        site.mat <- cbind(site.loc[[1]], unlist(attributes(site.loc[[1]])))
        row.names(site.mat) <- NULL
        colnames(site.mat) <- c("loc","length")
        # create a storage vector for the URLs
            url.stor <- matrix(data = NA, nrow = link.threshold, ncol = 2)
            for(i in 1:link.threshold){
               url.stor[i,1] <- substring(search.results,
               site.mat[i,"loc"],site.mat[i,"loc"] + site.mat[i,"length"] - 1)
               }

        # take out all .pdf references
            idx.keep.pdf <- grep('.pdf', url.stor[,1], perl = T, invert = T)
            url.stor <- as.matrix(url.stor[idx.keep.pdf,])

        # grab the text content from top [link.thredhold] URLS
        max.len <- length(url.stor[!is.na(url.stor) ==T]) 
        url.stor2 <- matrix(data = "",nrow = 5, ncol = 1)
            # create a matrix to store the results in - 5 units long
            for(i in 1:max.len){
                # get the text content from the URLs (top [link.thresdhold])
                    url.stor2[i] <- htmlToText(url.stor[i,1])
                # drop the returns
                #    url.stor2[i] <- gsub("[\n\t\r]","",url.stor2[i])
                }
    # create a single record for that search, with all the text from the top
    # [link.thresdhold] from the search.string
        text <- paste(url.stor2[1], url.stor2[2], url.stor2[3], 
        url.stor2[4], url.stor2[5], sep = " ")
            # this was hand hard coded - okay, since you don't have to be 5
            # clicks long, as long as it is 5 or shorter.
        output <- cbind(max.len, text) 
            # takes the number of links used and the actual text extracted from
            # that

        return(output)
        }

