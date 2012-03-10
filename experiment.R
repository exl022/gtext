library(foreign)
library(XML)
library(RCurl)
library(stringr)
library(R.utils)
library(data.table)


source("htmlToText.R")
source("gtext.R")


# GET DATA AND LOAD INTO DATA.TABLE
    raw <- read.csv("text_sample.csv")
    raw.dt <- data.table(raw)
    length(raw.dt$Category[raw.dt$Category == "Travel & Lodging"])
        # should be 242
    length(raw.dt$Category[raw.dt$Category == "Business"])
        # should be 154
    order <- raw.dt[,length(device_id), by = Category]
    order[order(order$V1, decreasing = T),]
        # this reveals what categories we could try to disambiguate

# CREATE THE SAMPLES FOR TESTING TIME
  bus.sam <- matrix(nrow = 154, ncol = 1)
  bus.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Business"])[ raw$search_phrase[raw$Category == "Business"]])
#  bus.sam[1:10,2] <- apply(bus.sam,1,gtext)


  trav.sam <- matrix(nrow = 242, ncol = 1)
  trav.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Travel & Lodging"])[ raw$search_phrase[raw$Category == "Travel & Lodging"]])


  veh.sam <- matrix(nrow = 460, ncol = 1)
  veh.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Vehicles"])[ raw$search_phrase[raw$Category == "Vehicles"]])

#music
  music.sam <- matrix(nrow = 1845, ncol = 1)
  music.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Arts & Entertainment - Music (+ Lyrics)"])[ raw$search_phrase[raw$Category == "Arts & Entertainment - Music (+ Lyrics)"]])
#science
  sci.sam <- matrix(nrow = 1395, ncol = 1)
  sci.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Science, Social Sciences & Humanities"])[ raw$search_phrase[raw$Category == "Science, Social Sciences & Humanities"]])
#reference
  ref.sam <- matrix(nrow = 1278, ncol = 1)
  ref.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Reference"])[ raw$search_phrase[raw$Category == "Reference"]])
#celebrities
  celeb.sam <- matrix(nrow = 1097, ncol = 1)
  celeb.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Arts & Entertainment - Celebrities"])[ raw$search_phrase[raw$Category ==
  "Arts & Entertainment - Celebrities"]])
# navigation
  nav.sam <- matrix(nrow = 910, ncol = 1)
  nav.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Navigation"])[ raw$search_phrase[raw$Category == "Navigation"]])


sam = length(bus.sam) 
results <- matrix(nrow = sam, ncol = 2)



# bus batch
s <- Sys.time()
for(i in 1:sam){
    results[i,] <- try(gtext(bus.sam[i],3), TRUE)
    }
Sys.time() - s

# trav batch

# define vars
    sam <- length(trav.sam)
    pb <- txtProgressBar(min = 1, max = sam, style = 3)
    results.trav <- matrix(nrow = sam, ncol = 2)
    timeout.time <- 15

# s <- Sys.time()
# for(i in 1:sam){
#     results.trav[i,] <- try(evalWithTimeout({gtext(trav.sam[i])}, 
#     timeout = 7, onTimeout = "warning"), TRUE)
#     # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
#     # skipping)}
#     # update progress bar
#       setTxtProgressBar(pb, i)
#     }
# Sys.time() - s


# THIS IS THE ONE THAT WORKS WITH THE TIMEOUT - no spec on onTimout
s <- Sys.time()
for(i in 1:sam){
    results.trav[i,] <- try(evalWithTimeout({gtext(trav.sam[i],3)}, 
    timeout = timeout.time), TRUE)
    # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
    # skipping)}
    # update progress bar
      setTxtProgressBar(pb, i)
    }
Sys.time() - s
length(results.trav[,1][nchar(results.trav[,1]) == "1"])


# REPLICATION FOR BUSINESS QUERIES

# define vars
    sam <- length(bus.sam)
    pb <- txtProgressBar(min = 333, max = sam, style = 3)
    results.bus <- matrix(nrow = sam, ncol = 2)
    timeout.time <- 15

# THIS IS THE ONE THAT WORKS WITH THE TIMEOUT - no spec on onTimout
s <- Sys.time()
for(i in 1:sam){
    results.bus[i,] <- try(evalWithTimeout({gtext(bus.sam[i],3)}, 
    timeout = timeout.time), TRUE)
    # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
    # skipping)}
    # update progress bar
      setTxtProgressBar(pb, i)
    }
Sys.time() - s
length(results.bus[,1][nchar(results.bus[,1]) == "1"])



# REPLICATION FOR VEHICLES QUERIES
# bogies - 117, 332, 
# define vars
    sam <- length(veh.sam)
    pb <- txtProgressBar(min = 1, max = sam, style = 3)
    results.veh <- matrix(nrow = sam, ncol = 2)
    timeout.time <- 15

# THIS IS THE ONE THAT WORKS WITH THE TIMEOUT - no spec on onTimout
s <- Sys.time()
for(i in 1:sam){
    results.veh[i,] <- try(evalWithTimeout({gtext(veh.sam[i],3)}, 
    timeout = timeout.time), TRUE)
    # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
    # skipping)}
    # update progress bar
      setTxtProgressBar(pb, i)
    }
Sys.time() - s
length(results.veh[,1][nchar(results.veh[,1]) == "1"])


# REPLICATION FOR MUSIC QUERIES
# define vars
    sam <- length(music.sam)
    pb <- txtProgressBar(min = 1, max = sam, style = 3)
    # results.music <- matrix(nrow = sam, ncol = 2)
    timeout.time <- 15

# THIS IS THE ONE THAT WORKS WITH THE TIMEOUT - no spec on onTimout

# bogies 43, 317, 582 
# win bogies 49m 582, 648, 649, 694, 695, 705, 1063, 1078, 1321
s <- Sys.time()
for(i in 1322:sam){
    results.music[i,] <- try(evalWithTimeout({gtext(music.sam[i],3)}, 
    timeout = timeout.time), TRUE)
    # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
    # skipping)}
    # update progress bar
      setTxtProgressBar(pb, i)
    }
Sys.time() - s
length(results.music[,1][nchar(results.music[,1]) == "1"])


# REPLICATION FOR NAVIGATION QUERIES

# navigation
  nav.sam <- matrix(nrow = 910, ncol = 1)
  nav.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Navigation"])[ raw$search_phrase[raw$Category == "Navigation"]])

# define vars
    sam <- length(nav.sam)
    pb <- txtProgressBar(min = 1, max = sam, style = 3)
    # results.nav <- matrix(nrow = sam, ncol = 2)
    timeout.time <- 15

# THIS IS THE ONE THAT WORKS WITH THE TIMEOUT - no spec on onTimout
# trips on 731, 
s <- Sys.time()
for(i in 732:sam){
    results.nav[i,] <- try(evalWithTimeout({gtext(nav.sam[i],3)}, 
    timeout = timeout.time), TRUE)
    # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
    # skipping)}
    # update progress bar
      setTxtProgressBar(pb, i)
    }
Sys.time() - s
length(results.nav[,1][nchar(results.nav[,1]) == "1"])


# REPLICATION FOR CELEB QUERIES

#celebrities
  celeb.sam <- matrix(nrow = 1097, ncol = 1)
  celeb.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Arts & Entertainment - Celebrities"])[ raw$search_phrase[raw$Category ==
  "Arts & Entertainment - Celebrities"]])

# define vars
    sam <- length(celeb.sam)
    pb <- txtProgressBar(min = 1, max = sam, style = 3)
    results.celeb <- matrix(nrow = sam, ncol = 2)
    timeout.time <- 15

# THIS IS THE ONE THAT WORKS WITH THE TIMEOUT - no spec on onTimout
# trips on 386, 459, 460
s <- Sys.time
for(i in 463:sam){
    results.celeb[i,] <- try(evalWithTimeout({gtext(celeb.sam[i],3)}, 
    timeout = timeout.time), TRUE)
    # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
    # skipping)}
    # update progress bar
      setTxtProgressBar(pb, i)
    }
Sys.time() - s
length(results.celeb[,1][nchar(results.celeb[,1]) == "1"])




# REPLICATION FOR QUERIES

#reference
  ref.sam <- matrix(nrow = 1278, ncol = 1)
  ref.sam [,1] <- as.character(levels(raw$search_phrase[raw$Category ==
  "Reference"])[ raw$search_phrase[raw$Category == "Reference"]])

# define vars
    sam <- length(ref.sam)
    pb <- txtProgressBar(min = 1, max = sam, style = 3)
    results.ref <- matrix(nrow = sam, ncol = 2)
    timeout.time <- 15

# THIS IS THE ONE THAT WORKS WITH THE TIMEOUT - no spec on onTimout
# trips on 513, 630
s <- Sys.time
for(i in 631:sam){
    results.ref[i,] <- try(evalWithTimeout({gtext(ref.sam[i],3)}, 
    timeout = timeout.time), TRUE)
    # TimeoutException = function(ex) {cat("Timeout. Skipping","timeout.
    # skipping)}
    # update progress bar
      setTxtProgressBar(pb, i)
    }
Sys.time() - s
length(results.ref[,1][nchar(results.ref[,1]) == "1"])



