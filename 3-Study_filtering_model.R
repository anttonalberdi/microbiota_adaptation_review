library(tabulizer)
library(pdftools)
library(pdfsearch)
library(stringr)
library(stringi)

setwd("/Users/anttonalberdi/github/microbiota_adaptation_review/")

####
# Define keywords
####
keywords = c('wild', 'fitness', 'adaptat','mutant','inbred','model','c57bl','knockout','disease','environment','human','laboratory')

####
# Get list of DOIs for the modelling
####

model_data <- read.csv("data/all_20211025_random200.csv")
DOIs <- model_data$DOI
DOIs <- gsub("/","_",DOIs)

####
# Iterate across dois
####

statstable <- c()

for(doi in doilist){

    print(paste0("Processing paper: ",doi))

    #Extract text
    pdftext <- c()
    tryCatch({
        pdftext <- tabulizer::extract_text(paste0("papers/",doi,".pdf"))
      }, error=function(e){})
    if(length(pdftext)==0){
    print("         ERROR")
    }else{

        #Correct errors
        #Remove line breaks
        pdftext <- gsub("\n"," ",pdftext)

        #Change tabs to spaces
        pdftext <- gsub("\t"," ",pdftext)

        #Correct hyphens
        pdftext <- gsub("- ","",pdftext)

        #Correct multi-spaces (iterate until no double-spaces are found)
        spacecount <- str_count(pdftext, "  ")

        while(spacecount > 0){
        pdftext <- gsub("  "," ",pdftext)
        spacecount <- str_count(pdftext, "  ")
        }

        #Extract keywords
        key_res <- c()
        tryCatch({
        key_res <- keyword_search(pdftext, keyword = keywords)
        }, error=function(e){})
        if(is.null(key_res)){
          print("         ERROR")
        }else{
          #Write document
          write.table(pdftext,paste0("texts/",doi,".txt"),col.names=FALSE,row.names=FALSE)

          #Get keyword statistics
          stats <- t(t(table(key_res[1])))
          colnames(stats) <- "Counts"
          write.csv(stats,paste0("extracts/",doi,".txt"))

          #Save extracts to table
          keyword_table <- cbind(key_res$keyword,unlist(key_res$line_text))
          colnames(keyword_table) <- c("Keyword","Context")
          write.csv(keyword_table,paste0("extracts/",doi,".csv"))

        }
    }
}

####
# Get statistics
####

doilist <- list.files(path = "extracts", pattern = ".txt",full.names=FALSE)
doilist <- gsub(".txt","",doilist)

#Create empty stats table
statstable <- c()

for(doi in doilist){
stats <- read.table(paste0("extracts/",doi,".txt"),header=TRUE,sep=",")

#Get stats
countsvector <- c()
for(key in keywords){
  counts <- stats[stats$X == key,"Counts"]
  if(length(counts)==0){counts=0}
  countsvector <- c(countsvector,counts)
}

#Add stats to table
statstable <- rbind(statstable,countsvector)
}

colnames(statstable) <- keywords
rownames(statstable) <- doilist

####
# Create contingency table
####

all_stats <- lapply(list.files(path = "extracts", pattern = ".txt",  full.names = TRUE), function(x){read.csv(x,row.names=1)})

#declare multimerge function
multimerge <- function (mylist) {
  ## mimics a recursive merge or full outer join
  unames <- unique(unlist(lapply(mylist, rownames)))
  n <- length(unames)
  out <- lapply(mylist, function(df) {
    tmp <- matrix(nr = n, nc = ncol(df), dimnames = list(unames,colnames(df)))
    tmp[rownames(df), ] <- as.matrix(df)
    rm(df); gc()
    return(tmp)
  })
  stopifnot( all( sapply(out, function(x) identical(rownames(x), unames)) ) )
  bigout <- do.call(cbind, out)
  colnames(bigout) <- paste(rep(names(mylist), sapply(mylist, ncol)), unlist(sapply(mylist, colnames)), sep = "_")
  return(bigout)
}

all_stats_matrix <- multimerge(all_stats)
colnames(all_stats_matrix) <- gsub(".txt","",list.files(path = "extracts", pattern = ".txt",  full.names = FALSE))
