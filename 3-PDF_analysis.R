library(tabulizer)
library(pdftools)
library(pdfsearch)
library(stringr)
library(stringi)

setwd("/Users/anttonalberdi/github/microbiota_adaptation_review/")

####
# Define keywords
####
keywords = c('wild', 'fitness', 'adaptation')

####
# Get list of dois
####
doilist <- list.files(path = "papers", pattern = ".pdf",full.names=FALSE)
doilist <- gsub(".pdf","",doilist)

####
# Iterate across dois
####

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
