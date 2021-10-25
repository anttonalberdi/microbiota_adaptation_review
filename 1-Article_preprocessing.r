setwd("/Users/anttonalberdi/github/microbiota_adaptation_review/")

#WOS needs to be exported as Excel and then exported as comma-separated csv

##############
# 1- PREPARE DATA TABLES
##############

#Load raw data tables
scp_raw <- read.csv("data/scp_20211025.csv")
wos_raw <- read.csv("data/wos_20211025.csv")

#Filter columns
scp <- scp_raw[,c("Title","Authors","Year","Document.Type","DOI","Abstract","Author.Keywords","Index.Keywords")]
colnames(scp) <- c("Title","Authors","Year","Type","DOI","Abstract","Keywords1","Keywords2")
wos <- wos_raw[,c("Article.Title","Authors","Publication.Year","Document.Type","DOI","Abstract","Author.Keywords","Keywords.Plus")]
colnames(wos) <- c("Title","Authors","Year","Type","DOI","Abstract","Keywords1","Keywords2")

#Unify formats
scp[,2] <- gsub("\\.","",scp[,2])
wos[,2] <- gsub(",","",wos[,2])
wos[,2] <- gsub(";",",",wos[,2])

#Merge datasets
all <- rbind(scp,wos)

#Merge keywords
all$Keywords <- paste(all$Keywords1,all$Keywords2,sep="; ")

#Merge text case
all$Keywords <- tolower(all$Keywords)

#Rename and order
all <- all[,c("Title","Authors","Year","Type","DOI","Abstract","Keywords")]
all <- all[order(all$Title),]

##############
# 2- FILTER DUPLICATES
##############

#Filter by duplicated Title
all.uniq <- all[!duplicated(all[,"Title"]),]

#Filter by duplicated DOI
all.uniq <- all.uniq[!duplicated(all.uniq[,"DOI"]),]

#Filter by duplicated Abstract
all.uniq <- all.uniq[!duplicated(all.uniq[,"Abstract"]),]

##############
# 3- FILTER BY DOCUMENT TYPE
##############

#Print all document types
unique(all.uniq$Type)

#Filter entries
all.filt_type <- all.uniq[all.uniq$Type %in% c("Article","Letter","Article in Press","Note","Short Survey","Reprint"),]

write.csv(all.filt_type, "data/all_20211025.csv", row.names=FALSE)

##############
# 4- GET STATS
##############

#Raw
nrow(all)
#After duplicate removal
nrow(all.uniq)
#After manuscript type filtering
nrow(all.filt_type)




##############
# 4- FILTER BY KEYWORDS (to be updated after machine learning)
##############

allkeywords <- paste(shQuote(all.filt_type$Keywords), collapse="; ")
allkeywords <- gsub("\"","",allkeywords)
allkeywords <- sort(unlist(strsplit(allkeywords, split="; ")))
allkeywordscounts <- as.data.frame(table(allkeywords))
allkeywordscounts <- allkeywordscounts[order(allkeywordscounts[,2],decreasing=TRUE),]

#Display 100 most common keywords
head(allkeywordscounts,100)

#List of keywords to remove
keywords_to_remove <- c("mice, inbred c57bl","mice, knockout","c57bl mouse","knockout mouse","disease models, animal","colitis","disease model")

#Filter by keywords
library(dplyr)
library(stringr)
all.filt_keywords <-  filter(all.filt_type, !str_detect(Keywords, paste(keywords_to_remove, collapse="|")))
