setwd("/Users/anttonalberdi/github/microbiota_adaptation_review/")

all <- read.csv("data/all_20211025.csv")

##############
# 1 - Get random articles
##############

all.random <- all[sample(1:nrow(all),200),]
write.csv(all.random,"data/all_20211025_random200.csv",row.names=FALSE)

##############
# 2 - Manually inspect whether they match criteria
##############

##############
# 3 - Load and modify response data
##############
all.random <- read.csv("data/all_20211025_random200.csv")
all.random <- all.random[,c("Title","DOI","Keywords")]

#Get all keywords
allkeywords <- paste(shQuote(all.random$Keywords), collapse="; ")
allkeywords <- gsub("\"","",allkeywords)
allkeywords <- unique(sort(unlist(strsplit(allkeywords, split="; "))))

#Create keyword matrix
all.matrix <- matrix(ncol=length(allkeywords)+3,nrow=nrow(all.random))
colnames(all.matrix) <- c(c("Title","DOI","Result"),allkeywords)
rownames(all.matrix) <- all.random$DOI

##############
# 4 - Create keyword model
##############
