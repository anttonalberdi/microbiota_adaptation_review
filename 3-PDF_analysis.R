library("pdftools")
library("pdfsearch")

setwd("/Users/anttonalberdi/github/microbiota_adaptation_review/")

#Keyword list
keywords = c('wild', 'fitness', 'adaptation')

#Study
doi="10.3389_fimmu.2020.585431"

#Screen PDF
pdffile <- pdftools::pdf_text(paste0("papers/",doi,".pdf"),split_pdf=TRUE)
key_res <- keyword_search(pdffile, keyword = keywords)


headings <- heading_search(pdffile, headings = c('METHODS', 'INTRODUCTION')) 

#Keyword matrix
table(key_res[1])

#Save extracts to table
keyword_table <- cbind(key_res$keyword,unlist(key_res$line_text))
write.csv(keyword_table,paste0("extracts/",doi,".csv"))