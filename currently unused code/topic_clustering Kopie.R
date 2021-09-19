library(magrittr) # code piping
library(readr) # fast file reading 
library(dplyr) # basic data wrangling
library(data.table) # basic data operations but fast
library(tidyr)

library(ggplot2) # visualisations
library(crayon) # nice console printing 
library(knitr) 
library(lubridate) # manage date/time data
library(purrr) # vectorization
library(testthat)
library(CausalImpact)
library(httr) # curl stuff / downloading stuff & calling APIs
source("./functions.R")

# df<-read_gdelt_prepared(filename = "./data/ALL_s.csv")
# df %>% saveRDS("./temp/data_prepared.RDS")
df <- readRDS("./temp/data_prepared.RDS")



cooccurrances<-function(x){
  
  if(length(x)>1){
    # print(length(x))
    combn(x, 2) %>% t %>% apply(1,sort) %>% t %>% .[.[,1]!=.[,2],]
  }else{
    matrix(nrow = 0,ncol = 2)
  }
}





# coorcurrance_matrix<-matrix(0, ncol = length(unique_topics),nrow = length(unique_topics),dimnames = list(NULL,unique_topics))

# df$news_item<-1:nrow(df)
# df_long <- df %>% dplyr::select(date,source,not_wb_topics,news_item) %>% unnest(not_wb_topics)
# df_long_subset<-df_long[df_long$news_item<10000,]
# df_long %>% 
#   group_by(news_item) %>% 
#   summarise(combinations = cooccurrances)



# total_topic_n<-counts %>% group_by(values) %>% summarise(n=sum(number_of_occurances))
# ggplot(total_topic_n %>% arrange(desc(n)),aes(x=values,y=n))+geom_point()
  
# cooc_placesholder<-matrix(integer(0),nrow=nrow(df_long_subset),ncol = length(unique_topics))
# colnames(cooc_placesholder)<-unique_topics




dir.create("./temp/topic_cooccurance")
dir.create("./temp/topic_cooccurance_count")
rm(df)
for(i in 1:length(df_sections)){
# ncomb to get all combinations of 2
all_cooc<-map(df_sections[[i]]$not_wb_topics,cooccurrances)
cat(crayon::green(paste("counted - ", round(1/length(df_sections),4)*100,'%')))
# organise data
coocs_counted<-as.data.frame(do.call(rbind,all_cooc))
colnames(coocs_counted)<-c("A","B")
# count identical rows
coocs_counted <- coocs_counted %>% group_by(A,B) %>% summarise(n=n())
# save data
write_rds(all_cooc,paste0('./temp/topic_cooccurance_count//occurances_counts-',i,'.RDS'))
cat(crayon::bgGreen(crayon::white(paste("\nsaved - ", round(1/length(df_sections),2)*100,'%'))))
}


coocs<-map(df$not_wb_topics,cooccurrances) %>% do.call(rbind,.)
saveRDS(coocs,file = "./temp/topic_coocurrance_list.RDS")
toc()
colnames(coocs)<-c("A","B")
tic("count them")

toc()
coocs_counted %>% arrange(desc(n))





df %>% head %>% mutate(coocs=list(cooccurrances(not_wb_topics)))





all_cooc<-map(df$wb_topics,cooccurrances)

df$not_wb_topics %>% lapply(length) %>% unlist %>% sort(decreasing = T)
combinations<-combn(unique_topics,2) %>% t




cooc<-lapply(df$not_wb_topics,paste,collapse=" ") %>% unlist %>% count_cooccurrances %>% 
  filter(freq>1)

cooc %>% arrange(desc(freq)) %>% nrow

library(igraph)

g <- graph_from_edgelist(cooc[,1:2] %>% as.matrix)

E(g)$weight<-cooc$freq

plot(g,vertex.cex=0.3, vertex.label=NA)


count_cooccurrances<-function(x){
dat<- x %>% cbind(1:length(.),.) %>% as.data.frame
colnames(dat)<-c("sentence_id","text")
out <- lapply(with(dat, split(text, sentence_id)), function(x) {
  strsplit(gsub("^\\s+|\\s+$", "", as.character(x)), "\\s+")[[1]]
})

nms <- sort(unique(unlist(out)))

out2 <- lapply(out, function(x) {
  as.data.frame(table(x), stringsAsFactors = FALSE)
})

dat2 <- data.frame(x = nms)

for(i in seq_along(out2)) {
  print(round(i/length(out2),2)*100)
  m <- merge(dat2, out2[[i]], all.x = TRUE)
  names(m)[i + 1] <- dat[["sentence_id"]][i]
  dat2 <- m
}

dat2[is.na(dat2)] <- 0
x <- as.matrix(dat2[, -1]) > 0

out3 <- x %*% t(x)
out3[upper.tri(out3, diag=TRUE)] <- NA
dimnames(out3) <- list(dat2[[1]], dat2[[1]])

out4 <- na.omit(data.frame( 
  word1 = rep(rownames(out3), ncol(out3)),  
  word2 = rep(colnames(out3), each = nrow(out3)),
  freq = c(unlist(out3)),
  stringsAsFactors = FALSE)
)

row.names(out4) <- NULL

out4
}
