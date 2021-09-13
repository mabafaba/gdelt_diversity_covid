

unique_topics<-df$not_wb_topics %>% unlist %>% unique
coorcurrance_matrix<-matrix(0, ncol = length(unique_topics),nrow = length(unique_topics),dimnames = list(NULL,unique_topics))
cooccurrances<-function(x){

if(length(x)>1){
  print(length(x))
combn(x, 2) %>% t %>% apply(1,sort) %>% t %>% .[.[,1]!=.[,2],]
}else{
  matrix(nrow = 0,ncol = 2)
}
}

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
