
max_length<-df$top_only %>% sapply(length) %>% max

df$not_wb_topics_same_length<-lapply(df$top_only,function(x){length(x)<-max_length;x})

unique_topics<-unique(unlist(df$top_only))
topic_matrix_top<-matrix(unlist(df$not_wb_topics_same_length),ncol = max_length,nrow = nrow(df),byrow = T)
levels_matched_boolean_top<-apply(topic_matrix_top, 1,`%in%`,x = unique_topics) 
levels_matched_boolean_top %>% str
test <-create.N.matrix.custom(levels_matched_boolean_top)
colnames(test)<-top_topics
rownames(test)<-top_topics
top_topics %>% str

levels_matched<-apply(topic_matrix[1:100,], 1,match,x = unique(topic_matrix)) 


topic_matrix <- matrix(unlist(df$not_wb_topics_same_length),ncol = max_length,nrow = nrow(df),byrow = T)
topic_matrix_factors<-factor(topic_matrix,levels = unique(unlist(df$not_wb_topics)))

object.size(topic_matrix_factors)

topic_matrix_factor<-factor(topic_matrix,levels= unique(topic_matrix))

saveRDS(topic_matrix_factors,"./temp/topicmatrixfactors.RDS")
str(topic_matrix)
object.size(topic_matrix)

test<-matrix(1:50,10,5,T)
test_levels<-1:100
match(test_levels,test[1,])

levels_matched<-apply(topic_matrix[1:10,], 1,match,x = unique(topic_matrix)) 
topic_matrix[1:10,1:50]
topic_matrix %>% str

topic_counts<-table(topic_matrix) %>% sort(decreasing = T)
top_n_covering <- (topic_counts %>% sort(decreasing = T) %>% head(100) %>% sum)/length(unlist(df$not_wb_topics))
top_topics<-names(topic_counts%>%sort(decreasing = T) %>%  head(100))

df$top_only<-df$not_wb_topics %>% lapply(function(x){x[x%in%top_topics]})




topic_counts %>% sort %>% head

levels_matched_boolean<-apply(topic_matrix[1:100,], 1,`%in%`,x = unique_topics) 

cooccurance_matrix <- create.N.matrix.custom(levels_matched_boolean)

cooccurance_matrix %>% str

create.N.matrix.custom <- function (mat) 
{
  nspp <- nrow(mat)
  Nmat <- matrix(nrow = nspp, ncol = nspp)
  row <- 0
  for (spp in 1:nspp) {
    print(spp/nspp)
    if (spp < nspp) {
      for (spp_next in (spp + 1):nspp) {
        Nmat[spp, spp_next] <- sum(mat[spp, ] * mat[spp_next, 
                                                    ])
        Nmat[spp_next, spp] <- sum(mat[spp, ] * mat[spp_next, 
                                                    ])
      }
    }
  }
  return(Nmat)
}





test <- apply(topic_matrix[1:100,],1,match,x = unique_topics)
rownames(test)<-unique_topics


levels_matched_boolean %>% str
test

df$top_only<- lapply(df$top_only,factor,levels=unique(df$top_only))

library(cooccur)
finches %>% str

N_matrix <- matrix(data = rbinom(n = nrow(finches)*ncol(finches),1,prob = 0.75),
                       nrow = nrow(finches),
                       ncol = ncol(finches)
                       ,byrow = T)

N_matrix <- matrix(data = rbinom(n = nrow(finches)*ncol(finches),1,prob = 0.75),
                   nrow = nrow(finches),
                   ncol = ncol(finches)
                   ,byrow = T)







