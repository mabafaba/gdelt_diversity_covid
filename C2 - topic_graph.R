library(purrr)
library(dplyr)
library(igraph)


# GKG theme lookup table --------------------------------------------------
gkg_categories<- read_csv('./data/GDELT-Global_Knowledge_Graph_CategoryList.csv')

# read cooccurance counts from temp file  ---------------------------------

coocs <- readRDS("./temp/cooccurrances_n_overall.RDS")



# subet -------------------------------------------------------------------

edgelist <- coocs %>% arrange(desc(n))
total_cooccurrances <- sum(coocs$n)
cat("total number of coccurrances in million:")
cat(round(sum(coocs$n)/1000000))
cat("subset number of coccurrances in million:")
cat(round(sum(edgelist$n)/1000000))

colnames(edgelist) <-c("from","to","count")
edgelist$weight <- edgelist$count

rescale_from_0_to_1 <- function(x){
  (x-min(x))/max(x)
}

edgelist$plot_weight <- rescale_from_0_to_1(edgelist$count) 


g<-graph_from_data_frame(edgelist,FALSE)
# plot_graph_quickly(g,edge.width = 5*E(g)$plot_weight)
# plot.igraph(g,vertex.size=1,vertex.label = NA,edge.width = 5*E(g)$plot_weight)

# edge betweenness not feasible on large graph
# also igraph apparently calculates modularity wrong 
# comunities_g <- igraph::edge.betweenness.community(g,weights = 1 / E(g)$weight,directed = FALSE,merges = T,modularity = FALSE)

# using fastgreedy 'cause I'm greedy and I want fast (and there are 16k nodes and a million edges)
communities_g <- igraph::fastgreedy.community(g)
write_rds(communities_g,'./temp/C2_graph_communities.RDS')

# plot_dendrogram(communities_g)



# topics in communities ---------------------------------------------------

# counts <- readRDS('./temp/data/ALL_COUNTS_1.RDS')
counts_global<-counts %>% group_by(values) %>% summarise(n = sum(number_of_occurances))
communities_df<- tibble(topic = communities_g$names,membership = communities_g$membership)
counts_global$community <-communities_df$membership[match(counts_global$values,communities_df$topic)]

top_n<-5
top_topics_in_each_community <- counts_global %>% arrange(desc(n)) %>% 
  group_by(community) %>% 
  summarise(values=first(values,top_n),n=first(n,top_n),community = first(community,top_n))


GKG_labels <- gkg_categories$Description[match(top_topics_in_each_community$values,gkg_categories$Name)]
top_topics_in_each_community$values[!is.na(GKG_labels)]<-GKG_labels[!is.na(GKG_labels)]
top_topics_in_each_community %>% split.data.frame(top_topics_in_each_community$community) %>% do.call(cbind,.) %>% write.csv("./temp/communities_top_topics.csv")
pdf("C2 - fastgreedy_modularity.pdf",width = 10,height = 10)
density(communities_g$modularity) %>% plot(log="y")

# WARNING
# Modularity calculation with weighted edge betweenness community detection might not make sense -- modularity treats edge weights as similarities while edge betwenness treats them as distances
counts$major_topic <- communities_df$membership[match(counts$values,communities_df$topic)]
major_names <- top_topics_in_each_community %>% group_by(community) %>% summarise(label = paste(values,collapse =  " / "))                                                
counts$major_topic_label<-major_names$label[counts$major_topic]                                         
counts$major_topic_label_own<- paste(c("Conflict, Protest, Religion, ...",
                                       "Nature, Oceans, Natural Desaster ...",
                                       "Politics, Leaders, Institutions, ...",
                                       "strange mix including pandemics"))[counts$major_topic]                                                
      
                                          

