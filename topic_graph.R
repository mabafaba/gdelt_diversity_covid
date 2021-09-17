library(readr)
library(igraph)
library(purrr)
library(tidyr)
library(dplyr)
library(testthat)
source("functions.R")
library(networkD3)
library(htmlwidgets)

# make graph from edgelist -----------------------------------------------------------

edgelist <- read_csv("./data/gdelt_n.csv",skip_empty_rows = T) %>% dplyr::select(values,count,parent)


# edgelist$parent %>% gsub(///////)



parent_as_R_code <- edgelist$parent %>%
  gsub("main category with [0-9]* children","c('ROOT')",.) %>% 
  gsub("[","c(",fixed = T,.) %>%
  gsub("]",")",fixed = T,.)

edgelist$parent <- parent_as_R_code %>% map(str2lang) %>% map(eval)  
edgelist$row<-1:nrow(edgelist)
edgelist<-tidyr::unnest(edgelist,cols=c(parent))
edgelist$values<-wbtopic_gdelt_to_label(edgelist$values)
edgelist$parent<-tolower(edgelist$parent)

p <- simpleNetwork(edgelist[,c('values','parent')], height="1000px", width="1000px",zoom = T,opacity = 1,linkDistance = 10)
p


    g<-edgelist %>% dplyr::select(from = values, to = parent) %>% as.matrix %>% (igraph::graph_from_edgelist)
E(g)$row<-edgelist$row

# loops:
edgelist[(edgelist$values==edgelist$parent) %>% which,]

# plot graph --------------------------------------------------------------

pdf("full_graph.pdf",width = 30,height = 30)
plot(g,vertex.size = 0.4,
     vertex.color= "#000000AA",
     vertex.label = NA,
     vertex.frame.color=NA,
     vertex.label.color=NA,
     edge.arrow.size = 0.1,
     edge.color="#000000AA",
     vertex.label.size = 0)
dev.off()

# get hierarchical subgraphs ----------------------------------------------


children<-function(x,g){
  neighbors(g,x,mode = 'in')
}

all_descendents<-function(parent, known_descendents = c(), g, max_levels=100,current_level = 0){
  # which children are not yet in descendent list? 
  
  children <- children(parent,g)
  new_children<- children[!(children %in% known_descendents)]
  # add them to the known descendents
  if(length(known_descendents)>0){
  known_descendents<-c(parent,known_descendents, new_children)
  }else{
    known_descendents<-c(parent, new_children)
  }
  known_descendents<-unique(known_descendents)
  # Add all decendents of the new children
  if(length(new_children)==0){return(known_descendents)}
  for(i in 1:length(new_children)){
    if (current_level>=max_levels) {
      warning("reached max recursion level, giving up :(")
      break
    }
    grandchildren <- all_descendents(new_children[i],
                                     known_descendents,
                                     max_levels = max_levels,
                                     g = g,
                                     current_level = current_level + 1)
    known_descendents <- c(known_descendents,grandchildren)
    known_descendents<-unique(known_descendents)
  }
  known_descendents<-unique(known_descendents)
  return(known_descendents)
}

root_children <- all_descendents(V(g)['root'],g = g,max_levels = 0)
jpeg('./outputs/wbthemes/root_one_down.jpg',1500,1500)
plot(subgraph(g,root_children),vertex.size=3,vertex.label.dist = 1,vertex.color = '#000000',vertex.frame.color = '#000000',vertex.label.cex=1,edge.arrow.size=0.2)
dev.off()

root_children <- all_descendents(V(g)['root'],g = g,max_levels = 1)
jpeg('./outputs/wbthemes/root_two_down.jpg',1500,1500)
plot(subgraph(g,root_children),vertex.size=3,vertex.label.dist = 1,vertex.color = '#000000',vertex.frame.color = '#000000',vertex.label.cex=1,edge.arrow.size=0.2)
dev.off()

root_children <- all_descendents(V(g)['root'],g = g,max_levels = 3)
jpeg('./outputs/wbthemes/root_three_down.jpg',1500,1500)
plot(subgraph(g,root_children),vertex.size=3,vertex.label.dist = 1,vertex.color = '#000000',vertex.frame.color = '#000000',vertex.label.cex=1,edge.arrow.size=0.2)
dev.off()

shortest.paths(g,V(g)[])

main_topics<-children("root",g)

for(i in 1:length(main_topics)){
jpeg(paste0("./outputs/wbthemes/",main_topics[i]$name,'.jpg'),1000,1000)
family_g<-all_descendents(main_topics[i],g = g,max_levels = 1000)

plot(subgraph(g,family_g),vertex.size=0.1,vertex.label.cex=0.5,edge.arrow.size=0.2)
dev.off()
}




# subset largest connected component --------------------------------------

components <- igraph::clusters(g, mode="weak")
biggest_cluster_id <- which.max(components$csize)

# ids
vert_ids <- V(g)[components$membership == biggest_cluster_id]

# subgraph
g_largest_comp<-igraph::induced_subgraph(g, vert_ids)
plot(g_largest_comp,vertex.size = 0.4,
     vertex.color= "#000000AA",
     # vertex.label = NA,
     vertex.frame.color=NA,
     vertex.label.color="gray",
     edge.arrow.size = 0.1,
     edge.color="#000000AA",
     vertex.label.cex = 0.2,layout= layout_components)
