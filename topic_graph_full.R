library(readr)
library(igraph)
library(purrr)
library(tidyr)
library(dplyr)
library(testthat)
source("functions.R")
library(networkD3)
library(htmlwidgets)
library(magrittr)


# set up data -------------------------------------------------------------


edgelist <- read_csv("./data/all_tax_df.csv",skip_empty_rows = T)[,-1] 
old_names<-read_csv('./data/old_names.csv')[,-1]
colnames(old_names)<-c("new",'old')

# parse columns
edgelist <- edgelist %>% mutate(parent = paste0("c",parent),
                    child = paste0("c",child))

edgelist$parent_parsed <- lapply(edgelist$parent,parse,file=NULL,n=NULL) %>% lapply(eval)
parents_twocolumns<-edgelist$parent_parsed %>% do.call(rbind,.)
edgelist$parent<-parents_twocolumns[,2]
edgelist$parent_code<-parents_twocolumns[,1]
# same thing for child column
edgelist$child_parsed <- lapply(edgelist$child,parse,file=NULL,n=NULL) %>% lapply(eval)
childs_twocolumns<-edgelist$child_parsed %>% do.call(rbind,.)
edgelist$child<-childs_twocolumns[,2]
edgelist$child_code<-childs_twocolumns[,1]

# unify old and new WB topic names
names_updated<-old_names$new[match(edgelist$parent,old_names$old)]
edgelist$parent[!is.na(names_updated)]<-names_updated[!is.na(names_updated)]

names_updated<-old_names$new[match(edgelist$child,old_names$old)]
edgelist$child[!is.na(names_updated)]<-names_updated[!is.na(names_updated)]

edgelist$child %>% table %>% sort(T) %>% head

# add root node (no longer needed, theres a root node called "Topics" now)
top_level_topics <- edgelist$parent[which(!(edgelist$parent %in% edgelist$child))] %>% unique

edgelist<- edgelist %>% bind_rows(
  tibble(parent = "root", child = top_level_topics,parent_parsed = NA,parent_code = NA,child_parsed = NA,child_code = NA)
)
edgelist<-edgelist %>% filter(!(parent=="Topics"),!(child=="Topics"))
# make graph --------------------------------------------------------------
g <- edgelist %>% dplyr::select(from = child, to = parent) %>% as.matrix %>% (igraph::graph_from_edgelist)


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


# cluster -----------------------------------------------------------------

# lets say.... each node belongs to the main topic to which it has the the shortest path
# on tie, pick the first (?) 
parents<- function(x,g){
  neighbors(g,x,mode = 'out')
}
main_topics<-children("root",g)
g_no_root<-subgraph(g,V(g)[V(g)$name!='root'])

dists<-distances(g_no_root,v = V(g_no_root)[main_topics], to =  V(g_no_root),mode = "all") 
main_topic_groups<-data.frame(topic = colnames(dists),main_topic = rownames(dists)[apply(dists,2,which.min)])

vertex_groups <- main_topic_groups[match(V(g)$name,main_topic_groups$topic),'main_topic'] %>% as.character
g <- set_vertex_attr(g, "main_group", value = vertex_groups)

V(g)$color  = rainbow(length(unique(V(g)$main_group)))[match(V(g)$main_group,unique(V(g)$main_group))]


pdf("full_graph_main_groups.pdf",width = 30,height = 30)
plot(g,vertex.size = 0.4,
     vertex.color= V(g)$color,
     vertex.label = NA,
     vertex.frame.color=NA,
     vertex.label.color=NA,
     edge.arrow.size = 0.1,
     edge.color="#000000AA",
     vertex.label.size = 0)
dev.off()


# simple tree graph --------------------------------------------------------

g_minimal<-minimum.spanning.tree(g)
pdf("full_graph_mst.pdf",width = 30,height = 30)
plot(g_minimal,vertex.size = 0.4,
     vertex.color= V(g)$color,
     vertex.label = NA,
     vertex.frame.color=NA,
     vertex.label.color=NA,
     edge.arrow.size = 0.1,
     edge.color="#000000AA",
     vertex.label.size = 0)
dev.off()
# entropy by group --------------------------------------------------------
components<-decompose(g,'weak',min.vertices = 1)
microwork
old_names$new<-tolower(old_names$new)
old_names$old<-tolower(old_names$old)

counts$values
themes_in_graph<-unique(c(edgelist$parent,edgelist$child)) %>% tolower
(unique(counts$values) %in% themes_in_graph) %>% table

unique(counts$values) [!(unique(counts$values) %in% themes_in_graph)]

counts <- readRDS('./temp/data/ALL_COUNTS_1.RDS')


counts$values<-wbtopic_gdelt_to_label(counts$values)
main_topic_groups$topic<-tolower(main_topic_groups$topic %>% as.character)
main_topic_groups$main_topic<-tolower(main_topic_groups$main_topic %>% as.character)
main_topic_groups<-main_topic_groups %>% as_tibble

(counts$values[!(counts$values %in% main_topic_groups$topic)] %>% length ) / nrow(counts)


