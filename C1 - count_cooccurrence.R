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
library(httr) # curl stuff / downloading stuff & calling APIs
source("./functions.R")


# get data ----------------------------------------------------------------

# df<-read_gdelt_prepared(filename = "./data/ALL_s.csv")
# df %>% saveRDS("./temp/data_prepared.RDS")
df <- readRDS("./temp/data_prepared.RDS")

# define chunks -----------------------------------------------------------

max_rows_at_once <- 10000
section_ids <- 1:nrow(df)
df_sections <- split.data.frame(df, ceiling(section_ids/max_rows_at_once))
rm(df) # get rid of df to save RAM

# cooccurrance function ---------------------------------------------------

cooccurrances<-function(x){
  
  if(length(x)>1){
    # print(length(x))
    combn(x, 2) %>% t %>% apply(1,sort) %>% t %>% .[.[,1]!=.[,2],]
  }else{
    matrix(nrow = 0,ncol = 2)
  }
}


# run ---------------------------------------------------------------------

# dir.create("./temp/topic_cooccurance")
dir.create("./temp/topic_cooccurance_count")
rm(df)

# combn -------------------------------------------------------------------

for(i in 1:length(df_sections)){
  # ncomb to get all combinations of 2
  all_cooc<-map(df_sections[[i]]$not_wb_topics,cooccurrances)
  cat(crayon::green(paste("counted - ", round(i/length(df_sections),4)*100,'%')))
  write_rds(all_cooc,paste0('./temp/topic_cooccurance_count//occurances_counts-',i,'.RDS'))
  cat(crayon::bgGreen(crayon::white(paste("\nsaved - ", round(i/length(df_sections),2)*100,'%'))))
}


# all together ------------------------------------------------------------


cooc_count_files <- list.files("./temp/topic_cooccurance_count/",full.names = T)
dir.create("./temp/cooccurrances_n")
for(i in c(1:length(cooc_count_files))){
  combinations <- readRDS(cooc_count_files[i]) %>% do.call(rbind,.) %>% as_tibble(.name_repair = 'minimal')
  colnames(combinations) <-c("A","B")
  combinations_counted <- combinations %>% tibble %>% group_by(A,B) %>% summarise(n=n())
  write_rds(combinations_counted,paste0("./temp/cooccurrances_n/cooc_",i,".RDS"))
}
combinations %>% unique %>% length

