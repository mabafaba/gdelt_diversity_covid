

library(readr) # fast file reading 
library(dplyr) # basic data wrangling
library(tidyr)
library(ggplot2) # visualisations
library(crayon) # nice console printing 
library(knitr) 
library(lubridate) # manage date/time data
library(purrr) # vectorization
library(testthat)
source("./functions.R")

# read all files
files <- list.files("./data",recursive = T,full.names = T)
df<-map(files,read_gdelt)
# combine them into one data frame
df <- do.call(bind_rows,df)
# remove odd sources
df <- df %>% filter(source %in% c("bbc.com","foxnews.com","cnn.com","theguardian.com"))



df$wb_topics <- df$X9 %>% lapply(function(x){grep("^WB_",x,value=T)}) # extract world bank standard topic codes only
unique_wb_topcs<-unique(unlist(df$wb_topics))
df$not_wb_topics <- df$X9 %>% lapply(function(x){x[!(x %in% unique_wb_topcs)]}) # sorry for this weird line. Just picks out the topics that are not in wb_topics.


# take subset so that all news sources on all days are equally represented
# n_max<-paste0(df$source,df$date) %>% table %>% min
# df <- df %>% group_by(source,date) %>% filter((1:n())<=n_max)

# count unique topics occurances by news outlet
counts <- df  %>%
  group_by(date,source) %>% # for each day...
  summarise(topic_count = count_occurances_across_list(not_wb_topics)) # count each topic's frequency

# unnest data frame
counts$values <- counts$topic_count$values
counts$number_of_occurances <- counts$topic_count$number_of_occurances
counts$topic_count<-NULL




totals <- counts %>% group_by(date = week(date),source) %>% summarise(number_of_topic_appearances = sum(number_of_occurances))

ggplot(totals,
       aes(x=date,
           y=number_of_topic_appearances,
           col=source))+
  geom_line()+
  theme_minimal()+
  geom_point()+
  ggtitle("Total Number of topics noted across articles per Source (Weekly)")+
  xlab("week")+
  ylab("# of topics")



# article counts ----------------------------------------------------------

df %>%
  group_by(date=date,source) %>% summarise(article_count = n()) %>%
  ggplot(aes(x=date,y=article_count,col=source)) +
  geom_line()+
  theme_minimal()+
  ggtitle("Number of articles per Source (Daily)")+
  ylab("# of articles")

df %>%
  group_by(date=week(date),source) %>% summarise(article_count = n()) %>%
  ggplot(aes(x=date,y=article_count,col=source)) +
  geom_line()+
  theme_minimal()+
  ggtitle("Number of articles per Source (Weekly)")+
  xlab("week")+
  ylab("# of articles")

# how many topics per article? --------------------------------------------

df %>%
  rowwise %>%
  mutate(topic_count = length(not_wb_topics)) %>%
  group_by(source) %>%
  summarise(mean = mean(topic_count),
            sd=sd(topic_count)) %>%
  kable


# entropy (weekly) -----------------------------------------------------------------


entropy <- counts %>%
  group_by(date = week(date),source) %>%
  summarise(entropy = shannon_entropy(number_of_occurances), # entropy
            relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
            num_unique_topics = n(),
            num_articles = sum(number_of_occurances),
            occuring_topics = paste(values,collapse = "; ") # list topics that appeared
  ) # number of unique topics found


ggplot(entropy,aes(x = date, y= entropy,col = source))+
  # scale_y_log10()+
  geom_line()+
  theme_minimal()+
  geom_point()+
  ggtitle("Topic Entropy by Source (Weekly)")+
  xlab("week")+
  ylab("Shannon Entropy")

# entropy (daily) -----------------------------------------------------------------


entropy <- counts %>%
  group_by(date,source) %>%
  summarise(entropy = shannon_entropy(number_of_occurances), # entropy
            relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
            num_unique_topics = n(),
            num_articles = sum(number_of_occurances),
            occuring_topics = paste(values,collapse = "; ") # list topics that appeared
  ) # number of unique topics found


par(mfrow=c(2,2))

ggplot(entropy,aes(x = date, y= entropy,col = source))+
  # scale_y_log10()+
  geom_line()+
  theme_minimal()+
  geom_point()+
  ggtitle("Topic Entropy by Source (Daily)")+
  xlab("date")+
  ylab("Shannon Entropy")



# topic stickiness ---------------------------------------------------------

df

counts_weekly <- counts %>% group_by(date = week(date), source, values) %>% summarise(number_of_occurances=sum(number_of_occurances))

counts %>%  filter(values == "TAX_FNCACT") %>% ggplot(aes(x=date,y=number_of_occurances,col=source))+geom_line()
counts %>% group_by(values) %>% summarise(n=sum(number_of_occurances)) %>% 
  filter(n>10, number_of_occurances<40) %>% arrange(desc(number_of_occurances))

counts_weekly %>% arrange(desc(number_of_occurances)) %>% head(50) %>% kable

counts_weekly %>% filter(values == "TAX_FNCACT") %>% kable








# download covid cases ----------------------------------------------------
library(magrittr)

cases<-httr::GET("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") %>%
  httr::content(type = "text/csv")
cases %<>% arrange(date)

cases_diff<-cases[-1,]
cases_diff$cases<-cases$cases[-1] - cases$cases[-nrow(cases)]
cases_diff$deaths<-cases$deaths[-1] - cases$deaths[-nrow(cases)]
cases_diff$source<-" covid cases"
cases_diff$what<-"Daily new covid cases"
cases$what<-"total covid cases"
entropy$what<-"entropy (weekly)"
ggplot(bind_rows(entropy,cases_diff),aes(x = date, y= entropy,col = source))+
  # scale_y_log10()+
  geom_line()+
  theme_minimal()+
  # geom_point()+
  ggtitle("US News Topic Entropy and Covid")+
  xlab("date")+
  ylab("")+
  geom_line(aes(x=date,y=cases))+
  facet_grid(rows = 'what',scales = "free_y")
  
  
content<-read_gdelt("/Volumes/2/content_diversity_data_2017_2021/20170101.csv",small = F)
content[1:10,25]
content %>% colnames
ggplot(cases)+geom_line(aes(x=date,y=deaths))+theme_minimal()
cases
counts$about_covid<- grepl("covid|coronav|pandemic|health|virus",counts$values,ignore.case = T)

entropy_covid_separated<-counts %>% split.data.frame(counts$about_covid) %>% lapply(entropy_from_counts)
entropy_covid_separated[[1]]$is_about_covid<-"other"
entropy_covid_separated[[2]]$is_about_covid<-"health / covid"
entropy_covid_separated<-do.call(rbind,entropy_covid_separated)

ggplot(entropy_covid_separated,aes(x=date,y=entropy,col=source))+
  geom_line()+
  facet_grid(rows = vars(is_about_covid),scales="free_y")

corona_topics <- grep("covid|coronav|pandemic|health",counts$values,value = T,ignore.case = T) %>% unique

covid_counts<-counts %>%
  filter(values %in% corona_topics) %>%
  group_by(source,year = year(date),week = week(date)) %>%
  summarise(n = sum(number_of_occurances),
            date=min(date))
  
ggplot(covid_counts,
       aes(x=date,y=n,col=source))+
  geom_line()+theme_minimal()+ggtitle("how often does covid appear as topic?")+scale_x_date()

class(covid_counts$date)
corona_only 




# within topic diversity --------------------------------------------------

files <- list.files("/Volumes/2/content_diversity_data_2017_2021/",pattern = "\\.csv$",recursive = T,full.names = T)

all_topic_combinations<-list()
for(f in files[1:2]){
  print(f)
  all_topic_combinations<- c(all_topic_combinations,
                             f %>% read_gdelt %>% .$wb_topics)
}
unique_wb_topics<-all_topic_combinations %>% unlist %>% unique %>% sort(decreasing = F)

unique_wb_topics %>% grep("WB_51",.,value=T, fixed =T)

rdf<-rdf_parse("http://vocabulary.worldbank.org/taxonomy/1079.rdf")
rdf_all<-rdf_parse("~/Downloads/taxonomy_WB.rdf")

sparql <-
  'PREFIX dc: <http://vocabulary.worldbank.org/taxonomy/>
SELECT ?predicate
WHERE {
?subject ?predicate ?object .
} LIMIT 1000'

sqarql<-function(query,rdf){
 query<- paste('PREFIX dc: <http://vocabulary.worldbank.org/taxonomy/>
               PREFIX broader: <http://www.w3.org/2004/02/skos/core#broader>',query)
  rdf_query(rdf, query)
}
sqarql(query="
       
       SELECT *
WHERE {
       broader:label
       }
       LIMIT 200

       ",rdf)

full <- sqarql(query="
       
       SELECT ?s ?p ?o
       WHERE {
       ?s ?p ?o .
       }
       ",rdf_all)


nrow(full)
broader<-full$p %>% grep("broader",.) 
narrower<-full$p %>% grep("narrower",.) 
full_bn <- full[c(broader,narrower),]
full_bn_tax<-full_bn[grepl("http://vocabulary.worldbank.org/taxonomy/",full_bn$s,fixed = T) & grepl("http://vocabulary.worldbank.org/taxonomy/",full_bn$o,fixed = T),]

full_bn_tax$






