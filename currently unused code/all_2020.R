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

# # read all files
# # files <- list.files("./data/2020/",recursive = T,full.names = T)
# files <- list.files("/Volumes/2/content_diversity_news_2020/",recursive = T,full.names = T)
# df<-map(files,read_gdelt)
# # combine them into one data frame
# df <- do.call(bind_rows,df)
# # remove odd sources
# df <- df %>% filter(source %in% c("bbc.com","foxnews.com","cnn.com","theguardian.com"))

# df <- read_rds('./data_temp/data_compiled.RDS')


df$wb_topics <- df$X9 %>% lapply(function(x){grep("^WB_",x,value=T)}) # extract world bank standard topic codes only
unique_wb_topcs<-unique(unlist(df$wb_topics))
df$not_wb_topics <- df$X9 %>% lapply(function(x){x[!(x %in% unique_wb_topcs)]}) # sorry for this weird line. Just picks out the topics that are not in wb_topics.

# show topic frequency frequency
df$not_wb_topics %>% unlist %>% table %>% paste("topic appears",.,"times") %>% table %>% sort(decreasing = T) %>% as.data.frame %>% head %>% kable


df$wb_topics %>% unlist %>% table %>% paste("topic appears",.,"times") %>% table %>% sort(decreasing = T) %>% as.data.frame %>% head %>% kable

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


ggplot(entropy %>% filter(date<53, date > 1),aes(x = date, y= entropy,col = source))+
  # scale_y_log10()+
  geom_line()+
  theme_minimal()+
  # geom_point()+
  ggtitle("Topic Entropy by Source (Weekly)")+
  xlab("week")+
  ylab("Shannon Entropy")

# entropy (monthly) -----------------------------------------------------------------


entropy <- counts %>%
  group_by(date = month(date),source) %>%
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
  # geom_point()+
  ggtitle("Topic Entropy by Source (Monthly)")+
  xlab("month")+
  ylab("Shannon Entropy")+
  scale_x_continuous(breaks = 1:12,labels = month(1:12,T))




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
  # geom_point()+
  ggtitle("Topic Entropy by Source (Daily)")+
  xlab("date")+
  ylab("Shannon Entropy")

