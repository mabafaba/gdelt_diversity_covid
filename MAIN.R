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

# read all files
# files <- list.files("./data/2020/",recursive = T,full.names = T)
files <- list.files("/Volumes/2/content_diversity_data_2017_2021/",pattern = "\\.csv$",recursive = T,full.names = T)



# THIS IS THE SLOW PART ------------------------------------------------------

# for(i in 1:length(files)){
#   cat(round(100*i/length(files)))
#     files[i] %>%
#       read_gdelt %>%
#       topic_counts_by_date_and_source %>%
#       write_rds(paste0("temp/data/counts/count",i,".RDS")) %>%
#       entropy_from_counts %>%
#       write_rds(paste0("temp/data/entropy/ent",i,".RDS"))
# }

# RESULTS ARE STORED IN TEMP FOLDER ---------------------------------------
# loading them from there  
# can still be a bit heavy..
counts <- list.files("temp/data/counts/",recursive = T,full.names = T) %>% lapply(read_rds) 
counts <- data.table::rbindlist(counts) %>% as_tibble 

entropy <- list.files("temp/data/entropy/",recursive = T,full.names = T) %>% lapply(read_rds)
entropy <- data.table::rbindlist(entropy) %>% as_tibble
entropy
# totals <- counts %>% group_by(date = week(date),source) %>% summarise(number_of_topic_appearances = sum(number_of_occurances))
# 
# ggplot(totals,
#        aes(x=date,
#            y=number_of_topic_appearances,
#            col=source))+
#   geom_line()+
#   theme_minimal()+
#   # geom_point()+
#   ggtitle("Total Number of topics noted across articles per Source (Weekly)")+
#   xlab("week")+
#   ylab("# of topics")


# TOPIC entropy (by news source, weekly) -----------------------------------------------------------------

entropy <- counts %>%
  # remove likely incomplete weeks 
  filter(week(date)<52,
         week(date)>1,
         ! (year(date)==2021 & week(date)== max(week(date)[year(date)==2021]))
  ) %>% 
  # split by year, week and news source
  group_by(year = year(date),week = week(date),source) %>%
  # calculate entropy for each
    summarise(date = min(date),
            entropy = shannon_entropy(number_of_occurances), # entropy
            relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
            num_unique_topics = n(),
            num_articles = sum(number_of_occurances),
            occuring_topics = paste(values,collapse = "; ") # list topics that appeared
  ) # number of unique topics found

ggplot(entropy,aes(x = date, y = entropy,col = source))+
  # scale_y_log10()+
  geom_line()+
  theme_minimal()+
  # geom_point()+
  ggtitle("Topic Entropy by Source (Weekly)")+
  xlab("week")+
  ylab("Shannon Entropy")+
  # geom_vline(xintercept=ymd("2021-11-2"),col="red")+
  # geom_vline(xintercept=ymd("2020-11-9"),col="black",lty=3)+
  annotate(geom = "vline",
                x = ymd("2020-11-9"),
                # x = c("9/11 attack anniversary"),
                xintercept = ymd("2020-11-9"),
                linetype = c("dashed"))
ggsave("./outputs/source_entropy.pdf",width = unit(6,'cm'),height= unit(3,'cm'))



# average topic entropy pre- and post- covid outbreak comparison ----------


covid_cases<-GET_covid_case_data(cummulative = T)
US_pandemic_start_date <- covid_cases %>% filter(cases >=1000) %>% .$date %>% min
measurement_pre_outbreak <- entropy$entropy[entropy$date < US_pandemic_start_date]
measurement_post_outbreak <- entropy$entropy[entropy$date >= US_pandemic_start_date]
sink("./outputs/results.txt",append = F)
t.test(measurement_post_outbreak,measurement_pre_outbreak,alternative = "less")
sink()


# linear recovery of content diversity? -----------------------------------

post_outbreak_lm <- lm(entropy ~ date, data = entropy[entropy$date>=US_pandemic_start_date+90,])
sink("./outputs/results.txt",append = T)
cat("\n\n\n\n RECOVERY LM\n\n")
print(summary(post_outbreak_lm))
sink()


# average number of sources per topic ----------------------------

# count how many different sources cover a topic in any given week 
num_sources<-counts %>%
  # remove likely incomplete weeks 
  filter(week(date)<52,
         week(date)>1,
         ! (year(date)==2021 & week(date)== max(week(date)[year(date)==2021]))
  ) %>% 
  group_by(year(date),week(date), values) %>%
  summarise(date = min(date),num_sources = length(unique(source)))

colnames(num_sources)<-c("year","week","values","date","count")

# mean over all topics
mean_num_sources<-num_sources %>%
  group_by(year(date),week(date)) %>%
  summarise(date = min(date),
            mean_num_sources = mean(count)
  )

ggplot(mean_num_sources)+
  geom_line(aes(x=date,y=mean_num_sources))+
  theme_minimal()+
  ylab("average number of sources per topic")

ggsave("./outputs/overall_source_entropy_weekly.pdf",width = unit(8,'cm'),height= unit(4,'cm'))


# comparison # of sources per topic pre and post outbreak -----------------

sink("./outputs/results.txt")
cat("\n\n Average sources per topic decreased?")
sources_per_topic_pre <- num_sources$count[num_sources$date < US_pandemic_start_date]
sources_per_topic_post <- num_sources$count[num_sources$date >= US_pandemic_start_date]
t.test(sources_per_topic_post,sources_per_topic_pre,alternative = "less")
sink()

source_entropy_phase<-counts %>% mutate(phase = ifelse(date<US_pandemic_start_date,"before outbreak","after outbreak")) %>% 
  group_by(phase,values, source) %>%
  summarise(number_of_occurances = sum(number_of_occurances)) %>% 
  group_by(phase,values) %>% 
  summarise(source_entropy = shannon_entropy(number_of_occurances))

sink("./outputs/results.txt")
cat("\n\n SOURCE ENTROPY PRE and POST OUTBREAK")
source_entropy_phase %>% group_by(phase) %>% summarise(mean(source_entropy))
sink()

ggplot(source_entropy_phase)+geom_histogram(aes(x=source_entropy,fill=phase),bins = 8, position="dodge")+theme_minimal()+xlab('Source Information Entropy')+ylab("# of weekly topics")
ggsave("./outputs/entropy_density.pdf",width = unit(4,'cm'),height = unit(2,'cm'))



# source entropy ----------------------------------------------------------


entropy_sources <- 
  counts %>%
  # remove likely incomplete weeks 
  filter(week(date)<52,
         week(date)>1,
         ! (year(date)==2021 & week(date)== max(week(date)[year(date)==2021]))
  ) %>% 
  # split by year, week and TOPIC
  group_by(year = year(date),
           # week = week(date),
           values) %>% 
  # calculate entropy of sources for each topic for each week
  #
  summarise(date = min(date),
            entropy = shannon_entropy(number_of_occurances), # entropy
            relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
            num_unique_topics = n(),
            num_articles = sum(number_of_occurances),
            occuring_sources = paste(source,collapse = "; ")
            )

weighted_mean_entropy_sources <- entropy_sources %>%
  group_by(date) %>%
  mutate(weighted_entropy = entropy * num_articles) %>% 
  summarise(entropy=mean(entropy)/sum(num_articles))

ggplot(mean_entropy_sources,aes(x = date, y = entropy))+
  # scale_y_log10()+
  geom_line()+
  theme_minimal()+
  # geom_point()+
  ggtitle("Mean Source Entropy (Weekly)")+
  xlab("week")+
  ylab("Mean Shannon Entropy")+
  # geom_vline(xintercept=ymd("2021-11-2"),col="red")+
  # geom_vline(xintercept=ymd("2020-11-9"),col="black",lty=3)+
  annotate(geom = "vline",
           x = ymd("2020-11-9"),
           # x = c("9/11 attack anniversary"),
           xintercept = ymd("2020-11-9"),
           linetype = c("dashed"))





# # entropy (monthly) -----------------------------------------------------------------
# 
# 
# entropy <- counts %>%
#   group_by(year = year(date),month=month(date),source) %>%
#   summarise(date = min(date),
#             entropy = shannon_entropy(number_of_occurances), # entropy
#             relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
#             num_unique_topics = n(),
#             num_articles = sum(number_of_occurances),
#             occuring_topics = paste(values,collapse = "; ") # list topics that appeared
#   ) # number of unique topics found
# 
# 
# ggplot(entropy,aes(x = date, y= entropy,col = source))+
#   # scale_y_log10()+
#   geom_line()+
#   theme_minimal()+
#   # geom_point()+
#   ggtitle("Topic Entropy by Source (Monthly)")+
#   xlab("date")+
#   ylab("Shannon Entropy")+
#   geom_line(aes(x=date,y=cases),data=cases)
# 
# ggplot(cases)+geom_line(aes(x=date,y=deaths))+theme_minimal()
# 
# 
# # entropy (daily) -----------------------------------------------------------------
# 
# 
# entropy <- counts %>%
#   group_by(date,source) %>%
#   summarise(entropy = shannon_entropy(number_of_occurances), # entropy
#             relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
#             num_unique_topics = n(),
#             num_articles = sum(number_of_occurances),
#             occuring_topics = paste(values,collapse = "; ") # list topics that appeared
#   ) # number of unique topics found
# 
# 
# ggplot(entropy,aes(x = date, y= entropy,col = source))+
#   # scale_y_log10()+
#   geom_line()+
#   theme_minimal()+
#   
#   # geom_point()+
#   ggtitle("Topic Entropy by Source (Daily)")+
#   xlab("date")+
#   ylab("Shannon Entropy")
# 
# 

