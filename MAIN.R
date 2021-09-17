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


# READING FROM MANY FILES IN ORIGINAL GDELT FORMAT ------------------------------------------------------

# files <- list.files("/Volumes/2/content_diversity_data_2017_2021/",pattern = "\\.csv$",recursive = T,full.names = T)

# for(i in 1:length(files)){
#   cat(round(100*i/length(files)))
#     files[i] %>%
#       read_gdelt %>%
#       topic_counts_by_date_and_source %>%
#       write_rds(paste0("temp/data/counts/count",i,".RDS")) %>%
#       entropy_from_counts %>%
#       write_rds(paste0("temp/data/entropy/ent",i,".RDS"))
# }

# loading them from there  
# can still be a bit heavy..
# counts <- list.files("temp/data/counts/",recursive = T,full.names = T) %>% lapply(read_rds) 
# counts <- data.table::rbindlist(counts) %>% as_tibble 


# WORK FROM SINGLE FILE -------------------------------------------------------------

# counts <- read_gdelt_prepared(filename = "./data/ALL_s.csv") %>% # function specific to the exact data format as received from Morry
#   topic_counts_by_date_and_source %>% # make one row for each topic, date and source combination and count occurances 
#   write_rds(paste0("temp/data/ALL_COUNTS_",i,".RDS")) # safe for later

counts <- readRDS('./temp/data/ALL_COUNTS_1.RDS')


# counts$week<-first_of_week(counts$date)
# totals <- counts %>% group_by(year(date),week(date),
#                               source) %>%
#                       summarise(
#                                 date = first_of_week(date),
#                                 number_of_topic_appearances = sum(number_of_occurances))
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

entropy <- counts %>% ungroup %>% 
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
  geom_vline(xintercept=ymd("2020-11-9"),col="black",lty=3)+
  annotate(geom = "vline",
                x = ymd("2020-11-9"),
                # x = c("9/11 attack anniversary"),
                xintercept = ymd("2020-11-9"),
                linetype = c("dashed"))+
  annotate(geom = "vline",
           x = ymd("2020-03-11"),
           # x = c("9/11 attack anniversary"),
           xintercept = ymd("2020-03-11"),
           linetype = c("dashed"))
  ggsave("./outputs/source_entropy.pdf",width = unit(6,'cm'),height= unit(3,'cm'))





# average topic entropy pre- and post- covid outbreak comparison ----------


covid_cases<-GET_covid_case_data(cummulative = T)
US_pandemic_start_date <- covid_cases %>% filter(cases >=1000) %>% .$date %>% min
WHO_pandemic_start_date<- lubridate::ymd("2020-03-11")

pandemic_start<-WHO_pandemic_start_date
measurement_pre_outbreak <- entropy$entropy[entropy$date < pandemic_start]
measurement_post_outbreak <- entropy$entropy[entropy$date >= pandemic_start]
sink("./outputs/results.txt",append = F)
t.test(measurement_post_outbreak,measurement_pre_outbreak,alternative = "less")
sink()


# linear recovery of content diversity? -----------------------------------

post_outbreak_lm <- lm(entropy ~ date, data = entropy[entropy$date>=pandemic_start+90,])
sink("./outputs/results.txt",append = T)
cat("\n\n\n\n RECOVERY LM\n\n")
print(summary(post_outbreak_lm))
sink()


# average number of sources per topic ----------------------------

# count how many different sources cover a topic in any given week 
num_sources<-counts %>% ungroup %>% 
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
sources_per_topic_pre <- num_sources$count[num_sources$date < pandemic_start]
sources_per_topic_post <- num_sources$count[num_sources$date >= pandemic_start]
t.test(sources_per_topic_post,sources_per_topic_pre,alternative = "less")
sink()


# comparison source entropy -----------------------------------------------

source_entropy_phase<-counts %>% mutate(phase = ifelse(date<pandemic_start,"before outbreak","after outbreak")) %>% 
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




# Which topics changed the most and how? ---------------------------------------------------

counts_pre_post<-compare_topic_prominence(counts,cutoff_date = WHO_pandemic_start_date)



# most reduced topics
top_increased_topics <- counts_pre_post %>% arrange(desc(delta)) %>% head(3) %>% dplyr::select(topic,delta) 
top_reduced_topics<-counts_pre_post %>% arrange(delta) %>% head(3) %>% dplyr::select(topic,delta) 

plot_and_save_topic_timeline<-function(topic,counts,prefix = ""){
  ggsave(filename = paste0("./outputs/",prefix,"topic_timeline_",topic,".pdf"),
         plot = topic_timeline(topic,counts),
         device = "pdf",
         width=unit(12,'cm'),
         height=unit(4,'cm'))
}

dir.create("./outputs/most_increased")
dir.create("./outputs/most_decreased")

lapply(top_increased_topics$topic,plot_and_save_topic_timeline,counts=counts,"/most_increased/")
lapply(top_reduced_topics$topic,plot_and_save_topic_timeline,counts=counts,"/most_decreased/")





