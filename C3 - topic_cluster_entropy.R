
# TOPIC ENTROPY BY MAJOR TOPIC --------------------------------------------

# must run C1...R and C2.....R first!


entropy <- counts %>% ungroup %>% 
  # remove likely incomplete weeks 
  filter(week(date)<52,
         week(date)>1,
         ! (year(date)==2021 & week(date)== max(week(date)[year(date)==2021]))
  ) %>% 
  # split by year, week and news source
  group_by(year = year(date),week = week(date),major_topic_label_own) %>%
  # calculate entropy for each
  summarise(date = min(date),
            entropy = shannon_entropy(number_of_occurances), # entropy
            relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
            num_unique_topics = n(),
            num_articles = sum(number_of_occurances),
            occuring_topics = paste(values,collapse = "; ") # list topics that appeared
  ) # number of unique topics found

ggplot(entropy,aes(x = date, y = entropy,col = major_topic_label_own))+
  # scale_y_log10()+
  geom_line()+
  theme_minimal()+
  # geom_point()+
  ggtitle("Topic Entropy by Major Themes (Weekly)")+
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
ggsave("./outputs/main_topic_entropy.pdf",width = unit(6,'cm'),height= unit(3,'cm'))




# ILM ---------------------------------------------------------------------

library(stargazer)
library(broom)

entropy_stat<-entropy

entropy_stat <- entropy_stat %>% ungroup %>% 
  mutate(
    days_since_pandemic = date-WHO_pandemic_start_date,
    is_after_pandemic = days_since_pandemic>=0,
    count_after_pandemic = ifelse(days_since_pandemic>0,days_since_pandemic,0)
  ) 




entropy_all_predicted<-entropy_stat %>% filter(FALSE)
sink('./outputs/results.html',append = F)
sink()

for(i in 1:length(unique(entropy_stat$major_topic_label_own))){
  thistopic<- unique(entropy_stat$major_topic_label_own)[i]
  this_entropy<-entropy_stat %>% filter(major_topic_label_own == thistopic)
  
  interrupted_time_series_reg <- ilm(this_entropy$date,
                                     this_entropy$entropy,
                                     x_treatment = WHO_pandemic_start_date) 
  
  
  sink('./outputs/results.html',append = T)
  print(paste("<br><br><br>",thistopic,"<br><br><br>"))
  stargazer( interrupted_time_series_reg$lm , 
             type = "html", 
             dep.var.labels = (paste("Weekly Entropy", thistopic)),
             column.labels = "",
             cavariate.labels = c("Time", "Treatment", "Time Since Treatment"),
             omit.stat = "all", 
             digits = 3 )
  sink()
  predicted <- predict(interrupted_time_series_reg$lm, interrupted_time_series_reg$data) 
  this_entropy$predicted<-predicted
  entropy_all_predicted<-bind_rows(entropy_all_predicted,this_entropy)
  
  
}
ggplot(entropy_all_predicted,aes(x=days_since_pandemic))+
  geom_line(aes(y=entropy),alpha=0.5,lwd=0.3)+
  facet_grid(rows = vars(major_topic_label_own))+
  geom_point(aes(y=entropy),size=0.4,alpha=0.5)+
  geom_line(aes(y=predicted),lwd=1)+
  annotate(geom = "vline",
           x = 0,
           xintercept = 0,
           linetype = c("dashed"))+scale_color_brewer(palette = "Dark2")+scale_x_continuous()+theme_minimal()
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
ggsave("./outputs/ITSA_maintopic_entropy.pdf",width=unit(10,'cm'),height = unit(8,"cm"))


counts_major_topic_weekly<- counts %>% group_by(year(date),week(date),major_topic_label_own) %>% summarise(date=min(date),n = sum(number_of_occurances))

counts_major_topic_weekly <- counts_major_topic_weekly %>% group_by(year(date),week(date)) %>% mutate(percentage = n / sum(n))
counts_major_topic_weekly$percentage %>% min

ggplot(counts_major_topic_weekly,aes(x=date,y=percentage, col=major_topic_label_own))+
  geom_line()+theme_minimal()+scale_fill_brewer(palette = "RdBu")+
  annotate(geom="vline",xintercept = WHO_pandemic_start_date, lwd = 1, linetype = 'dashed')
ggsave("./outputs/major_topic_percentages.pdf")


