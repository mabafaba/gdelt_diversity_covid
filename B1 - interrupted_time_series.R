library(stargazer)
library(broom)
entropy_stat<-entropy

entropy_stat <- entropy_stat %>% ungroup %>% 
  mutate(
    days_since_pandemic = date-WHO_pandemic_start_date,
    is_after_pandemic = days_since_pandemic>=0,
    count_after_pandemic = ifelse(days_since_pandemic>0,days_since_pandemic,0)
  ) 



# interrupted linear model
ilm<-function(x,y,x_treatment){
  df <- tibble(x,y) %>%  mutate(
    time_relative_to_treatment = x-x_treatment,
    is_after_treatment = time_relative_to_treatment>=0,
    time_after_treatment = ifelse(is_after_treatment,time_relative_to_treatment,0)
  ) 
 lm_result <- lm( y ~ 
                time_relative_to_treatment + 
                is_after_treatment + 
                time_after_treatment, 
              data = df)
 lm_data<-df
 lm_prediction<-predict(lm_result,lm_data)
 return(list(lm=lm_result,data=lm_data,prediction=lm_prediction))
}

entropy_all_predicted<-entropy_stat %>% filter(FALSE)
sink('./outputs/results.html',append = F)
sink()

for(i in 1:length(unique(entropy_stat$source))){
  thissource<- unique(entropy_stat$source)[i]
  this_entropy<-entropy_stat %>% filter(source == thissource)
  
  interrupted_time_series_reg <- ilm(this_entropy$date,
                 this_entropy$entropy,
                 x_treatment = WHO_pandemic_start_date) 
  
  
  sink('./outputs/results.html',append = T)
  print(paste("<br><br><br>",thissource,"<br><br><br>"))
  stargazer( interrupted_time_series_reg$lm , 
             type = "html", 
             dep.var.labels = (paste("Weekly Entropy", thissource)),
             column.labels = "",
             cavariate.labels = c("Time", "Treatment", "Time Since Treatment"),
             omit.stat = "all", 
             digits = 3 )
  sink()
  predicted <- predict(interrupted_time_series_reg$lm, interrupted_time_series_reg$data) 
  this_entropy$predicted<-predicted
  entropy_all_predicted<-bind_rows(entropy_all_predicted,this_entropy)
  
  
}
ggplot(entropy_all_predicted,aes(x=days_since_pandemic,col=source))+
  geom_line(aes(y=entropy),alpha=0.5,lwd=0.3)+geom_point(aes(y=entropy),size=0.4,alpha=0.5)+
  geom_line(aes(y=predicted),lwd=1)+
  annotate(geom = "vline",
           x = 0,
           xintercept = 0,
           linetype = c("dashed"))+scale_color_brewer(palette = "Dark2")+scale_x_continuous()+theme_minimal()
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
ggsave("./outputs/ITSA_entropy.pdf",width=unit(10,'cm'),height = unit(4,"cm"))
entropy_predicted<- entropy_stat
entropy_predicted$entropy<-pred1
ggplot(predicted,aes(x=days_since_pandemic,y=entropy))
ggplot(entropy_stat,aes(x=days_since_pandemic,y = entropy,col = source))+
  geom_point()+
  annotate(geom = "vline",
           x = 0,
           xintercept = 0,
           linetype = c("dashed"))+
  theme_minimal()+geom_line(data=entropy_predicted)
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(T = rep(1 : 365), D = rep(0), P = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( Y,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 365), 
      ylim = c(0, 400),
      xlab = "Time (days)", 
      ylab = "Wellbeing index")

lines( rep(1:199), pred1[1:199], col="dodgerblue4", lwd = 3 )
lines( rep(201:365), pred1[201:365], col="dodgerblue4", lwd = 3 )
lines( rep(200:365), pred2[200:365], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 45, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(300, 95, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")


