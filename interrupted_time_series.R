library(stargazer)

entropy_stat<-entropy

entropy_stat <- entropy_stat %>% ungroup %>% 
  mutate(
    days_since_pandemic = date-WHO_pandemic_start_date,
    is_after_pandemic = days_since_pandemic>=0,
    count_after_pandemic = ifelse(days_since_pandemic>0,days_since_pandemic,0)
  ) 


for(i in 1:length(unique(entropy_stat$source))){
 thissource<- unique(entropy_stat$source)[i]
 this_entropy<-entropy %>% filter(source == thissource)
interrupted_time_series_reg <- lm( entropy ~ 
                                     days_since_pandemic + 
                                     is_after_pandemic + 
                                     count_after_pandemic, 
                                   data = entropy_stat)
sink('./outputs/results.html',append = T)
cat(paste("\n\n\n\n\n\n<br><br><br>",thissource,"\n\n"))
stargazer( interrupted_time_series_reg , 
           type = "html", 
           dep.var.labels = (paste("Weekly Entropy", thissource)),
           column.labels = "",
           covariate.labels = c("Time", "Treatment", "Time Since Treatment"),
           omit.stat = "all", 
           digits = 3 )
sink()

}


plot(interrupted_time_series_reg)

ggplot(entropy_stat,aes(x=days_since_pandemic,y = entropy,col = source))+geom_point()+theme_minimal()
pred1 <- predict(regTS, dataTS) 
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

# Line marking the interruption
abline( v=200, col="darkorange2", lty=2 )

Time series and its counterfactual



