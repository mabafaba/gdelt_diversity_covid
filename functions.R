library(purrr)


# DATA MANAGAMENT ---------------------------------------------------------



#' read gdlet csv if reduced to the needed columns
#' @param filename full path to file
#' @param small if TRUE, keep main columns only
#' @param filter_sources if TRUE keeps only sources that match exactly 'bbc.com', 'ccn.com', 'foxnews.com', 'theguardian.com'
#' @return a tibble data frame 
read_gdelt_prepared<-function(filename, small = T,filter_sources = T){
  # cat(filename);cat('\n')
  cat(cyan("reading data..\n"))
  df <- suppressMessages(read_csv(filename,col_names = FALSE,
                                  col_types = list(col_character(),
                                                   col_character(),
                                                   col_character(),
                                                   col_character())))
  df<-df[-1,-1] #remove first row and first columns
  colnames(df)<-c("date","source","topics")
  # Topic labels are separated with ";". Convert that column into a "vector column", where each entry contains a vector of topics (splitting the strings on ";")
  cat(cyan("parsing topics..\n"))
  df$topics<-strsplit(df$topics,";")
  # get dates into good format
  cat(cyan("parsing dates..\n"))
  df$date<- paste0(substr(df$date,1,4),# year: first four characters
                   "-",
                   substr(df$date,5,6), #month 
                   "-",
                   substr(df$date,7,8)) %>% # day 
    (lubridate::ymd) # convert to datetime data format (ymd = year-month-day)
  
  # remove odd sources
  if(filter_sources){
    df <- df %>% filter(source %in% c("bbc.com","foxnews.com","cnn.com","theguardian.com"))
  }
  cat(cyan("separate worldbank topics..\n"))
  df$wb_topics <- df$topics %>% lapply(function(x){grep("^WB_",x,value=T)}) # extract world bank standard topic codes only
  unique_wb_topcs<-unique(unlist(df$wb_topics))

  df$not_wb_topics <- df$topics %>% 
     lapply(function(x){x[!(x %in% unique_wb_topcs)]}) # picks out the topics that are not in wb_topics.
  
  df<-ungroup(df) 
  
  cat(green("data is ready yay!\n"))

    if(small){
    return(df[,c("date","source", "not_wb_topics")])
  }
  return(df)
}



GET_covid_case_data<-function(cummulative = F){
  cases<-httr::GET("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") %>%
    httr::content(type = "text/csv")
  cases %<>% arrange(date)
  if(!cummulative){return(cases)}
  cases_diff<-cases[-1,]
  cases_diff$cases<-cases$cases[-1] - cases$cases[-nrow(cases)]
  cases_diff$deaths<-cases$deaths[-1] - cases$deaths[-nrow(cases)]
  if(cummulative){return(cases_diff)}
  stop("problem in getting covid case data")
}

wbtopic_gdelt_to_label<-function(x){
  x %>% gsub("^WB_[0-9]*_","",.)%>% tolower %>% gsub("_"," ",.)  
}


first_of_week<-function(date){
  paste(year(date), week(date),"1") %>% as.POSIXlt(format = "%Y %U %u")
}




# CALCULATIONS ------------------------------------------------------------




#' count how often each unique item occurs in a list
#' @param x a list of vectors
#' @return returns a data frame with two columns: one for the values, one for the counts
count_occurances_across_list<-function(x){
  counts <- x %>%
    unlist %>% # turn list of vectors into one big vector
    table %>% # count occurances
    as.data.frame # convert into long format (one row per unique item)
  
  if(ncol(counts)!=2){ # catch empty input
    warning("no occurances to count")
    return(data.frame(values = character(0),number_of_occurances=integer(0)))
  }
  colnames(counts)<-c("values","number_of_occurances")
  counts$values<-as.character(counts$values) # change the default "factor" data type to character data type
  counts
}


relative_freqency<-function(counts){counts/sum(counts)}

shannon_entropy<-function(counts){
  p <-   relative_freqency(counts)
  -sum(p * log2(p))
}


test_that("shannon entropy correct",{
  expect_error(shannon_entropy("text"))
  expect_error(shannon_entropy(factor("text")))
  expect_true(is.na(shannon_entropy(c(1,NA))))
  expect_true(shannon_entropy(NULL)==0)
  expect_true(shannon_entropy(1) ==0)
  expect_true(shannon_entropy(c(1,1)) ==   - 2*(0.5*log2(0.5)))
  expect_true(shannon_entropy(c(1,1,1,1)) ==  - 4*(0.25*log2(0.25)))
  
})



topic_counts_by_date_and_source<-function(df){
  counts <- df  %>%
    group_by(date,source) %>% # for each day...
    summarise(topic_count = count_occurances_across_list(not_wb_topics)) # count each topic's frequency
  
  # unnest data frame
  counts$values <- counts$topic_count$values
  counts$number_of_occurances <- counts$topic_count$number_of_occurances
  counts$topic_count<-NULL
  counts
}




entropy_from_counts<-function(counts){
  
  counts %>% group_by(year = year(date), week = week(date),source) %>%
    summarise(date = min(date),
              entropy = shannon_entropy(number_of_occurances), # entropy
              relative_entropy  = shannon_entropy(number_of_occurances)/sum(number_of_occurances),
              num_unique_topics = n(),
              num_articles = sum(number_of_occurances),
              occuring_topics = paste(values,collapse = "; "))
}



# count how often a topic appeared before vs. after pandemic
compare_topic_prominence<-function(counts, cutoff_date = ymd("2020-03-11")){
  timespan_before <- (cutoff_date - min(counts$date)) %>% as.numeric %>% divide_by(7)
  timespan_after <- (max(counts$date) - (cutoff_date+1))  %>% as.numeric %>% divide_by(7) 
  
  counts_pre_post<-counts %>% group_by(date < cutoff_date,values) %>% summarise(count=sum(number_of_occurances)) 
  counts_before<-counts_pre_post[counts_pre_post$`date < cutoff_date`,]
  counts_after<-counts_pre_post[!(counts_pre_post$`date < cutoff_date`),]
  counts_pre_post<-full_join(counts_before,counts_after,by = "values")
  colnames(counts_pre_post)<-c("X","topic","count_before","XX","count_after")
  
  # per week average
  # counts_before$count<-counts_before$count/timespan_before
  # counts_after$count<-counts_after$count/timespan_after
  
  counts_pre_post<-counts_pre_post %>% 
    dplyr::select(topic,count_before,count_after) %>%
    arrange(count_before) %>% 
    mutate(count_after = ifelse(is.na(count_after),0,count_after)) %>% # NAs to zeros (NAs happen in join() if zero occurances on one side)
    mutate(count_before = ifelse(is.na(count_before),0,count_before)) %>% 
    mutate(ratio = count_after/count_before, total = count_after+count_before, delta = count_after-count_before) 
}


# MODEL -------------------------------------------------------------------

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


# PLOTTING ----------------------------------------------------------------


# plot count over time for a given topic
topic_timeline<-function(topic,counts,as_percentage = TRUE){
  
  
  counts_topic<-counts %>% filter(grepl(topic,values,ignore.case = T)) %>% group_by(year(date),week(date),source) %>% summarise(date = min(date),number_of_occurances=sum(number_of_occurances)) 
  
  
  if(as_percentage){
    total_weekly <- counts %>% ungroup %>%
      group_by(source,year(date),week(date)) %>%
      summarise(date=min(date),total_articles = sum(number_of_occurances))
    
    counts_topic<-left_join(counts_topic,total_weekly,by=c("date","source"))
    counts_topic<-counts_topic %>% mutate(number_of_occurances = number_of_occurances/total_articles)
  }
  
  
  ggplot(counts_topic,aes(x=date,y=number_of_occurances,col=source))+
    geom_line(position = 'stack')+
    ggtitle(paste0("number of articles including `",wbtopic_gdelt_to_label(topic),"` as a topic"))+
    ylab(paste(ifelse(as_percentage,"percentage","number"),"of topic labels"))+
    theme_minimal()  
}



# NETWORKS ------------------------------------------------------------------

plot_graph_quickly<-purrr::partial(plot.igraph,
                                   vertex.size = 1,
                                   vertex.color='#00000055',
                                   vertex.label.cex=0.7,
                                   vertex.label.color = "#00000055",
                                   vertex.col=0.1,edge.color = "#00000055")


