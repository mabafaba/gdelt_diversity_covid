

#' read gdlet csv and get it into nicer format
#' @param filename full path to file
#' @param small if TRUE, keep main columns only
#' @param filter_sources if TRUE keeps only sources that match exactly 'bbc.com', 'ccn.com', 'foxnews.com', 'theguardian.com'
#' @return a tibble data frame 
read_gdelt<-function(filename, small = T,filter_sources = T){
  # cat(filename);cat('\n')
  df <- suppressMessages(read_csv(filename,col_names = FALSE))
  # Topic labels are separated with ";". Convert that column into a "vector column", where each entry contains a vector of topics (splitting the strings on ";")
  df$X9<-strsplit(df$X9,";")
  # get dates into good format
  # parse dates
  df$date<- paste0(substr(df$X2,1,4),# year: first four characters
                   "-",
                   substr(df$X2,5,6), #month 
                   "-",
                   substr(df$X2,7,8)) %>% # day 
    (lubridate::ymd) # convert to datetime data format (ymd = year-month-day)
  colnames(df)[which(colnames(df)=="X5")]<-"source"
  
  # remove odd sources
  if(filter_sources){
  df <- df %>% filter(source %in% c("bbc.com","foxnews.com","cnn.com","theguardian.com"))
  }
  df$wb_topics <- df$X9 %>% lapply(function(x){grep("^WB_",x,value=T)}) # extract world bank standard topic codes only
  unique_wb_topcs<-unique(unlist(df$wb_topics))
  df$not_wb_topics <- df$X9 %>% lapply(function(x){x[!(x %in% unique_wb_topcs)]}) # sorry for this weird line. Just picks out the topics that are not in wb_topics.
  if(small){
    return(df[,c("X9","date","source", "wb_topics","not_wb_topics")])
  }
  df
}





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


#'
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


