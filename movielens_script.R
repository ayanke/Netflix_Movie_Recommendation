library(tidyverse)
library(dplyr)
library(caret)
library(lubridate)
library(gridExtra)
library(OneR)
load("C:/Users/ayan/OneDrive - Equinor/R/EDX_Movies/Environment.RData")

str(edx)

edx <- edx %>% mutate(release=as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(release=as.numeric(str_sub(title,-5,-2)))

#Simplest approach: predict rating with the overall mean
overall <- mean(edx$rating)

#Compute the RMSE error
error <- data.frame("Method"="Average","RMSE"=RMSE(plyr::round_any(overall,0.5),validation$rating))

##### MOVIE EFFECT #####

#Examine movie effect. Some movies are rated higher than others because they are blockbusters or single hits.
edx %>% group_by(movieId) %>% summarize(N=n()) %>% ggplot(aes(N)) + 
  geom_histogram(bins=30, color="white") + scale_x_log10() + 
  labs(title="Histogram of # of movie reviews",x="# of Reviews",y="Frequency")

#Show a boxplot of how movies with few number of ratings tend to show larger variability in those ratings on average.
edx %>% group_by(movieId) %>% summarize(N=n(), avg=mean(rating)) %>% ggplot(aes(x=N,y=avg)) + 
  geom_boxplot(aes(group=cut_width(N,1000))) + 
  geom_hline(yintercept=overall,color="red") +
  labs(x="# of Ratings by movie",y="Average Rating",title="Average Rating by # of Ratings Across Movies")

##### USER EFFECT #####

#Examine user effect. Some users are more active in reviewing movies. 
edx %>% group_by(userId) %>% summarize(N=n())
quantiles_usr<- edx %>% group_by(userId) %>% summarize(N=n()) %>% pull(N) %>% quantile()

#histogram of number of reviews by users divided into quartiles.
edx %>% group_by(userId) %>% summarize(N=n()) %>% ggplot(aes(N)) + 
  geom_histogram(bins=30, color="white") + scale_x_log10() +  
  labs(title="Histogram of # of movie reviews by user",x="# of Reviews",y="Frequency") + 
  geom_vline(xintercept=quantiles_usr,color="red") + 
  geom_vline(xintercept=17,color="blue")
  
#Show a errorbar plot of how users who give few ratings show larger variability in those ratings on average. Also, user ratings decrease as users review more titles.

edx %>% group_by(userId) %>% summarize(N=n(),rounded=plyr::round_any(N,10),avg=mean(rating),se=sd(rating)/sqrt(rounded)) %>% group_by(rounded) %>%
  summarize(avg=mean(avg),se=mean(se)) %>%
  ggplot(aes(x=rounded,y=avg, ymin =avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  geom_hline(yintercept=overall,color="red") +
  xlim(0,1000) + 
  geom_vline(xintercept=375,color="blue") +
  labs(x="# of Ratings by user",y="Average Rating",title="Average Rating by # of Ratings Across Users")

##### GENRE EFFECT #####

#examine genre effect. Create tidy table where movie genres are ungrouped (i.e., seperate genre strings by "|")
edx_genres_ungroup<-edx %>% separate_rows(genres,sep="\\|") %>% filter(genres!="(no genres listed)") %>% mutate(date=round_date(as_datetime(timestamp),"week"))
validation_genres_ungroup<-validation %>% separate_rows(genres,sep="\\|") %>% filter(genres!="(no genres listed)") %>% mutate(date=round_date(as_datetime(timestamp),"week"))
  
edx_genres<- edx_genres_ungroup %>% group_by(genres) %>% summarize(N=n(),avg=mean(rating),se=sd(rating)/sqrt(N)) %>% arrange(desc(N))
edx_genres %>% knitr::kable()

#create an errorbar plot that shows the rating (relative to the global mean) of each movie genre for all users.
#This chart should show if there is a preference for certain genres of movies among all users. 
edx_genres %>% ggplot(aes(x=reorder(genres,(abs(avg-overall))),y =avg, ymin =avg - 2*se, ymax = avg + 2*se,color=N,label=round(avg-overall,3))) + 
  labs(title="Error Chart of Average Ratings by Genre",x="Genre",y="Average Rating") +
  geom_point() + 
  geom_errorbar() + 
  geom_label(nudge_y=0.07) +
  geom_hline(yintercept=overall,color="red")

#Boxplot showing how genres with fewer numbers of ratings show greater deviations and variability from the global average rating.
edx_genres %>% select(N) %>% OneR::bin(nbins=6) %>% set_names("Bins") %>% cbind(edx_genres) %>% 
  mutate(dev=abs(avg-overall)) %>% ggplot(aes(x=Bins,y=dev)) + geom_boxplot() + 
  labs(title="# of ratings by genre vs. deviation from global avg rating",x="# of ratings",y="Deviation from Mean Rating")

##### TIME EFFECT #####

#plot total number of ratings by review date
p1<-edx_genres_ungroup %>% group_by(date) %>% summarize(N=n()) %>% ggplot(aes(x=date,y=N)) + 
  geom_point() + 
  geom_smooth() +
  ylim(0,1.5e5) +
  labs(title="Rating Date vs. # of ratings",x="Rating Date",y="N")
p2<-edx_genres_ungroup %>% group_by(release) %>% summarize(N=n()) %>% ggplot(aes(x=release,y=N)) +
  geom_point() +
  geom_smooth() + 
  scale_y_log10() +
  labs(title="Movie Release Date vs. # of ratings",x="Release Date",y="N")

#plot average rating by year
p3<-edx_genres_ungroup %>% group_by(date) %>% summarize(avg=mean(rating)) %>% ggplot(aes(x=date,y=avg)) + 
  geom_point() + 
  geom_smooth() +
  geom_hline(yintercept=overall,color="red") +
  ylim(3,4.25) + 
  labs(title="Rating Date vs. Avg Rating",x="Rating Date",y="Avg")
p4<-edx_genres_ungroup %>% group_by(release) %>% summarize(avg=mean(rating)) %>% ggplot(aes(x=release,y=avg)) +
  geom_point() +
  geom_smooth() + 
  ylim(3,4.25) + 
  geom_hline(yintercept=overall,color="red") +
  labs(title="Movie Release Date vs. Avg Rating",x="Release Date",y="Avg")

grid.arrange(p2,p1,p4,p3,ncol=2,nrow=2) 

##### MODELS #####

#MOVIE MODEL: taking the average deviation from the global mean rating. 
movie_model<-edx %>% group_by(movieId) %>% summarize(b_i=mean(rating-overall)) 
predictions<-validation %>% left_join(movie_model,by="movieId") %>% mutate(pred=overall + b_i) %>% pull(pred) 

error<-rbind(error,data.frame("Method"="Movie","RMSE"=RMSE(predictions,validation$rating)))

#MOVIE MODEL REGULARIZED
lambdas <- seq(0, 10, 0.25) #trial regularization parameters
rmses <- sapply(lambdas, function(l){
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - overall)/(n()+l)) #computed movie effect that regularizes biases incurred by small sample populations 
  preds <-validation %>% left_join(b_i, by = "movieId") %>% mutate(pred = overall + b_i) %>% pull(pred)  #predictions
  return(RMSE(preds, validation$rating))
})
l<-lambdas[which.min(rmses)] #get lambda value that minimizes the error, calculated above
plot(lambdas, rmses) %>% abline(v=l) %>% text(x=l,y=rmses[lambdas==l],labels=paste0("Best Lambda: ",l),pos=3,offset=4) #plot rmse vs. lambdas to obtain optimal value

model<-edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - overall)/(n()+l)) #Compute final model based on regularization, above.
predictions<-validation %>% left_join(model,by="movieId") %>% mutate(pred=overall + b_i) %>% pull(pred)  #extract predicted values for comparing to the validation dataset

error<-rbind(error,data.frame("Method"="Movie, regularized","RMSE"=RMSE(predictions,validation$rating))) #error table of predictions vs. validation dataset

error %>% knitr::kable()

#MOVIE + USER MODEL REGULARIZED
lambdas <- seq(0, 10, 0.25) #trial regularization parameters
get_rmse <- function(lambdas) {
  sapply(lambdas, function(l){
    b_i <- edx %>% #movie effect calculated earlier
      group_by(movieId) %>%
      summarize(b_i = sum(rating - overall)/(n()+l))
    b_u <- edx %>% #user effect calculation using same methodology as movie effect
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - overall)/(n()+l))
    preds <-validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% 
      mutate(pred = overall + b_i + b_u) %>% pull(pred)
    return(RMSE(preds, validation$rating))
  })}
rmse <- get_rmse(lambdas)

l<-lambdas[which.min(rmse)] #get lambda value that minimizes the error, calculated above
plot(lambdas, rmse) %>% abline(v=l) %>% text(x=l,y=rmse[lambdas==l],labels=paste0("Best Lambda: ",l),pos=3,offset=4) #plot rmse vs. lambdas to obtain optimal value

model <- get_rmse(l)
error<-rbind(error,data.frame("Method"="Movie + User, regularized","RMSE"=rmse))

error %>% knitr::kable()

#MOVIE + USER + GENRE MODEL REGULARIZED 
lambdas <- seq(13, 16, 0.25) #trial regularization parameters
get_rmse <- function(lambdas) {
  sapply(lambdas, function(l){
    b_i <- edx_genres_ungroup %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - overall)/(n()+l))
    b_u <- edx_genres_ungroup %>%
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - overall)/(n()+l))
    b_g <- edx_genres_ungroup %>% #genre effect calculation using same methodology as the others
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%
      group_by(genres) %>%
      summarize(b_g = sum(rating - b_i - b_u - overall)/(n()+l))
    
    preds <-validation_genres_ungroup %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% left_join(b_g, by = "genres") %>% 
      mutate(pred = overall + b_i + b_u + b_g) %>% pull(pred)
    return(RMSE(preds, validation_genres_ungroup$rating))
  })}
rmse <- get_rmse(lambdas)

l<-lambdas[which.min(rmse)] #get lambda value that minimizes the error, calculated above
plot(lambdas, rmse) %>% abline(v=l) %>% text(x=l,y=rmse[lambdas==l],labels=paste0("Best Lambda: ",l),pos=3,offset=4) #plot rmse vs. lambdas to obtain optimal value

rmse <- get_rmse(l)
error<-rbind(error,data.frame("Method"="Movie + User + Genre, regularized","RMSE"=rmse))

error %>% knitr::kable()

#MOVIE + USER + GENRE REGULARIZED + RELEASE DATE
lambdas <- seq(13, 15, .25) #trial regularization parameters
get_rmse <- function(lambdas) {
  sapply(lambdas, function(l){
    b_i <- edx_genres_ungroup %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - overall)/(n()+l))
    b_u <- edx_genres_ungroup %>%
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - overall)/(n()+l))
    b_g <- edx_genres_ungroup %>% #genre effect calculation using same methodology as the others
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%
      group_by(genres) %>%
      summarize(b_g = sum(rating - b_i - b_u - overall)/(n()+l))
    b_t <- edx_genres_ungroup %>%  #non-regularized time effect calculation. No regularization needed because exploratory data analysis showed anomalous levels of variability among movie release years.
      left_join(b_i, by="movieId") %>%
      left_join(b_u, by="userId") %>%
      left_join(b_g, by="genres") %>%
      group_by(release) %>%
      summarize(b_t = mean(rating - b_i - b_u - b_g - overall))
    
    preds <-validation_genres_ungroup %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% 
      left_join(b_g, by = "genres") %>% left_join(b_t, by = "release") %>% 
      mutate(pred = overall + b_i + b_u + b_g + b_t) %>% pull(pred)
    
    return(RMSE(preds, validation_genres_ungroup$rating))
  })}
rmse <- get_rmse(lambdas)

l<-lambdas[which.min(rmse)] #get lambda value that minimizes the error, calculated above
plot(lambdas, rmse) %>% abline(v=l) %>% text(x=l,y=rmse[lambdas==l],labels=paste0("Best Lambda: ",l),pos=3,offset=4) #plot rmse vs. lambdas to obtain optimal value

rmse <- get_rmse(l)
error<-rbind(error,data.frame("Method"="Movie + User + Genre, regularized, + Release Year","RMSE"=rmse))

##### FINAL ERROR ####

error %>% knitr::kable()



