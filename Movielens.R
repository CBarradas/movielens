###########################################################
##########     Movielens Capstone      ###################
##########################################################

##########################################################
# Creation of the Dataset
##########################################################
# Note: this process could take a couple of minutes

#Loading the necessary libraries
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(knitr)
library(RColorBrewer)
library(rmarkdown)
library(dslabs)
library(pdftools)
library(kableExtra)

##########   CODE PROVIDED   ##############
# Downloading the data and creating the edx set and validation set (final hold-out test set)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########   MY CODE   ##############

##########################################################
# Data Exploration
##########################################################
#Analyzing data basics: size, variables, missing data, main information
dim(edx) 
#There are 9,000,055 observations and 6 variables
summary(edx) 
#The six variables are userId, movieID,rating, timestamp, title and genres
str(edx) 
#Data types are integers, numeric and character
glimpse <- head(edx)
kable(glimpse) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
mean_rating <- mean(edx$rating)
any(is.na(edx))
#Same for the validation set
dim(validation)
#999,999 observations
any(is.na(validation))

#Analyzing each variable

#####   movieId   #####
#1. How many movies?
edx %>% summarize(n_movies= n_distinct(movieId)) 
#There are 10,6777 unique movies

#2. How many ratings?
#Generate plot
edx %>% count(movieId) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill = "grey") + 
  scale_x_log10() + 
  ggtitle("Number of Ratings per Movie") + 
  xlab("Number of Ratings") + 
  ylab("Number of Movies")
#Generate tables
most_ratings <- edx %>% count(movieId) %>% top_n(5)
kable(most_ratings) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
least_ratings <- edx %>% count(movieId) %>% top_n(-5)
least_ratings
#There are 126 movies with only one rating and 143 with more than 10,000 ratings. 
#The highest number of ratings are concentrated around the 100s.
most_rated <- edx %>% count(movieId, title) %>% top_n(3)
kable(most_rated) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
least_rated <- edx %>% count(movieId, title) %>% top_n(-3)
least_rated
#There are three movies with more than 30,000 ratings: Pulp Fiction, The Shawshank Redemption and The Silence of the Lambs. Very popular movies.
#Among the movies with only one rating are The Quarry, Hexed and Impulse, quite unknown movies.
#Clearly there is a bias of more ratings on more popular movies, therefore we need to adjust for this in the modeling. 

#3.Distribution of ratings
movies_rating<-edx %>% 
  select(rating) %>% 
  group_by(rating) %>% 
  summarise(count= n()) %>% 
  arrange(-count)
#Generate table
kable(movies_rating) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
#Generate plot
edx %>% ggplot(aes(rating)) + 
  geom_histogram(bins = 10, color = "black", fill = "grey") + 
  ggtitle("Distribution of Ratings") + 
  scale_x_continuous(breaks= c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5), labels= c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)) + 
  scale_y_continuous(breaks= c(1000000, 2000000), labels= c("1", "2")) + 
  xlab("Ratings") + 
  ylab("Number of Movies (in Millions)")
#We can see that the most common rating is 4, followed by 3 and 5. Half points are less common. 

#####   userId   #####
#1. How many users?
edx %>% summarize(n_users= n_distinct(userId)) 
#There are 69,878 unique users

#2.How many ratings per user?
most_user_ratings <- edx %>% count(userId) %>% filter(n>1000) %>% arrange(desc(n()))
most_user_ratings
least_user_ratings <- edx %>% count(userId) %>% filter(n<15) %>% arrange(desc(n()))
least_user_ratings
top_3_users <-edx %>% count(userId) %>% top_n(3)
kable(top_3_users) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
bottom_3_users <- edx %>% count(userId) %>% top_n(-3)
kable(bottom_3_users) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
user_ratings_count <- edx %>%
  group_by(userId) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
#Generate plot
user_ratings_count_plot <- user_ratings_count %>%
  ggplot(aes(count)) + 
  geom_histogram(bins = 10, color = "black", fill = "grey") + 
  scale_x_log10() +
  ggtitle("Ratings per User") + 
  xlab("Number of Ratings") + 
  ylab("Number of Users")
user_ratings_count_plot
#Like in the movie variable, there is a wide spread of rating activity with the users, 
#while there are 610 users that rated more than 10,000 movies; there are 28 that rated less than 15. 
#Most users rated between 20 and 150 movies. After that, the number of ratings declines sharply.  
#As some users are more active than others, these variables also need to be adjusted.

#3. Distribution of ratings
average_rating_user <- edx %>%
  group_by(userId) %>%
  summarize(mean_rating=sum(rating)/n())
#Generate plot
average_rating_user_plot <- average_rating_user %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(bins=10, color = "black", fill = "grey") + 
  ggtitle("Average Ratings per User") + 
  scale_x_continuous(breaks= c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5), labels= c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)) + 
  xlab("Average Rating") + 
  ylab("Users")
average_rating_user_plot
#Even though average rating per user range from 2.5 to 4.5, most users stick to the average rating of ~3.5.
#Useful to know the average rating for all movies.
mean_rating <- mean(edx$rating)
kable(mean_rating) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)

#####   genre   #####
head(edx$genres)
#Exploring this variable, we can see that one single movie belongs to several genres, 
#in order to analyze them, we first need to separate them into individual categories.
edx <- edx %>% separate_rows(genres, sep="\\|")
validation <- validation %>% separate_rows(genres, sep="\\|")

#1. How many ratings per Genre?
genre_rating <- edx %>%
  group_by(genres) %>%
  summarize(count=n(), rating= round(mean(rating), 2)) %>%
  arrange(desc(count))
#Generate table
kable(genre_rating) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
#Generate plot
genre_rating_plot <- genre_rating %>%
  ggplot(aes(genres, count)) + 
  geom_histogram(aes(fill=genres), stat="identity",  bins = 20, color = "black", fill = "grey") +  
  ggtitle("Number of Ratings per Genre") + 
  scale_y_continuous(breaks= c(1000000, 2000000, 3000000, 4000000), labels=c("1", "2", "3", "4")) + 
  xlab("Genres") +  
  ylab("Number of Rated Movies (in Millions)") + 
  theme(axis.text.x=element_text(angle=90, hjust=1))
genre_rating_plot
#We can see that the most rated genre is Drama with 3.9 million ratings
#followed by Comedy, Action and Thriller, very common movie genres;
#while the least ratings are among Documentary, Film-Noir and IMAX,
#the least popular genres. 

#2.Average rating per Genre
average_rating_genre <- edx %>%
  group_by(genres) %>%
  summarize(count=n(), mean_rating=mean(rating)) %>%
  arrange(desc(mean_rating))
kable(average_rating_genre) %>% kable_styling(bootstrap_options="hover", font_size=10, full_width=F)
#Generate plot
average_rating_genre_plot <- average_rating_genre %>% 
  ggplot(aes(genres, mean_rating)) + 
  geom_point() + 
  scale_y_continuous(limits = c(3.5, 4.1)) +  
  ggtitle("Average Rating per Genre") + 
  xlab("Genres") + 
  ylab ("Average Rating") + 
  theme(axis.text.x=element_text(angle=90, hjust=1))
average_rating_genre_plot
#We can see that the genres least rated have the highest average rating (Film-Noir, Documentary, Imax). 
#This biased also needs to be controlled for. 

#####   Year   #####
#This variable contains the name of the movie and the year of its release. The title is useless for the analysis, 
#but we can extract the release year to check if the age of the movie has an effect on rating.
#We extract the release year from both the edx and validation sets. 
edx <- edx %>% mutate(year = as.numeric(str_remove_all(str_extract(title,"\\((\\d{4})\\)"),"\\(|\\)")), title = str_remove(title," \\((\\d{4})\\)"))
validation <- validation %>% mutate(year = as.numeric(str_remove_all(str_extract(title,"\\((\\d{4})\\)"),"\\(|\\)")), title = str_remove(title," \\((\\d{4})\\)"))

#1. How many ratings per year of release?
ratings_year <- edx %>%
  group_by(year) %>%
  summarize(count=n())
#Generate plot
ratings_year_plot<-ratings_year %>%
  ggplot(aes(year, count)) +
  geom_line() +
  ggtitle("Ratings per Release Year") + 
  scale_y_continuous(breaks= c(500000, 1000000, 1500000, 2000000), labels=c(".5", "1", "1.5", "2")) + 
  xlab("Year") + 
  ylab("Number of Ratings (in Millions)")
ratings_year_plot
#We can see that the ratings are not numerous with movies released before 1970, with an upward trend with movies afterwards
#reaching a peak of over 2 million ratings for movies released in 1995 and then declining to 2007. 
#2008 low number of ratings can be due to incomplete data from that year. 

#2. Average ratings per year of Release
average_rating_year <- edx %>%
  group_by(year) %>%
  summarize(mean_rating=mean(rating))
average_rating_year_plot <- average_rating_year %>%
  ggplot(aes(year, mean_rating)) +
  geom_line()+ geom_smooth() +
  scale_y_continuous(limits = c(3.2, 4.2)) +  
  ggtitle("Average Rating per Release Year") + 
  xlab("Year") + 
  ylab("Average Rating")
average_rating_year_plot
#We can see a higher appreciation for older movies starting in the 1920s and a decline in average ratings from 1980s on. 

##########################################################
# Data Modeling
##########################################################
#The objective is to find an algorithm that obtains an RMSE below 0.86490
#We could use linear regression to obtain the best model, but the computing
#capacity needed is beyond what a personal computer can deliver.
#We will first start with a naive model, to then add other variables to the model. 
#After each variable added, we will perform a regularization, which is a technique
#that shrinks or constraints the coefficient estimates towards 0 (or zero). 
#In this technique, a penalty is added to the various parameters of the model in order to reduce the freedom of the given model.
#A table listing the different results will follow every calculation to keep track of the improvements.

#Create train and test set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y= edx$rating, times=1, p= 0.2, list=FALSE)
train <- edx[-test_index,]
test <- edx[test_index,]

test <- test %>%
  semi_join(train, by ="movieId") %>%
  semi_join(train, by="userId")

#Create the list to record the results
Results <- tibble(Method="Objective", RMSE= 0.86490)
Results %>% knitr::kable()

#1.Naive model
mu_hat <- mean(train$rating)
mu_hat
#test Naive model
naive_rmse <- RMSE(test$rating, mu_hat)
naive_rmse
#Add to the Results' list
Results <- bind_rows(Results, tibble(Method="Naive Model", RMSE=naive_rmse))
Results %>% knitr::kable()
#As we can see, the naive model, which is  returns a RMSE of 1.0519, much higher than the goal. 
#It means that our ratings will be off by more than 1 point. 
#This is unacceptable, we will try to improve the model by adding other variables.

#2. Movie bias model  (mu + b_i)
mu <- mean(train$rating)
movie_avg <- train %>%
  group_by(movieId) %>%
  summarize(b_i= mean(rating - mu))
#test movie model
bi_pred <- mu + test %>%
  left_join(movie_avg, by="movieId") %>%
  .$b_i 
#Calculate RMSE
movie_rmse <- RMSE(bi_pred, test$rating)
movie_rmse
#Add to the Results' list
Results <- bind_rows(Results, tibble(Method="Movie Effect Model", RMSE=movie_rmse))
Results %>% knitr::kable()
#We can already see a big improvement against the naive model, yet not good enough to reach the goal.

#2.1 Regularized Movie Model
#Calculate the right lambda
lambdas <- seq(0, 10, 0.25)
mu <- mean(train$rating)
movie_reg_avg <- train %>%
  group_by(movieId) %>% 
  summarize(s= sum(rating-mu))
rmses<-sapply(lambdas, function(l){
  movie_reg_pred <- test %>% 
    left_join (movie_reg_avg, by="movieId") %>% 
    mutate(b_i= s/(n()+l)) %>% 
    mutate(pred= mu + b_i) %>% 
    .$pred
  movie_reg_rmse <- RMSE(movie_reg_pred, test$rating)
})
lambdas[which.min(rmses)]
lambda <- 1.75
#Run the algorithm with the right lambda
mu<- mean(train$rating)
movie_reg<- train %>%
  group_by(movieId) %>%
  summarize(b_i= sum(rating-mu)/(n()+lambda))
moviereg_pred<-test %>% 
  left_join(movie_reg, by="movieId") %>% 
  mutate(pred= mu + b_i) %>% 
  .$pred
#Calculate RMSE
moviereg_rmse <- RMSE(moviereg_pred, test$rating)
moviereg_rmse
#Add to the Results' list
Results <- bind_rows (Results, tibble(Method="Regularized Movie Effect Model", RMSE= moviereg_rmse))
Results %>% knitr::kable()
#This regularization has a minimal improvement on the model.

#3. Integrate the User bias(b_u) to the model = mu + b_i + b_u
user_avg<- train %>%
  left_join(movie_avg, by= "movieId") %>%
  group_by(userId) %>% 
  summarize(b_u= mean(rating - mu - b_i))
#test movie + user model
bu_pred<-test %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by= "userId") %>%
  mutate(pred= mu + b_i + b_u)%>% 
  .$pred
#Calculate RMSE
user_rmse <- RMSE(bu_pred, test$rating)
#Add to the Results' list
Results <- bind_rows(Results, data_frame(Method="User + Movie Effect Model", 
                                       RMSE=user_rmse))
Results %>% knitr::kable()
#Including the user in the model delivers the biggest improvement in the model so far. 

#3.1 Regularized  User + Movie Effect
#Calculate the right lambda
lambdas <- seq(0, 10, 0.25)
rmses <- sapply (lambdas, function(l){
  mu <- mean(train$rating)
  b_i <- train %>%
    group_by(movieId) %>%
    summarize(b_i= sum(rating-mu)/(n()+l))
  b_u <- train %>%
    left_join(b_i, by= "movieId") %>%
    group_by(userId) %>%
    summarize(b_u= sum(rating-b_i-mu)/(n()+l))
  user_reg_pred<- test %>%
    left_join (b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    mutate(pred= mu + b_i + b_u) %>% 
    .$pred
  user_reg_rmse<-RMSE(user_reg_pred, test$rating)
})
lambdas[which.min(rmses)]
lambda <- 4.25
##Run the algorithm with the right lambda
mu <- mean(train$rating)
b_i <- train %>%
  group_by(movieId) %>%
  summarize(b_i= sum(rating - mu)/(n()+lambda))
b_u <- train %>%
  left_join(b_i, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u= sum(rating - b_i - mu)/(n()+lambda))
userreg_pred<-test %>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  mutate(pred= mu + b_i + b_u) %>% 
  .$pred
#Calculate RMSE
userreg_rmse<-RMSE(userreg_pred, test$rating)
userreg_rmse
#Add to the Results' list
Results <- bind_rows (Results, tibble(Method="Regularized User + Movie Effect Model", RMSE= userreg_rmse))
Results %>% knitr::kable()
#Again, the regularization of the model, only has a slight improvement in the model.

#4. Integrate the Genre bias(b_g) to the User + Movie Effect Model = mu + b_i + b_u + b_g
genre_avg <- train %>%
  left_join(movie_avg, by= "movieId") %>%
  left_join(user_avg, by= "userId") %>%
  group_by(genres) %>% 
  summarize (b_g = mean (rating- mu - b_i - b_u))
#test genre + movie + user model
bg_pred <-test %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avg, by= "userId") %>%
  left_join(genre_avg, by= "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% 
  .$pred
#Calculate RMSE
genre_rmse <- RMSE(bg_pred, test$rating)
genre_rmse
#Add to the Results' list
Results <- bind_rows(Results, data_frame(Method="Genre +User + Movie Effect Model", 
                                         RMSE=genre_rmse))
Results %>% knitr::kable()
#Adding the Genre variable into the model has an improvement over the User+Movie Effect model, 
#but not over the Regularized User+ Movie Effect model. 

#4.1 Regularized Genre + User + Movie Model
#Calculate the right lambda
lambdas <-seq(0, 10, 0.25)
rmses <- sapply (lambdas, function(l){
  mu <-mean(train$rating)
  b_i <-train %>%
    group_by(movieId) %>%
    summarize(b_i= sum(rating - mu)/(n()+l))
  b_u <- train %>%
    left_join(b_i, by= "movieId") %>%
    group_by(userId) %>%
    summarize(b_u= sum(rating - b_i - mu)/(n()+l))
  b_g <- train%>%
    left_join(b_i, by= "movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(genres) %>%
    summarize(b_g= sum(rating - b_u - b_i - mu)/(n()+l))
  genre_reg_pred<- test %>%
    left_join (b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    left_join(b_g, by="genres") %>% 
    mutate(pred= mu + b_i + b_u + b_g) %>% 
    .$pred
  genre_reg_rmse <-RMSE(genre_reg_pred, test$rating)
})
lambdas[which.min(rmses)]
lambda <-4.25
#Run the algorithm with the right lambda
mu <- mean(train$rating)
b_i <- train %>%
  group_by(movieId) %>%
  summarize(b_i= sum(rating - mu)/(n()+lambda))
b_u <- train %>%
  left_join(b_i, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u= sum(rating - b_i - mu)/(n()+lambda))
b_g <- train%>%
  left_join(b_i, by= "movieId") %>%
  left_join(b_u, by= "userId") %>%
  group_by(genres) %>%
  summarize(b_g= sum(rating - b_u - b_i - mu)/(n()+lambda))
genrereg_pred<-test %>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  left_join(b_g, by="genres") %>% 
  mutate(pred= mu + b_i + b_u + b_g) %>% 
  .$pred
#Calculate RMSE
genrereg_rmse<-RMSE(genrereg_pred, test$rating)
genrereg_rmse
#Add to the Results' list
Results <- bind_rows (Results, tibble(Method="Regularized Genre +User + Movie Effect Model", RMSE= genrereg_rmse))
Results %>% knitr::kable()
#This regularizad version of the model keeps improving the model.
#Even though we have reached and surpassed the goal, we will add a last variable to see if we can further improve the model.

#5. Integrate the Release Year of the movie bias(b_y) to the Genre + User + Movie Model = mu + b_i + b_u + b_g + b_y
year_avg<- train%>%
  left_join(movie_avg, by= "movieId")%>%
  left_join(user_avg, by= "userId")%>%
  left_join(genre_avg, by= "genres")%>%
  group_by(year) %>% 
  summarize (b_y = mean (rating- mu - b_i - b_u - b_g))
by_pred<-test%>%
  left_join(movie_avg, by="movieId")%>%
  left_join(user_avg, by= "userId")%>%
  left_join(genre_avg, by= "genres")%>%
  left_join(year_avg, by= "year")%>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  .$pred
#Calculate RMSE
year_rmse <- RMSE(by_pred, test$rating)
#Add to the Results' list
Results<-bind_rows(Results, data_frame(Method="Release Year + Genre +User + Movie Effect Model", 
                                       RMSE=year_rmse))
Results %>% knitr::kable()
#Indeed, there is room for more improvement.

#5.1 Regularize Year +Genre +User + Movie Effect Model
lambdas<-seq(0, 10, 0.25)
rmses<- sapply (lambdas, function(l){
  mu <- mean(train$rating)
  b_i <- train %>%
    group_by(movieId) %>%
    summarize(b_i= sum(rating-mu)/(n()+l))
  b_u <- train %>%
    left_join(b_i, by= "movieId") %>%
    group_by(userId) %>%
    summarize(b_u= sum(rating - b_i - mu)/(n()+l))
  b_g <- train %>%
    left_join(b_i, by= "movieId") %>%
    left_join(b_u, by= "userId") %>%
    group_by(genres) %>%
    summarize(b_g= sum(rating - b_u - b_i- mu)/(n()+l))
  b_y <- train %>%
    left_join(b_i, by= "movieId") %>%
    left_join(b_u, by= "userId") %>%
    left_join(b_g, by= "genres") %>%
    group_by(year) %>%
    summarize(b_y= sum(rating - b_g - b_u - b_i - mu)/(n()+l))
  year_reg_pred<- test%>%
    left_join(b_i, by="movieId") %>% 
    left_join(b_u, by="userId") %>% 
    left_join(b_g, by="genres") %>% 
    left_join(b_y, by="year") %>% 
    mutate(pred= mu + b_i + b_u + b_g + b_y) %>% 
    .$pred
  year_reg_rmse <- RMSE(year_reg_pred, test$rating)
})
lambdas[which.min(rmses)]
lambda<-3.75
#Run the algorithm with the right lambda
mu <- mean(train$rating)
b_i <- train %>%
  group_by(movieId) %>%
  summarize(b_i= sum(rating - mu)/(n()+lambda))
b_u <- train %>%
  left_join(b_i, by="movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u= sum(rating - b_i - mu)/(n()+lambda))
b_g <- train %>%
  left_join(b_i, by= "movieId") %>%
  left_join(b_u, by= "userId") %>%
  group_by(genres) %>%
  summarize(b_g= sum(rating - b_u - b_i - mu)/(n()+lambda))
b_y <- train %>%
  left_join(b_i, by= "movieId") %>%
  left_join(b_u, by= "userId") %>%
  left_join(b_g, by= "genres") %>%
  group_by(year) %>%
  summarize(b_y= sum(rating - b_g - b_u - b_i - mu)/(n()+lambda))
yearreg_pred<-test %>% 
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  left_join(b_g, by="genres") %>% 
  left_join(b_y, by="year") %>%
  mutate(pred= mu + b_i + b_u + b_g + b_y) %>% 
  .$pred
#Calculate the RMSE
yearreg_rmse <- RMSE(yearreg_pred, test$rating)
yearreg_rmse
#Add to the Results' list
Results<- bind_rows (Results, data_frame(Method="Regularized Release Year + Genre + User + Movie Effect Model", 
                                         RMSE= yearreg_rmse))
Results %>% knitr::kable()
#The regularized version of this model obtains a RMSE of p.8569670, well below the expected goal. 
#We could potentially also include the timestamp variable and analyze the effects of the time of review. 
#But, the computer is taking long enough calculating the right lambdas as it is, so I will leave that analysis for the future.
#Instead, we will turn to the final validation of the model.

#Finally: Predict ratings on the Validation Set
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm = T))
}
predicted_ratings <- validation %>%
  left_join(b_i, by="movieId") %>% 
  left_join(b_u, by="userId") %>% 
  left_join(b_g, by="genres") %>% 
  left_join(b_y, by="year") %>%
  mutate(pred= mu + b_i + b_u + b_g + b_y) %>% 
  .$pred
#Calculate the RMSE
validation_rmse <- RMSE(validation$rating, predicted_ratings)
validation_rmse
#Add to the Results' list
Results<- bind_rows (Results, data_frame(Method="Validation Set", 
                                         RMSE= validation_rmse))
Results %>% knitr::kable()
#An RMSE of 0.8629121! Goal attained.
##################################################################################