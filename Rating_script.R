if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(knitr)
library(kableExtra)
library(data.table)
library(dplyr)
library(rafalib)

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
set.seed(1)
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

set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in  test set are also in train set
test <- temp %>%
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from testset back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(test_index, temp, removed)

train %>% glimpse()

users <- sample(unique(train$userId), 100)
rafalib::mypar()
train %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>%
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
title("Ratings")

edx %>%
  group_by(title) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  top_n(10, count) %>%
  ggplot(aes(count, reorder(title, count))) +
  geom_bar(color = "black", fill = "blue", stat = "identity") +
  xlab("Count") +
  ylab(NULL) +
  theme_bw()


edx %>%
  ggplot(aes(rating, y = ..prop..)) +
  geom_bar(color = "black", fill = "blue") +
  labs(x = "Ratings", y = "Relative Frequency") +
  scale_x_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
  theme_bw()


edx %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black", fill = "blue", alpha = 0.75) +
  scale_x_log10() +
  ggtitle("Grouping Movies by Number of Ratings") +
  xlab("Number of Ratings") + ylab("Number of Movies")


edx %>%
  group_by(movieId, title) %>%
  summarize(No_Ratings = n(), Average = mean(rating),
            .groups = 'drop') %>%
  arrange(desc(No_Ratings)) %>%
  slice_head(n = 10) %>% kbl(.,booktabs = T) %>%   kable_styling(latex_options = c("striped","hold_position"))

edx %>% group_by(movieId) %>%
  summarize(Average = mean(rating)) %>%
  ggplot(aes(x = Average)) +
  geom_histogram(bins = 30, color = "black", fill = "blue", alpha = 0.75) +
  ggtitle("Grouping Movies by Average Rating") +
  xlab("Average Rating") + ylab("Number of Movies")

edx %>%
  group_by(userId) %>%
  summarize(n = n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black", fill = "darkgreen", alpha = 0.75) +
  scale_x_log10() +
  ggtitle("Grouping Users by Number of Ratings") +
  xlab("Number of Ratings") + ylab("Number of Users")

edx %>%
  group_by(userId) %>%
  summarize(No_Ratings = n(), Average = mean(rating)) %>%
  arrange(desc(No_Ratings)) %>%
  top_n(n = 10, No_Ratings) %>% kbl(.,booktabs = T) %>%   kable_styling(latex_options = c("striped","hold_position"))

edx %>%
  group_by(userId) %>%
  summarize(Average = mean(rating)) %>%
  ggplot(aes(x = Average)) +
  geom_histogram(bins = 30, color = "black", fill = "darkgreen", alpha = 0.75) +
  ggtitle("Grouping Users by Average Rating") +
  xlab("Average Rating") + ylab("Number of Users")

edx %>%
  mutate(Year = year(as_datetime(timestamp))) %>%
  group_by(Year) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(Year, avg_rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Effect") +
  xlab("Date") + ylab("Rating")

edx %>%
  mutate(Month = month(as_datetime(timestamp))) %>%
  group_by(Month) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(Month, avg_rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Effect") +
  xlab("Date") + ylab("Rating")

edx %>%
  mutate(Day = day(as_datetime(timestamp))) %>%
  group_by(Day) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(Day, avg_rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Time Effect") +
  xlab("Date") + ylab("Rating")

edx %>%
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), stdev = sd(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n > 100000) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar(width=0.75, colour="black", alpha=0.75, size=0.25) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept=mean(edx$rating),color="blue",linetype='dashed') +
  ggtitle("Genre Effect") +
  xlab("Genres") + ylab("Rating")

mu <- mean(train$rating)

naive_rmse <- RMSE(test$rating,mu)
result <- tibble(Method = "Naive Bayes", RMSE = naive_rmse)

movie_avg <-
  train %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu),
            .groups='drop')

model_2 <-
  mu + test %>%
  left_join(movie_avg,by="movieId") %>%
  pull(b_i)

movies_rmse <- RMSE(test$rating,model_2)

result <- bind_rows(result,
                    tibble(
                      Method = "Movie Effect",
                      RMSE = movies_rmse)
)

user_avg <- train %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

model_3 <- test %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

movie_user_rmse <- RMSE(test$rating, model_3)
result <- bind_rows(result,
                    tibble(
                      Method = "Movie + User Effects",
                      RMSE = movie_user_rmse)
)

genre_avg <- train %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

model_4 <- validation %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(genre_avg, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

movie_user_genre_rmse <- RMSE(validation$rating, model_4)
result <- bind_rows(result,
                    tibble(
                      Method = "Movie + User + Genres Effects",
                      RMSE = movie_user_genre_rmse)
)

train <- train %>% mutate(Year = year(as_datetime(timestamp)))

test <- test %>% mutate(Year = year(as_datetime(timestamp)))

year_avg <- train %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(genre_avg, by='genres') %>%
  group_by(Year) %>%
  summarize(b_yr = mean(rating - mu - b_i - b_u - b_g))

model_5 <- test %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(genre_avg, by='genres') %>%
  left_join(year_avg, by='Year') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_yr) %>%
  pull(pred)

movie_user_genre_year_rmse <- RMSE(test$rating, model_5)
result <- bind_rows(result,
                    tibble(
                      Method = "Movie + User + Genres + Year Effects",
                      RMSE = movie_user_genre_year_rmse)
)

edx <- edx %>% mutate(Year = year(as_datetime(timestamp)))

validation <- validation %>% mutate(Year = year(as_datetime(timestamp)))

movie_avg <-
  edx %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu),
            .groups='drop')

user_avg <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

genre_avg <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

year_avg <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(genre_avg, by='genres') %>%
  group_by(Year) %>%
  summarize(b_yr = mean(rating - mu - b_i - b_u - b_g))

model_5 <- validation %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(genre_avg, by='genres') %>%
  left_join(year_avg, by='Year') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_yr) %>%
  pull(pred)

movie_user_genre_year_rmse <- RMSE(validation$rating, model_5)
result <- bind_rows(result,
                    tibble(
                      Method = "Model Validation",
                      RMSE = movie_user_genre_year_rmse)
)

movie_titles <- edx %>%
  select(movieId, title) %>%
  distinct()

edx %>%
  count(movieId) %>%
  left_join(movie_avg, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10)


lambda <- 1
movie_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

tibble(original = movie_avg$b_i,
       regularlized = movie_reg$b_i,
       n = movie_reg$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) +
  geom_point(shape=1, alpha=0.5)

ls_focused <- seq(0, 2, 0.1)

reg_rmses_focused <- sapply(ls_focused, function(l) {

  # Calculate the regularized avges for movie, user, genre, and time

  movie_avgs_reg <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))

  user_avgs_reg <- edx %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i) / (n() + l))

  genre_avgs_reg <- edx %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + l))

  year_avgs_reg <- edx %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    left_join(genre_avgs_reg, by='genres') %>%
    group_by(Year) %>%
    summarize(b_yr = sum(rating - mu - b_i - b_u - b_g) / (n() + l))

  #predict the movie ratings on edx set
  pred_movie_user_genre_year_reg <- edx %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    left_join(genre_avgs_reg, by='genres') %>%
    left_join(year_avgs_reg, by='Year') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_yr) %>%
    pull(pred)

  #calculate RMSE
  RMSE(edx$rating, pred_movie_user_genre_year_reg)
})

# pick the final lambda
plot(ls_focused, reg_rmses_focused)

l_final <- ls_focused[which.min(reg_rmses_focused)]

reg_rmse_validation <- sapply(l_final, function(l) {

  # Calculate the regularized avg for movie, user, genre, and time
  movie_avgs_reg <- validation %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))

  user_avgs_reg <- validation %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i) / (n() + l))

  genre_avgs_reg <- validation %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + l))

  year_avgs_reg <- validation %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    left_join(genre_avgs_reg, by='genres') %>%
    group_by(Year) %>%
    summarize(b_yr = sum(rating - mu - b_i - b_u - b_g) / (n() + l))

  #predict the movie ratings on validation set
  pred_movie_user_genre_year_reg <- validation %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    left_join(genre_avgs_reg, by='genres') %>%
    left_join(year_avgs_reg, by='Year') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_yr) %>%
    .$pred

  #calculate RMSE
  RMSE(validation$rating, pred_movie_user_genre_year_reg)
})

result <-
  result %>%
  add_row("Method" = "Regularized Model", "RMSE" = reg_rmse_validation)

l_final <- ls_focused[which.min(reg_rmses_focused)]

reg_rmse_test <- sapply(l_final, function(l) {

  # Calculate the regularized avg for movie, user, genre, and time
  movie_avgs_reg <- test %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu) / (n() + l))

  user_avgs_reg <- test %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i) / (n() + l))

  genre_avgs_reg <- test %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + l))

  year_avgs_reg <- test %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    left_join(genre_avgs_reg, by='genres') %>%
    group_by(Year) %>%
    summarize(b_yr = sum(rating - mu - b_i - b_u - b_g) / (n() + l))

  #predict the movie ratings on test set
  pred_movie_user_genre_year_reg <- test %>%
    left_join(movie_avgs_reg, by='movieId') %>%
    left_join(user_avgs_reg, by='userId') %>%
    left_join(genre_avgs_reg, by='genres') %>%
    left_join(year_avgs_reg, by='Year') %>%
    mutate(pred = mu + b_i + b_u + b_g + b_yr) %>%
    .$pred

  #calculate RMSE
  RMSE(test$rating, pred_movie_user_genre_year_reg)
})

result <-
  result %>%
  add_row("Method" = "Model Evaluation", "RMSE" = reg_rmse_test)

result

matrix_fact <-
  function(R,P,Q,K, steps=5000,alpha=0.002,beta=0.02){
    #R rating matrix
    #P user features matrix
    #Q item (movie) features matrix
    #K latent features
    #steps = iterations
    #alpha = learning rate
    #beta = regularization param
    Q = t(Q)
    for (step in 1:steps){
      for (i in 1:nrow(R)){
        for (j in 1:ncol(R)){
          if (R[i,j] >0) {

            eij <- R[i,j] - (P[i,]%*%Q[,j])

            for (k in 1:K){
              P[i,k] = P[i,k] + alpha * (2 * eij * Q[k,j] - beta * P[i,k])
              Q[k,j] = Q[k,j] + alpha * (2 * eij * P[i,k] - beta * Q[k,j])
            }
          }
        }
      }

      e = 0

      for (i in 1:nrow(R)){

        for (j in 1:ncol(R)){

          if (R[i,j] > 0) {

            e = e + (R[i,j]-(P[i,]%*%Q[,j]))^2

            for (k in 1:K){
              e = e + (beta/2) * ((P[i,k])^2+ (Q[k,j])^2)
            }

          }

        }

      }

      if (e < 0.001){
        break
      }

    }

    return(list(
      P=P,
      Q = t(Q))
    )

  }

users <- sample(unique(train$userId), 10)

# R is the rating matrix grid and
R<-
  train %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 10)) %>%
  as.matrix() %>% replace_na(0)
R

N <- nrow(R)
M <- ncol(R)
#latent features:
K = 3

P <- matrix(rexp(N*K), N)
Q <- matrix(rexp(M*K), M)

output <- matrix_fact(R,P,Q,K)
output

mf_example <- output$P %*% t(output$Q)
mf_example

library(recosystem)
#Convert train set into rating matrix grid
train_set = data_memory(user_index = train$userId,
                        item_index = train$movieId,
                        rating = train$rating, index1 = TRUE)

r = Reco()
opts_tune = r$tune(train_set,
                   opts = list(dim = c(10, 20, 30),
                               costp_l2 = c(0.01, 0.1),
                               costq_l2 = c(0.01, 0.1),
                               costp_l1 = 0,
                               costq_l1 = 0,
                               lrate    = c(0.01, 0.1),
                               nthread  = 4,
                               niter    = 10,
                               verbose  = TRUE))
r$train(train_set, opts = c(opts_tune$min,
                            niter = 100, nthread = 4))


test_set= data_memory(test$userId, test$movieId, index1 = TRUE)
pred = r$predict(test_set, out_memory())
mf_rmse <- RMSE(pred,test$rating)

result <-
  result %>%
  add_row("Method" = "Matrix Factorization", "RMSE" = mf_rmse)

result

user = 1:20
movie = 1:20
pred = expand.grid(user = user, movie = movie)
test_set = data_memory(pred$user, pred$movie, index1 = TRUE)
pred$rating = r$predict(test_set, out_memory())

library(ggplot2)
ggplot(pred, aes(x = movie, y = user, fill = rating)) +
  geom_raster() +
  scale_fill_gradient("Rating", low = "#d6e685", high = "#1e6823") +
  xlab("Movie ID") + ylab("User ID") +
  coord_fixed() +
  theme_bw(base_size = 22)



