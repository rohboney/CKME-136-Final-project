library(caret)
library(randomForest)
library(tidyverse)
library(corrplot)
#import excel spreadsheet 
movie_data <- read.csv("C:/Users/rohan/Documents/movie_data.csv", header = T, stringsAsFactors = F)
#inspect data
str(movie_data)
movie_data$movie_title <- as.factor(movie_data$movie_title)
summary(movie_data$movie_title)
#Remove duplicates and correct data types
movie_data <- movie_data[!duplicated(movie_data$movie_title), ]
summary(movie_data$movie_title)
movie_data$movie_title <- as.character(movie_data$movie_title)
str(movie_data)
movie_data$color <- as.factor(movie_data$color)
movie_data$director_name <- as.factor(movie_data$director_name)
movie_data$actor_2_name <- as.factor(movie_data$actor_2_name)
movie_data$actor_1_name <- as.factor(movie_data$actor_1_name)
movie_data$actor_3_name <- as.factor(movie_data$actor_3_name)
movie_data$language <- as.factor(movie_data$language)
movie_data$country <- as.factor(movie_data$country)
movie_data$content_rating <- as.factor(movie_data$content_rating)
str(movie_data)
#remove plot keywords and imbd link
movie_data <- movie_data[ ,-c(24,25)]
#remove na's from dataset and turn 0's into na
movie_data <- movie_data[!is.na(movie_data$gross), ]
movie_data[movie_data == 0] <- NA
colSums(is.na(movie_data))
#remove aspect ratio and facenumber in poster
movie_data <- movie_data[ , -c(23, 32)]
movie_data <- na.omit(movie_data)
colSums(is.na(movie_data))
#make new column for genres using binary variables
movie_data$action <- ifelse( movie_data$genres == "Action", 1,
                     ifelse(movie_data$genre.2 =="Action", 1, 
                    ifelse(movie_data$genre.3 == "Action", 1, 
                    ifelse(movie_data$genre.4 == "Action", 1, 
                    ifelse (movie_data$genre.5 == "Action", 1, 
                    ifelse(movie_data$genre.6 == "Action", 1,
                    ifelse(movie_data$genre.7 == "Action", 1,
                    ifelse(movie_data$genre.8 == "Action", 1, 0))))))))
movie_data$adventure <- ifelse( movie_data$genres == "Adventure", 1,
                        ifelse(movie_data$genre.2 =="Adventure", 1, 
                        ifelse(movie_data$genre.3 == "Adventure", 1, 
                        ifelse(movie_data$genre.4 == "Adventure", 1, 
                        ifelse (movie_data$genre.5 == "Adventure", 1, 
                        ifelse(movie_data$genre.6 == "Adventure", 1,
                        ifelse(movie_data$genre.7 == "Adventure", 1,
                        ifelse(movie_data$genre.8 == "Adventure", 1, 0))))))))
movie_data$animation<- ifelse( movie_data$genres == "Animation", 1,
                       ifelse(movie_data$genre.2 =="Animation", 1, 
                       ifelse(movie_data$genre.3 == "Animation", 1, 
                       ifelse(movie_data$genre.4 == "Animation", 1, 
                       ifelse (movie_data$genre.5 == "Animation", 1, 
                        ifelse(movie_data$genre.6 == "Animation", 1,
                        ifelse(movie_data$genre.7 == "Animation", 1,
                        ifelse(movie_data$genre.8 == "Animation", 1, 0))))))))
movie_data$biography  <- ifelse( movie_data$genres == "Biography", 1,
                        ifelse(movie_data$genre.2 =="Biography", 1, 
                        ifelse(movie_data$genre.3 == "Biography", 1, 
                        ifelse(movie_data$genre.4 == "Biography", 1, 
                        ifelse (movie_data$genre.5 == "Biography", 1, 
                        ifelse(movie_data$genre.6 == "Biography", 1,
                        ifelse(movie_data$genre.7 == "Biography", 1,
                        ifelse(movie_data$genre.8 == "Biography", 1, 0))))))))
movie_data$comedy  <- ifelse( movie_data$genres == "Comedy", 1,
                      ifelse(movie_data$genre.2 =="Comedy", 1, 
                      ifelse(movie_data$genre.3 == "Comedy", 1, 
                      ifelse(movie_data$genre.4 == "Comedy", 1, 
                      ifelse (movie_data$genre.5 == "Comedy", 1, 
                      ifelse(movie_data$genre.6 == "Comedy", 1,
                      ifelse(movie_data$genre.7 == "Comedy", 1,
                      ifelse(movie_data$genre.8 == "Comedy", 1, 0))))))))
movie_data$crime  <- ifelse( movie_data$genres == "Crime", 1,
                    ifelse(movie_data$genre.2 =="Crime", 1, 
                    ifelse(movie_data$genre.3 == "Crime", 1, 
                    ifelse(movie_data$genre.4 == "Crime", 1, 
                    ifelse (movie_data$genre.5 == "Crime", 1, 
                    ifelse(movie_data$genre.6 == "Crime", 1,
                     ifelse(movie_data$genre.7 == "Crime", 1,
                     ifelse(movie_data$genre.8 == "Crime",1, 0))))))))
movie_data$documentary  <- ifelse( movie_data$genres == "Documentary", 1,
                          ifelse(movie_data$genre.2 =="Documentary", 1, 
                          ifelse(movie_data$genre.3 == "Documentary", 1, 
                          ifelse(movie_data$genre.4 == "Documentary", 1, 
                          ifelse (movie_data$genre.5 == "Documentary", 1, 
                          ifelse(movie_data$genre.6 == "Documentary", 1,
                          ifelse(movie_data$genre.7 == "Documentary", 1,
                          ifelse(movie_data$genre.8 == "Documentary",1, 0))))))))
movie_data$drama  <- ifelse( movie_data$genres == "Drama", 1,
                      ifelse(movie_data$genre.2 =="Drama", 1, 
                      ifelse(movie_data$genre.3 == "Drama", 1, 
                      ifelse(movie_data$genre.4 == "Drama", 1, 
                      ifelse (movie_data$genre.5 == "Drama", 1, 
                      ifelse(movie_data$genre.6 == "Drama", 1,
                      ifelse(movie_data$genre.7 == "Drama", 1,
                      ifelse(movie_data$genre.8 == "Drama",1, 0))))))))
movie_data$fantasy  <- ifelse( movie_data$genres == "Fantasy", 1,
                       ifelse(movie_data$genre.2 =="Fantasy", 1, 
                       ifelse(movie_data$genre.3 == "Fantasy", 1, 
                        ifelse(movie_data$genre.4 == "Fantasy", 1, 
                        ifelse (movie_data$genre.5 == "Fantasy", 1, 
                        ifelse(movie_data$genre.6 == "Fantasy", 1,
                        ifelse(movie_data$genre.7 == "Fantasy", 1,
                        ifelse(movie_data$genre.8 == "Fantasy",1, 0))))))))
movie_data$horror  <- ifelse( movie_data$genres == "Horror", 1,
                      ifelse(movie_data$genre.2 =="Horror", 1, 
                      ifelse(movie_data$genre.3 == "Horror", 1, 
                      ifelse(movie_data$genre.4 == "Horror", 1, 
                      ifelse (movie_data$genre.5 == "Horror", 1, 
                      ifelse(movie_data$genre.6 == "Horror", 1,
                      ifelse(movie_data$genre.7 == "Horror", 1,
                      ifelse(movie_data$genre.8 == "Horror",1, 0))))))))
movie_data$mystery  <- ifelse( movie_data$genres == "Mystery", 1,
                      ifelse(movie_data$genre.2 =="Mystery", 1, 
                      ifelse(movie_data$genre.3 == "Mystery", 1, 
                      ifelse(movie_data$genre.4 == "Mystery", 1, 
                      ifelse (movie_data$genre.5 == "Mystery", 1, 
                      ifelse(movie_data$genre.6 == "Mystery", 1,
                     ifelse(movie_data$genre.7 == "Mystery", 1,
                      ifelse(movie_data$genre.8 == "Mystery",1, 0))))))))
movie_data$romance <- ifelse( movie_data$genres == "Romance", 1,
                      ifelse(movie_data$genre.2 =="Romance", 1, 
                      ifelse(movie_data$genre.3 == "Romance", 1, 
                      ifelse(movie_data$genre.4 == "Romance", 1, 
                      ifelse (movie_data$genre.5 == "Romance", 1, 
                      ifelse(movie_data$genre.6 == "Romance", 1,
                      ifelse(movie_data$genre.7 == "Romance", 1,
                      ifelse(movie_data$genre.8 == "Romance",1, 0))))))))
movie_data$sci_Fi <- ifelse( movie_data$genres == "Sci-Fi", 1,
                    ifelse(movie_data$genre.2 =="Sci-Fi", 1, 
                    ifelse(movie_data$genre.3 == "Sci-Fi", 1, 
                    ifelse(movie_data$genre.4 == "Sci-Fi", 1, 
                    ifelse (movie_data$genre.5 == "Sci-Fi", 1, 
                    ifelse(movie_data$genre.6 == "Sci-Fi", 1,
                    ifelse(movie_data$genre.7 == "Sci-Fi", 1,
                    ifelse(movie_data$genre.8 == "Sci-Fi",1, 0))))))))
movie_data$thriller <- ifelse( movie_data$genres == "Thriller", 1,
                      ifelse(movie_data$genre.2 =="Thriller", 1, 
                      ifelse(movie_data$genre.3 == "Thriller", 1, 
                       ifelse(movie_data$genre.4 == "Thriller", 1, 
                       ifelse (movie_data$genre.5 == "Thriller", 1, 
                      ifelse(movie_data$genre.6 == "Thriller", 1,
                      ifelse(movie_data$genre.7 == "Thriller", 1,
                      ifelse(movie_data$genre.8 == "Thriller",1, 0))))))))
#remove old data columns
movie_data <- movie_data[ , -c(10:17)]
#make new columns grouping movies by their gross revenue
movie_data$boxoffice <- ifelse(movie_data$gross >= 100000000,"A", 
                        ifelse(movie_data$gross < 100000000 & movie_data$gross >= 80000000, "B",
                        ifelse(movie_data$gross < 80000000 & movie_data$gross >= 60000000, "C",
                        ifelse (movie_data$gross < 60000000 & movie_data$gross >=40000000, "D",
                        ifelse (movie_data$gross  < 40000000 & movie_data$gross >= 30000000, "E",    
                        ifelse (movie_data$gross < 30000000 & movie_data$gross >= 20000000, "F", 
                        ifelse (movie_data$gross < 20000000 & movie_data$gross >= 10000000, "G",   
                        ifelse (movie_data$gross < 10000000 & movie_data$gross >=1000000, "H", "I"))))))))
movie_data$boxoffice <- as.factor(movie_data$boxoffice)
#view and remove outliers from gross
boxplot(movie_data$gross)
outlier <- boxplot(movie_data$gross, plot = F)$out
movie_data <- movie_data[-which(movie_data$gross %in% outlier), ]
boxplot(movie_data$gross)
rownames(movie_data) <- 1:nrow(movie_data)
#normalize data 
normalize <- function(x){return((x - min(x))/ (max(x)- min(x))) }
movies<- movie_data[ , c(3,4,5,6,8,9,12,13,15,19,20,21,22,23)]
movies <- as.data.frame(sapply(movies, normalize))
movies <- cbind(movies, movie_data[ , c(24:37)])
mov <- cor(movies)
print(mov)
corrplot(mov)
cor(movies, method = "spearman")
#make a linear regression
linregress <- lm(movies$gross ~movies$num_critic_for_reviews + movies$num_voted_users +
                   movies$budget + movies$num_user_for_reviews + movies$movie_facebook_likes)
summary(linregress)
#now do classification method
moviess <- cbind(movie_data[ , c(1,2,7,10,14,16,17,18, 38)], movies)  
moviess <- moviess[ , -c(1:8, 15)]
#make training and test set
samp <- sample(nrow(moviess), .8*nrow(moviess), replace = F)
training <- moviess[samp, ]
testing <- moviess[-samp, ]
tree <- rpart(boxoffice ~ ., training, method = "class")
pred <- predict(tree, testing, type = "class")
plot(tree)
text(tree)
#make confusion matrix and get accuracy precision and recall
conf <- table(testing$boxoffice, pred)
print(conf)
dia <- diag(conf)
rowsums <- apply(conf, 1, sum)
colsums <- apply(conf, 2, sum)
confsum <- sum(conf)
acc <- sum(dia)/confsum
print(acc)
precision <- dia/colsums
print(precision)
recal <- dia/ rowsums
print(recal)
#run a random forest
model <- randomForest(training$boxoffice ~ ., data= training, importance= T )
model
#finish with a knn
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(boxoffice ~., data = training, method = "knn", trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)
print(knn_fit)
trainnn <- training[, -1]
testt <- testing[ , -1]
mm <-knn(trainnn, testt, cl = training$boxoffice, k =11)
confusionMatrix(mm, testing$boxoffice)
