library(ggplot2)
library(class)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
movie_data = read.csv("movie_metadata.csv",head=TRUE,sep=",")
nums <- sapply(movie_data, is.numeric)


movie_data_nums<- movie_data[,nums]
movie_data_nums[is.na(movie_data_nums)] <- 0
movie_data_rows<-nrow(movie_data_nums)
movie_data_coln<-ncol(movie_data_nums)
movie_data_test<-movie_data[4001:movie_data_rows,]
movie_data_test$predicted_genre=c()
movie_data_nums_norm <- as.data.frame(lapply(movie_data_nums[1:movie_data_coln], scale))
movie_data_nums_train_norm<-movie_data_nums_norm[1:4000,]
movie_data_nums_test_norm<-movie_data_nums_norm[4001:movie_data_rows,]

knn_measure<-function()
{
  train_labels <- movie_data[1:4000, 10]
  test_labels <- movie_data[4001:movie_data_rows, 10]
  test_pred <- knn(train = movie_data_nums_train_norm, test = movie_data_nums_test_norm,cl = train_labels, k=20)
  return(test_pred)
  
}

movie_data_test$predicted_genre<-knn_measure()
vc<-c(10,29)
print(movie_data_test[1:100,vc])