library(class)
library(gmodels)
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
movie_data_test$original_genre=c()
movie_data_nums_norm <- as.data.frame(lapply(movie_data_nums[1:movie_data_coln], scale))
movie_data_nums_train_norm<-movie_data_nums_norm[1:4000,]
movie_data_nums_test_norm<-movie_data_nums_norm[4001:movie_data_rows,]

change_genre<-function()
{
  test_labels <- movie_data[4001:movie_data_rows, 10]
  split=strsplit(as.character(test_labels),"|",fixed=TRUE) 
  temp=do.call(rbind,split)
  test_labels=temp[,1]
  return(test_labels)
}
knn_measure<-function()
{
  train_labels <- movie_data[1:4000, 10]
  test_labels <- movie_data[4001:movie_data_rows, 10]
  split=strsplit(as.character(test_labels),"|",fixed=TRUE) 
  temp=do.call(rbind,split)
  test_labels=temp[,1]
  
  split=strsplit(as.character(train_labels),"|",fixed=TRUE) 
  temp=do.call(rbind,split)
  train_labels<-temp[,1]
  test_pred <- knn(train = movie_data_nums_train_norm, test = movie_data_nums_test_norm,cl = train_labels, k=sqrt(4000))
  return(test_pred)
  
}

movie_data_test$predicted_genre<-knn_measure()
movie_data_test$original_genre<-change_genre()
vc<-c(29,30)
print(movie_data_test[1:10,vc])
y = CrossTable(x=movie_data_test$ original_genre,y=movie_data_test$predicted_genre)
View(y[1])
write.csv(y[[1]], file = "cnfmatrix.csv")
