#library(ggplot2)
movie_data = read.csv("movie_metadata.csv",head=TRUE,sep=",")
nums <- sapply(movie_data, is.numeric)
movie_data_nums<- movie_data[,nums]
movie_data_nums[is.na(movie_data_nums)] <- 0
movie_data_rows<-nrow(movie_data_nums)
movie_data_coln<-ncol(movie_data_nums)
movie_data_nums_train<-movie_data_nums[1:4000,]
movie_data_nums_test<-movie_data_nums[4001:movie_data_rows,]

movie_data_nums_test$predict_score=c()
train_regression<-function()
{
model<-lm(movie_data_nums_train$imdb_score~. ,data = movie_data_nums_train) 
output<-predict(model,movie_data_nums_test)
return(output)
}
calculate_rmse<-function()
{
  diffxy<-0
  diffxysquare<-0
  vect1<-movie_data_nums_test$imdb_score
  vect2<-movie_data_nums_test$predict_score
  len<-length(vect1)
  for(x in 1:len){
    if(!is.na(vect1[x])&&!is.na(vect2[x]))
    {
      diffxy<-(vect1[x]-vect2[x])
      diffxysquare<-diffxysquare+(diffxy*diffxy)
    }
  }  
  rmse<-diffxysquare/len
  rmse<-sqrt(rmse)
  return(rmse)
}
movie_data_nums_test$predict_score<-train_regression()
#qplot(movie_data_nums_test$imdb_score,movie_data_nums_test$predict_score,geom = c("point", "smooth"))
rmse_val<-calculate_rmse()
print("Calculated RMSE")
print(rmse_val)
