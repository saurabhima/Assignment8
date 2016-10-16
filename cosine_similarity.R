library(dplyr)
movie_data = read.csv("movie_metadata.csv",head=TRUE,sep=",")
nums <- sapply(movie_data, is.numeric)
movie_data_nums<- movie_data[,nums]
movie_data_rows<-nrow(movie_data_nums)
movie_data_coln<-ncol(movie_data_nums)

calc_cosine_similarity<-function(row1,row2)
{
  sumxy<-0
  vect1=as.numeric(movie_data_nums[row1,])
  vect2=as.numeric(movie_data_nums[row2,])
  for(x in 1:movie_data_coln){
    sumxy=sumxy+(vect1[x]*vect2[x])  
  }
  sumxsquare<-0
  sumysquare<-0
  for(y in 1:movie_data_coln){
  sumxsquare<-sumxsquare+(vect1[y]*vect1[y])
  sumysquare<-sumysquare+(vect2[y]*vect2[y])
  }
  cosinesimilarity<-sumxy/(sqrt(sumxsquare)*sqrt(sumysquare))
return(cosinesimilarity)
  }

consine_func<-function(value_a)
{
  topdata <- data.frame(cosval= numeric(0), movie_name = character(0))
  a<-value_a
    for (b in 1:movie_data_rows)
    {
      
        
      
        size<-nrow(topdata)
      topdata<-topdata[order(topdata$cosval),]
      if (b!=a){
      if(size<5)
      {
        movie_name_temp<-movie_data[b,12]
        movie_cosine_temp<-calc_cosine_similarity(a,b)
        newRow <- data.frame(cosval=movie_cosine_temp,movie_name=movie_name_temp)
        #new_row<-c(movie_name_temp:movie_cosine_temp)
        if (!is.na(movie_cosine_temp))
        {
          topdata<-rbind(topdata,newRow)  
        }
       
      }
      else{
        movie_name_temp<-movie_data[b,12]
        movie_cosine_temp<-calc_cosine_similarity(a,b)
        if (!is.na(movie_cosine_temp))
        {
          minval<-topdata[1,1]
          #print(movie_cosine_temp)
          if (movie_cosine_temp>minval)
          {
            newRow <- data.frame(cosval=movie_cosine_temp,movie_name=movie_name_temp)
            row_to_keep = c(FALSE,TRUE, TRUE, TRUE, TRUE)
            topdata <- topdata[row_to_keep,]
            topdata<-rbind(topdata,newRow)
          }
          
          }
       
      }
      
      }
    }
  

return(topdata)
  }
search_by_movie<-function()
{
  movie <- readline(prompt="Please Enter Movie Name: ")
  print(movie)
  movie_index<-grep(movie, movie_data$movie_title)
  print(movie_index)
  consine_func(movie_index)  
}
search_by_movie()