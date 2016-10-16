movie_data = read.csv("movie_metadata.csv",head=TRUE,sep=",")
nums <- sapply(movie_data, is.numeric)
movie_data_nums<- movie_data[,nums]
movie_data_rows<-nrow(movie_data_nums)
movie_data_coln<-ncol(movie_data_nums)

calc_euclidean_similarity<-function(row1,row2)
{
  diffxy<-0
  diffxysquare<-0
  vect1=as.numeric(movie_data_nums[row1,])
  vect2=as.numeric(movie_data_nums[row2,])
  for(x in 1:movie_data_coln){
    if(!is.na(vect1[x])&&!is.na(vect2[x]))
    {
    diffxy<-(vect1[x]-vect2[x])
    diffxysquare<-diffxysquare+(diffxy*diffxy)
    }
  }
  
  euclideansimilarity<-sqrt(diffxysquare)
return(euclideansimilarity)
  }

euclidean_func<-function(value_a)
{
  topdata <- data.frame(equidileanval= numeric(0), movie_name = character(0))
  a<-value_a
    for (b in 1:movie_data_rows)
    {

        size<-nrow(topdata)
      topdata<-topdata[order(topdata$equidileanval),]
      if (b!=a){
      if(size<5)
      {
        movie_name_temp<-movie_data[b,12]
        movie_euclidean_temp<-calc_euclidean_similarity(a,b)
        newRow <- data.frame(equidileanval=movie_euclidean_temp,movie_name=movie_name_temp)
        if (!is.na(movie_euclidean_temp))
        {
          topdata<-rbind(topdata,newRow)
        }

      }
      else{
        movie_name_temp<-movie_data[b,12]
        movie_euclidean_temp<-calc_euclidean_similarity(a,b)
        if (!is.na(movie_euclidean_temp))
        {
          minval<-topdata[5,1]
          #print(movie_cosine_temp)
          if (movie_euclidean_temp<minval)
          {
            newRow <- data.frame(equidileanval=movie_euclidean_temp,movie_name=movie_name_temp)
            row_to_keep = c(TRUE, TRUE, TRUE, TRUE,FALSE)
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
  euclidean_func(movie_index)
}
search_by_movie()