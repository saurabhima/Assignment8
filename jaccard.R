movie_data = read.csv("movie_metadata.csv",head=TRUE,sep=",")
nums <- sapply(movie_data, is.numeric)
movie_data_nums<- movie_data[,nums]
movie_data_rows<-nrow(movie_data_nums)
movie_data_coln<-ncol(movie_data_nums)
director_totals_df<- data.frame(movieval= numeric(0), director_name = character(0))
calc_director_total_movie<-function(director)
{
  movie_index<-grep(director, movie_data$director_name)
  return(length(movie_index))
}
calc_actor_total_movie<-function(actor)
{
  movie_index1<-grep(actor,movie_data$actor_1_name)
  movie_index2<-grep(actor,movie_data$actor_2_name)
  movie_index3<-grep(actor,movie_data$actor_3_name)
  total_actor_movie<-length(movie_index1)+length(movie_index2)+length(movie_index3)
  return(total_actor_movie)
}

populate_director_df<-function()
{
  director_totals<- data.frame(movieval= numeric(0), director_name = character(0))
  for (b in 1:movie_data_rows)
  {
  director_name<-movie_data[b,2]
  director_index<-grep(director_name,director_totals$director_name)
  if (length(director_index)==0)
  {
    total_movie<-calc_director_total_movie(director_name)
    print(director_name)
    print(total_movie)
    newRow <- data.frame(movieval=total_movie,director_name=director_name)
    director_totals<-rbind(director_totals,newRow)
  }
  }
return(director_totals)
  }
#out<-calc_actor_total_movie('Bruce Willis')
#print(out)
director_totals_df<-populate_director_df()
