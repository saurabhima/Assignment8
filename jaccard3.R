movie_data = read.csv("movie_metadata.csv",head=TRUE,sep=",")
nums <- sapply(movie_data, is.numeric)
movie_data_nums<- movie_data[,nums]
movie_data_rows<-nrow(movie_data_nums)
movie_data_coln<-ncol(movie_data_nums)
#director_totals_df<- data.frame(movieval= numeric(0), director_name = character(0))
#actor_totals_df<- data.frame(movieval= numeric(0), actor_name = character(0))
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
  director_vect<-movie_data[,2]
  director_vect <- director_vect[!is.na(director_vect)]
  director_vect<-unique(director_vect)
 
  director_vect_len=length(director_vect)
  
  for (b in 1:director_vect_len)
  {
    director_name_temp<-director_vect[b]
    total_movie<-calc_director_total_movie(director_name_temp)
    # print(director_name)
    # print(total_movie)
    if(!is.null(director_name_temp)&&!is.na(director_name_temp)&&!is.null(total_movie)&&!is.na(total_movie))
    {
    newRow <- data.frame(movieval=total_movie,director_name=director_name_temp)
    
    director_totals<-rbind(director_totals,newRow)
    
    }

  }
  return(director_totals)

  
}
populate_actor_df<-function()
{
  actor_totals<- data.frame(movieval= numeric(0), actor_name = character(0))
  actor_vect1<-movie_data[,11]
  actor_vect2<-movie_data[,7]
  actor_vect3<-movie_data[,15]
  actor_vect=c()
  
  for (v in actor_vect1)
    actor_vect <- c(actor_vect, v)
  for (v in actor_vect2)
    actor_vect <- c(actor_vect, v)
  for (v in actor_vect3)
    actor_vect <- c(actor_vect, v)
  actor_vect <- actor_vect[!is.na(actor_vect)]
  actor_vect<-unique(actor_vect)
  actor_vect_len<-length(actor_vect)
  for (b in 1:actor_vect_len)
  {
    actor_name<-actor_vect[b]
    actor_index<-grep(actor_name,actor_totals$actor_name)
    if (length(actor_index)==0)
    {
      total_movie<-calc_actor_total_movie(actor_name)
      
      newRow <- data.frame(movieval=total_movie,actor_name=actor_name)
      actor_totals<-rbind(actor_totals,newRow)
    }
    

  }
  return(actor_totals)
}
#out<-calc_actor_total_movie('Bruce Willis')
#print(out)
#director_totals_df<-populate_director_df()
#actor_totals_df<-populate_actor_df()
#populate_director_df()
#populate_actor_df()