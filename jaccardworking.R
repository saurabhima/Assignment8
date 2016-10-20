library(plyr)
movie_data = read.csv("movie_metadata.csv",head=TRUE,sep=",")
nums <- sapply(movie_data, is.numeric)
movie_data_nums<- movie_data[,nums]
movie_data_rows<-nrow(movie_data_nums)
movie_data_coln<-ncol(movie_data_nums)
director_totals_df<- data.frame(movieval= numeric(0), director_name = character(0))
actor_totals_df<- data.frame(movieval= numeric(0), actor_name = character(0))
insersection_df<-data.frame(movieval= numeric(0),director_name = character(0),actor_name = character(0))
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
    director_name_temp<-toString(director_vect[b])
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

calculate_director_movies<-function()
{
  
  direct_df<- data.frame(director_name = character(0),movie_index=vector())
  
  director_vect<-director_totals_df$director_name
  
  dv_len=length(director_vect)

  
  for (dvcount in 1:dv_len)
  {
    direct_name<-toString(director_vect[dvcount])
    director_movie_vect<-grep(director_vect[dvcount],movie_data$director_name)
    newRow1 <- data.frame(director_name=direct_name,movie_index=director_movie_vect)
    direct_df<-rbind(direct_df,newRow1)
    

  }
 
return(direct_df)
}


calculate_actor_movies<-function()
{
  
  
  actor_df<- data.frame(actor_name = character(0),movie_index=vector())
  
  
  actor_vect<-actor_totals_df$actor_name
  
  av_len=length(actor_vect)
  
 
  for (avcount in 1:av_len)
  {
    actor_name<-toString(actor_vect[avcount])
    actor_movie_vect1<-grep(actor_vect[avcount],movie_data$actor_1_name)
    actor_movie_vect2<-grep(actor_vect[avcount],movie_data$actor_2_name)
    actor_movie_vect3<-grep(actor_vect[avcount],movie_data$actor_3_name)
    actor_movie_vect=c()
    for (v in actor_movie_vect1)
      actor_movie_vect <- c(actor_movie_vect, v)
    for (v in actor_movie_vect2)
      actor_movie_vect <- c(actor_movie_vect, v)
    for (v in actor_movie_vect3)
      actor_movie_vect <- c(actor_movie_vect, v)
    actor_movie_vect <- actor_movie_vect[!is.na(actor_movie_vect)]
    actor_movie_vect<-unique(actor_movie_vect)
    newRow1 <- data.frame(actor_name=actor_name,movie_index=actor_movie_vect)
    actor_df<-rbind(actor_df,newRow1)
    
    
  }
  
  
  
  return(actor_df)
}

calculate_director_movies<-function()
{
  director_vect<-director_totals_df$director_name
  dv_len=length(director_vect)
  actor_vect<-actor_totals_df$actor_name
  av_len=length(actor_vect)
  
  
}

calculate_intersection<-function()
{
  pair_df<- data.frame(director_name = character(0),actor_name = character(0)) 
  unique_pair_df<- data.frame(director_name = character(0),actor_name = character(0)) 
  output_df<-data.frame(director_name = character(0),actor_name = character(0),movieval=numeric(0)) 
  pair_df1<- data.frame(director_name = character(0),actor_name = character(0)) 
  pair_df1<-movie_data[,c("director_name","actor_1_name")]
  colnames(pair_df1) <- c("director_name", "actor_name")
  pair_df2<- data.frame(director_name = character(0),actor_name = character(0)) 
  pair_df2<-movie_data[,c("director_name","actor_2_name")]
  colnames(pair_df2) <- c("director_name", "actor_name")
  pair_df3<- data.frame(director_name = character(0),actor_name = character(0)) 
  pair_df3<-movie_data[,c("director_name","actor_3_name")]
  colnames(pair_df3) <- c("director_name", "actor_name")
  #pair_df <- merge(pair_df1,pair_df2,by="ac)
  
  pair_df<-rbind(pair_df1, pair_df2)
  pair_df<-rbind(pair_df, pair_df3)
  
  check_vector<-duplicated(pair_df)
  unique_pair_df<-pair_df[!duplicated(pair_df), ]
  pair_df<-subset(pair_df,director_name!="")
  pair_df<-subset(pair_df,actor_name!="")
  freq<-ddply(pair_df,.(director_name,actor_name),nrow)
  
  colnames(freq) <- c("director_name", "actor_name","freq")
  freq <- merge(freq,director_totals_df,by="director_name")
  freq <- merge(freq,actor_totals_df,by="actor_name")
  colnames(freq) <- c("actor_name","director_name", "pair_freq","director_freq","actor_freq")
  freq$jaccard=c()
  freq$jacard<-freq$pair_freq/abs(freq$director_freq+freq$actor_freq-freq$pair_freq)
  freq<-freq[order(-freq$jacard),]
  return(freq)
  
}
#out<-calc_actor_total_movie('Bruce Willis')
#print(out)
director_totals_df<-populate_director_df()
actor_totals_df<-populate_actor_df()
#populate_director_df()
#populate_actor_df()
#director_list<-calculate_director_movies()
#actor_list<-calculate_actor_movies()
intersect<-calculate_intersection()
print(intersect[1:5,])
