################### Basic Importing, loading and pre-processing code ##################
#Load packages
library(ggplot2)
library(dplyr)
#Import data
setwd('/Users/prakhar/Desktop/MSBA/431/hw1')
movie <- read.csv("movie.csv")

movie$Adjusted_Gross <- as.numeric(gsub(",","",as.character(movie$Adjusted_Gross)))
movie$Gross_rev <- as.numeric(gsub(",","",as.character(movie$Gross_rev)))
movie$Overseas_rev <- as.numeric(gsub(",","",as.character(movie$Overseas_rev)))
movie$Profit <- as.numeric(gsub(",","",as.character(movie$Profit)))
movie$US_Profit = movie$Profit*(movie$US_perc/100)
movie$Overseas_Profit = movie$Profit*(movie$Overseas_Perc/100)
str(movie)

#2.	Shall we make a short movie or a long movie?

#Create the buckets based on runtime
movie$runtime_cat <- ifelse(movie$Runtime_min<100,"Short Movies(< 100 min)",
                            ifelse(movie$Runtime_min>=100 & movie$Runtime_min<140,
                                   "Feature Films(100-140 min)","Long Movies(> 140 min)"))
movie$runtime_cat <- factor(as.character(movie$runtime_cat), 
                            levels=c("Short Movies(< 100 min)","Feature Films(100-140 min)","Long Movies(> 140 min)"))


#Calculate the mean metrics by Genre and length of the movie
genre_rev <- movie %>% group_by(Genre, runtime_cat) %>% 
  summarise(Revenue = mean(Adjusted_Gross), Avg_budget = mean(Budget),
            Avg_Profit = mean(Profit), Avg_US_Profit = mean(US_Profit), 
            Avg_Overseas_Profit = mean(Overseas_Profit)) 


#Plot a bar graph by Genre and runtime bucket
ggplot(data=genre_rev,aes(y=Avg_budget,x=reorder(Genre,-Avg_budget)))+
  geom_bar(stat = "identity", fill = 'red1', width = 0.2)+
  ylab("Budget ($Bn)")+
  xlab("Genre")+
  ggtitle("Budget by Genres")+
  coord_flip()+
  facet_grid(runtime_cat~.,)
ggplot(data=genre_rev,aes(y=Revenue,x=Genre))+
  geom_bar(stat = "identity", fill = 'green3', width = 0.2)+
  ylab("Adjusted Gross Revenue ($Bn)")+
  xlab("Genre")+
  ggtitle("Revenue by Genres")+
  coord_flip()+
  facet_grid(runtime_cat~.,)
ggplot(data=genre_rev,aes(y=Avg_Profit,x=reorder(Genre,-Avg_budget)))+
  geom_bar(stat = "identity", fill = 'dodgerblue1', width = 0.2)+
  ylab("Profit ($Bn)")+
  xlab("Genre")+
  ggtitle("Profit by Genres")+
  coord_flip()+
  facet_grid(runtime_cat~.,)
ggplot(data=genre_rev,aes(y=Avg_US_Profit,x=Genre))+
  geom_bar(stat = "identity", fill = 'orangered', width = 0.2)+
  ylab("Profit ($Bn)")+
  xlab("Genre")+
  ggtitle("US Profit by Genre")+
  coord_flip()+
  facet_grid(runtime_cat~.,)
ggplot(data=genre_rev,aes(y=Avg_Overseas_Profit,x=Genre))+
  geom_bar(stat = "identity", fill = 'forestgreen', width = 0.2)+
  ylab("Profit ($Bn)")+
  xlab("Genre")+
  ggtitle("Overseas Profit by Genre")+
  coord_flip()+
  facet_grid(runtime_cat~.,)
