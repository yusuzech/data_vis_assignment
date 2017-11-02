################### Basic Importing, loading and pre-processing code ##################
#Load packages
library(ggplot2)
library(dplyr)
#Import data
movie <- read.csv("movie.csv")
#Explore the data
head(movie,5)
tail(movie,5)
summary(movie)
str(movie)
#Convert the factors to numeric
movie$Adjusted_Gross <- as.numeric(gsub(",","",as.character(movie$Adjusted_Gross)))
movie$Gross_rev <- as.numeric(gsub(",","",as.character(movie$Gross_rev)))
movie$Overseas_rev <- as.numeric(gsub(",","",as.character(movie$Overseas_rev)))
movie$Profit <- as.numeric(gsub(",","",as.character(movie$Profit)))
str(movie)

#2.	Shall we make a short movie or a long movie?

###########################################################################
################## Runtime Analysis starts from here ######################
###########################################################################

#Plot the relationship between Runtime & other variables
ggplot(data=movie,aes(x=Runtime_min,y=Adjusted_Gross))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("Run Time (in minutes)")+
  ylab("Adjusted Gross Revenue ($MM)")+
  ggtitle("Relationship between Run Time & Revenue")

ggplot(data=movie,aes(x=Runtime_min,y=Profit))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("Run Time (in minutes)")+
  ylab("Profit ($MM)")+
  ggtitle("Relationship between Run Time & Profit")

ggplot(data=movie,aes(x=Runtime_min,y=Budget))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("Run Time (in minutes)")+
  ylab("Budget ($MM)")+
  ggtitle("Relationship between Run Time & Budget")

ggplot(data=movie,aes(x=Runtime_min,y=IMDb_Rating))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("Run Time (in minutes)")+
  ylab("IMDb_Rating")+
  ggtitle("Relationship between Run Time & IMDb_Rating")

#Find the correlation coefficient
cor(movie$Adjusted_Gross,movie$Runtime_min) 
cor(movie$Profit,movie$Runtime_min) 
cor(movie$Budget,movie$Runtime_min) 
cor(movie$IMDb_Rating,movie$Runtime_min) 

#Check the distribution of movies on runtime
ggplot(movie, aes(Runtime_min)) +
  geom_histogram(bins=15,fill="rosybrown1", color="black") +
  labs(title="Distribution of Movie Run times", x="Movie Run Times", y="# of Movies")+
  geom_hline(yintercept=0, size=0.4, color="black")

mean(movie$Runtime_min)

#Create the buckets based on runtime
movie$runtime_cat <- ifelse(movie$Runtime_min<100,"Short Movies",
                            ifelse(movie$Runtime_min>=100 & movie$Runtime_min<140,
                                   "Feature Films","Long Movies"))
movie$runtime_cat <- factor(as.character(movie$runtime_cat), 
                            levels=c("Short Movies","Feature Films","Long Movies"))

#Filter to see the top Genres by Revenue
top_genre <- movie %>% group_by(Genre) %>% summarise(Revenue = (mean(Adjusted_Gross))) %>% 
  arrange(desc(Revenue))
vec_top_genre <- top_genre[1:10,] %>% pull(Genre)

movie_top10 <- movie[movie$Genre %in% vec_top_genre,]

#Calculate the mean metrics by Genre and length of the movie
genre_rev <- movie_top10 %>% group_by(Genre, runtime_cat) %>% 
  summarise(Revenue = mean(Adjusted_Gross), Avg_budget = mean(Budget),
            Avg_Profit = mean(Profit)) 

#Plot the tile view to check overall profit for each runtime bucket
ggplot(data=genre_rev, aes(runtime_cat, Genre))+
  geom_tile(aes(fill=Avg_Profit), color="white")+
  scale_fill_gradient("Average Profit", low="white", high="blue")+
  ylab("Genre")+
  xlab("Movie Type")

#Plot a bar graph by Genre and runtime bucket
ggplot(data=genre_rev,aes(y=Avg_budget,x=reorder(Genre,-Avg_budget)))+
  geom_bar(stat = "identity", fill = 'red1', width = 0.2)+
  ylab("Budget ($Bn)")+
  xlab("Genre")+
  ggtitle("Budget by Genres")+
  coord_flip()+
  facet_grid(runtime_cat~.,)
ggplot(data=genre_rev,aes(y=Revenue,x=reorder(Genre,-Avg_budget)))+
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


###########################################################################
####################### Runtime Analysis Ends here ########################
###########################################################################


######################### Additional EDA - not reqd #######################

#1.	Does higher rating movie also have higher adjusted gross revenue? 
#What’s the relationship between rating and adjusted gross revenues? 
#Add a trend line to your visualization to tell a better story

#Create a scatterplot to show the relationship between Rating and Adjusted revenue
ggplot(data=movie,aes(x=IMDb_Rating,y=Adjusted_Gross))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("IMDB Rating")+
  ylab("Adjusted Gross Revenue ($MM)")+
  ggtitle("Relationship between IMDB Rating & Revenue")
ggplot(data=movie,aes(x=MovieLens_Rating,y=Adjusted_Gross))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("MovieLens Rating")+
  ylab("Adjusted Gross Revenue ($MM)")+
  ggtitle("Relationship between MovieLens Rating & Revenue")

#Calculating the correlation coefficient - 0.272, 0.226
cor(movie$Adjusted_Gross,movie$IMDb_Rating) 
cor(movie$Adjusted_Gross,movie$MovieLens_Rating) 

ggplot(data=movie,aes(x=Runtime_min,y=Adjusted_Gross))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("Run Time (in minutes)")+
  ylab("Adjusted Gross Revenue ($MM)")+
  ggtitle("Relationship between Run Time & Revenue")+
   facet_grid(Genre~Day_of_Week)

#3. If a movie does well in US, does it also usually do well overseas?
ggplot(data=movie,aes(x=US_rev,y=Overseas_rev))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  xlab("US Revenue ($MM)")+
  ylab("Overseas Revenue ($MM)")+
  ggtitle("Relationship between US & Overseas Revenue")

cor(movie$US_rev,movie$Overseas_rev) 

#Summarise and arrange the revenue for the Genres
genre_rev <- movie %>% group_by(Genre) %>% summarise(Revenue = (mean(Adjusted_Gross))) %>% 
  arrange(desc(Revenue))
genre_rev

#Plot the top 5 Genres
ggplot(data=genre_rev[1:5,],aes(x=reorder(Genre,-Revenue),Revenue))+
  geom_bar(stat = "identity", fill = 'Blue', width = 0.4)+
  xlab("Genre")+
  ylab("Adjusted Gross Revenue ($Bn)")+
  ggtitle("Top 5 Genres by Revenue")

#Filter for the required Genres
subset_movie <- movie[movie$Genre %in% c("action","animation","adventure","comedy",'drama'),]

#Plot using the filtered data
ggplot(data = subset_movie, aes(x=Adjusted_Gross))+
  geom_histogram(binwidth=20,aes(fill=Genre),color="Black")+
  xlim(c(200,1000))+
  ggtitle("Movies' Gross Revenue Distribution by Genre")+
  xlab("Adjusted Gross Revenue in millions")+
  ylab("count")+
  facet_grid(Genre~.,)
