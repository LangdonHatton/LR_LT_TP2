#Group Project 2
## Langdon Hatton & Leighann Robinson
### DS160 SP23
#install.packages("tidyverse") 
#install.packages('caTools')

library(caTools)
library(tidyverse)
dataset=read.csv("titles.csv")

#Explore the dataset
view(dataset)
glimpse(dataset)
length(dataset)
names(dataset)
summary(dataset)

#Fixing Missing Values
colSums(is.na(dataset))

#seasons
ggplot(data=dataset,
       aes(seasons))+
  geom_density()
seasons_mean=mean(dataset$seasons, na.rm=TRUE)
dataset$seasons=ifelse(is.na(dataset$seasons),
                       seasons_mean,
                       dataset$seasons)
colSums(is.na(dataset))

#imdb_votes
ggplot(data=dataset,
       aes(imdb_votes))+
  geom_histogram()
imdb_votes_mean=mean(dataset$imdb_votes, na.rm=TRUE)
dataset$imdb_votes=ifelse(is.na(dataset$imdb_votes),
                          imdb_votes_mean,
                          dataset$imdb_votes)
colSums(is.na(dataset))

#tmdb_popularity
ggplot(data=dataset,
       aes(tmdb_popularity))+
  geom_histogram()
tmdb_popularity_mean=mean(dataset$tmdb_popularity, na.rm=TRUE)
dataset$tmdb_popularity=ifelse(is.na(dataset$tmdb_popularity),
                               tmdb_popularity_mean,
                               dataset$tmdb_popularity)
colSums(is.na(dataset))

#tdmb_score
ggplot(data=dataset,
       aes(tmdb_score))+
  geom_histogram()
tdmb_score_mean=mean(dataset$tmdb_score, na.rm=TRUE)
dataset$tmdb_score=ifelse(is.na(dataset$tmdb_score),
                          tdmb_score_mean,
                          dataset$tmdb_score)
colSums(is.na(dataset))

#imdb_score
ggplot(data=dataset,
       aes(imdb_score))+
  geom_histogram()
imdb_score_mean=mean(dataset$imdb_score, na.rm=TRUE)
dataset$imdb_score=ifelse(is.na(dataset$imdb_score),
                          imdb_score_mean,
                          dataset$imdb_score)
colSums(is.na(dataset)) #There are no longer missing values 

#Graphs and Data Visualization


##Splitting the data into 2 sets, y variable is imdb_score
library(caTools)
set.seed(10)
split=sample.split(dataset$imdb_score, SplitRatio = 0.8) #80% training, 20% test
training_set=subset(dataset, split=TRUE)
test_set=subset(dataset, split=FALSE)


#proportion table for age certification and plot
table(dataset$age_certification)
freq_table = table(dataset$age_certification)
prop_table = prop.table(freq_table)
result_table = data.frame(age_certification = names(freq_table), 
                           frequency = as.vector(freq_table),
                           percentage = paste0(format(prop_table*100, digits=4), "%"))
colnames(result_table) = c("Age Verification", "Count", 'Percentage')
print(result_table)


#proportions for movie type 
table(dataset$type)
freq_table = table(dataset$type)
prop_table = prop.table(freq_table)
result_table = data.frame(type = names(freq_table), 
                          frequency = as.vector(freq_table),
                          percentage = paste0(format(prop_table*100, digits=4), "%"))
colnames(result_table) = c("Type", "Count", 'Percentage')
print(result_table)

type_counts = table(dataset$type)
pie(type_counts, labels = c("Movies", "Shows"), 
    main = "Movie vs. Show Proportions", col = c("red", "blue"))








