
#Importing the dataset
videos <- read.csv("C:/Users/kaush/Desktop/USvideos.csv")
str(videos)

#loading packages
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggmap)
library(gcookbook)
library(plyr)
library(lubridate)

#looking for null values in the dataset
is.na(videos)
sum(is.na(videos))
#we can now conclude that there are zero null values in the dataset

#Changing column name from category_id to category
colnames(videos)[colnames(videos) == 'category_id'] <- 'category'
str(videos)

#renaming the category id to category names
videos$category[videos$category == "1"] <- "Film & Animation"
videos$category[videos$category == "2"] <- "Cars & Vehicles"
videos$category[videos$category == "10"] <- "Music"
videos$category[videos$category == "15"] <- "Pets & Animals"
videos$category[videos$category == "17"] <- "Sports"
videos$category[videos$category == "19"] <- "Travel & Events"
videos$category[videos$category == "20"] <- "Gaming"
videos$category[videos$category == "22"] <- "People & Blogs"
videos$category[videos$category == "23"] <- "Comedy"
videos$category[videos$category == "24"] <- "Entertainment"
videos$category[videos$category == "25"] <- "News & Politics"
videos$category[videos$category == "26"] <- "Howto & Style"
videos$category[videos$category == "27"] <- "Education"
videos$category[videos$category == "28"] <- "Science & Technology"
videos$category[videos$category == "29"] <- "Nonprofits & Activism"
videos$category[videos$category == "43"] <- "Shows"


videos$trending_date <- ydm(videos$trending_date)

videos$publish_time <- ymd(substr(videos$publish_time,start = 1,stop = 10))

videos$dif_days <- videos$trending_date-videos$publish_time

videos_dif <- subset(videos, videos[,17] < 30)

RColorBrewer::brewer.pal.info

ggplot(data = videos_dif, aes(x = as.factor(dif_days), fill = as.factor(dif_days))) + 
  geom_bar(color = "black") + theme(legend.position = "none") + xlab("No. of Days") + ylab("No. of Videos") +
  ggtitle("Time it takes for Videos to Trend") + ylim(c(0,5000)) + scale_color_grey()


diff_days <- data.frame(videos$category, videos$dif_days)
diff_days <- aggregate(diff_days$videos.dif_days, list(diff_days$videos.category), FUN = mean)
diff_days$x <- as.integer(diff_days$x)
str(diff_days)

ggplot(data = diff_days, aes(x = reorder(Group.1, -x), y = x, fill = as.factor(x))) + 
  geom_bar(stat = "identity", color = "black") + ylim(c(0,50)) + 
  theme(legend.position = "none") + coord_flip() + 
  ggtitle("Time Taken by Videos of Each Category to Trend") + xlab("Categories") + ylab("No. of Days")


ggplot(data = videos, aes(x = category, y = views)) + geom_boxplot(aes( fill = category)) + 
  ylim(c(0, 4000000)) + theme(legend.position = "none") + theme_minimal()


ggplot(data = videos, aes(x = category, y = likes)) + geom_boxplot(aes( fill = category)) + 
  ylim(c(0, 150000)) + theme(legend.position = "none") + theme_minimal()
                              
