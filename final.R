
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
                              

#Top 5 channels across all the five categories 

# top 5 channel across comedy 

uschannel <- usnew

uscomedy <- uschannel[uschannel$category == "Comedy",]

uscomedy <- filter(uscomedy) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)
x <- ggplot(data = uscomedy, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Comedy Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))
x

x + labs( x = " Total number of views" , y = "Channels")

# Finding top 5 channel for the Entertainment

usenter <- uschannel[uschannel$category == "Entertainment",]

usenter <- filter(usenter) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usenter, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Entertainment Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))

x

x + labs( x = " Total number of views" , y = "Channels")



# Finding the top 5 music channel 

usmusic <- uschannel[uschannel$category == "Music",]

usmusic <- filter(usmusic) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usmusic, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Music Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))

x
x + labs( x = " Total number of views" , y = "Channels")

#How to and Style top5 channel

usstyle <- uschannel[uschannel$category == "How to and Style",]
usstyle <- filter(usstyle) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usstyle, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 How to and Style Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))
x

x + labs( x = " Total number of views" , y = "Channels")

#People and Blogs


usblog <- uschannel[uschannel$category == "People and Blogs",]

usblog <- filter(usblog) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usblog, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 People and Blogs Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))

x

x + labs( x = " Total number of views" , y = "Channels")


data_us$hour <- substr(data_us$publish_time,12,13)
data_us$hour <- as.numeric(data_us$hour)

q1 <- tapply(data_us$views,list(data_us$hour),FUN = sum)
q1 <- as.data.frame(q1)
q1$hours <- rownames(q1)
rownames(q1) <- NULL
total_view <- q1$q1
hours <- q1$hours
hours <- as.numeric(hours)
df2 <- q1
df2$hours <- as.numeric(df2$hours)

#First plot
plot(hours, total_view, main = "Total number of views in terms of different hours",
     xlab = "Hours", ylab = "Total number of views",
     pch = 19,col = "darkgreen",las=1)
axis(side=1, at=0:23, labels=hours[0:24],cex.axis=1)
lines(lowess(hours, total_view), col = "red")

#Second plot
data_us$hour <- substr(data_us$publish_time,12,13)
data_us$hour <- as.numeric(data_us$hour)

data_us$day_of_week <- weekdays(as.Date(data_us$publish_time))
day_week <- tapply(data_us$likes,list(data_us$day_of_week),FUN = sum)
day_week <- as.data.frame(day_week)
day_week <- cbind(the_day = rownames(day_week), day_week)
rownames(day_week) <- 1:nrow(day_week)
rownames(day_week) <- NULL
names(day_week) <- c("day_of_week", "totalViews")
day_week$share = day_week$totalViews/sum(day_week$totalViews)

plot2 = ggplot(day_week, aes(x="", y=share, fill=day_of_week)) + geom_bar(stat="identity", width=1)
plot2 = plot2 + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(share*100), "%")), position = position_stack(vjust = 0.5))
plot2 = plot2 + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999", "#FF4500")) 
plot2 = plot2 + labs(x = NULL, y = NULL, fill = NULL, title = "Number of views in terms of different days")
plot2 = plot2 + theme_classic() + theme(axis.line = element_blank(),
                                        axis.text = element_blank(),
                                        axis.ticks = element_blank(),
                                        plot.title = element_text(color = "#666666"))

plot2