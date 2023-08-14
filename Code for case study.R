library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame
jan_23<- read_csv("202301-divvy-tripdata.csv")  
dec_22<- read_csv("202212-divvy-tripdata.csv") 
nov_22<- read_csv("202211-divvy-tripdata.csv")
oct_22<- read_csv("202210-divvy-tripdata.csv")
sep_22<- read_csv("202209-divvy-publictripdata.csv")
aug_22<- read_csv("202208-divvy-tripdata.csv")
jul_22<- read_csv("202207-divvy-tripdata.csv")
jun_22<- read_csv("202206-divvy-tripdata.csv")
may_22<- read_csv("202205-divvy-tripdata.csv")
apr_22<- read_csv("202204-divvy-tripdata.csv")
mar_22<- read_csv("202203-divvy-tripdata.csv")
feb_22<- read_csv("202202-divvy-tripdata.csv")

#merge all of the data frames into one year view
cyclistic <- rbind (jan_23,dec_22,nov_22, oct_22, sep_22, aug_22, jul_22, jun_22, may_22,apr_22,mar_22,feb_22)

#remove individual month data frames to clear up space in the environment 
remove(jan_23,dec_22,nov_22, oct_22, sep_22, aug_22, jul_22, jun_22, may_22,apr_22,mar_22,feb_22)

##create new data frame to contain new columns
cyclistic_date <- cyclistic

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
cyclistic_date$ride_length <- difftime(cyclistic$ended_at, cyclistic$started_at, units = "mins")

cyclistic_date$day_of_week <- weekdays(cyclistic$started_at)#create column for day
cyclistic_date$month <- format(as.Date(cyclistic_date$started_at), "%m")#create column for month
cyclistic_date$day <- format(as.Date(cyclistic_date$started_at), "%d") #create column for day
cyclistic_date$year <- format(as.Date(cyclistic_date$started_at), "%Y") #create column for year
cyclistic_date$time <- as_hms((cyclistic$started_at))#create column for time
cyclistic_date$hour <- hour(cyclistic_date$time) #create new column for hour
cyclistic_date$date <- as.Date(cyclistic_date$started_at)#default format is yyyy-mm-dd, use start date






#clean the data
cyclistic_date <- na.omit(cyclistic_date) #remove rows with NA values
cyclistic_date <- distinct(cyclistic_date) #remove duplicate rows 
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_date <- cyclistic_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 
cyclistic_date$day_of_week <- ordered(cyclistic_date$day_of_week, levels=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))



#total number of rides
nrow(cyclistic_date)

#-----------------MEMBER TYPE
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(member_casual) %>% 
  ggplot(cyclistic_date,mapping= aes(x =member_casual, y = n,fill= member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n), vjust = -0.2)+
  labs(title="No.of rides by Subscription type", y="No. of rides",x= "Subscription Type")



#total rides by member type 
cyclistic_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type) %>% 
  ggplot(cyclistic_date,mapping= aes(x =rideable_type, y = n,fill= member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n), vjust = -0.2)+
  labs(title="No.of rides by type of bike", y="No. of rides",x= "Type of bike",fill="Subscription Type")

#total rides 
cyclistic_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)  %>% 
  ggplot(cyclistic_date,mapping= aes(x =rideable_type,y= n, fill= rideable_type)) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n),vjust = -0.2)+
  labs(title="No.of rides by type of bike", y="No. of rides",x= "Type of bike")

#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print(n = 48) %>% #lets you view the entire tibble
  ggplot(cyclistic_date,mapping= aes(x =hour, y = n,fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n), vjust = -0.2, size= 3 )+
  labs(title="No.of rides By hour of Day", y="No. of rides",x= "Hour of day",fill="Subscription Type") 
  #code with facet wrap{ggplot(cyclistic_date,mapping= aes(x =hour, y = n,fill=member_casual )) +
  #geom_col(position = "dodge")+
  #geom_text(aes(label = n), vjust = -0.2, size= 2 )+
  ##labs(title="No.of rides By hour of Day", y="No. of rides",x= "Hour of day",fill="Subscription Type")+
  ##facet_wrap(~ member_casual)}



#total rides
cyclistic_date %>%
  count(hour) %>% 
  print(n = 24) %>% #lets you view the entire tibble
  ggplot(cyclistic_date,mapping= aes(x =hour, y = n )) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = n), vjust = -0.2, size= 3 )+
  labs(title="No.of rides By hour of Day", y="No. of rides",x= "Hour of day") 

#----------------DAY OF THE WEEK-------------
#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)%>% 
  ggplot(cyclistic_date,mapping= aes(x =day_of_week, y = n,fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="No.of rides By Day of Week", y="No. of rides",x= "Day of Week",fill="Subscription Type") 

#total rides 
cyclistic_date %>%
  count(day_of_week) %>%
  ggplot(cyclistic_date,mapping= aes(x =day_of_week, y = n )) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = n), vjust = -0.2, size= 3 )+
  labs(title="No.of rides By Day of Week", y="No. of rides",x= "Hour of day")  

#----------------DAY OF THE MONTH-----------------

#total rides by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62)%>%
  ggplot(cyclistic_date,mapping= aes(x = day, y = n, fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n), vjust = -0.2, size= 2,position = position_dodge(.9) )+
  labs(title="No.of rides By Day of Month", y="No. of rides",x= "Day of Month",fill="Subscription type")+
  #facet_wrap(~member_casual)
#lets you view the entire tibble

#total rides
cyclistic_date %>%
  count(day) %>% 
  print(n = 31) %>%
  ggplot(cyclistic_date,mapping= aes(x =day,y = n)) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = n), vjust = -0.2, size= 2.5 )+
  labs(title="No.of rides By Day of Month", y="No. of rides",x= "Day of Month") #lets you view the entire tibble
#----------MONTH-----------

#total rides by member type 
cyclistic_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) %>%
  ggplot(cyclistic_date,mapping= aes(x = month,y = n, fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = n), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="No.of rides By Month", y="No. of rides",x= "Month",fill="Subscription type")
  #facet_wrap(~member_casual)

#total rides
cyclistic_date %>%
  count(month) %>%
  ggplot(cyclistic_date,mapping= aes(x =month,y = n)) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = n), vjust = -0.2, size= 2.5 )+
  labs(title="No.of rides By Month", y="No. of rides",x= "Month") 

#-----AVG RIDE LENGTH----

#average of ride_length
cyclistic_avgRide <- mean(cyclistic_date$ride_length)
print(cyclistic_avgRide)

# BY MEMBER TYPE--------------------

#average ride_length
cyclistic_date %>% group_by( member_casual) %>% 
  summarise(avg_ride=mean(ride_length)) %>%
  ggplot(cyclistic_date,mapping= aes(x = member_casual ,y = round(avg_ride,digits=2), fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Subscription Type", y="Avg. ride Length in mins",x= "Subscription Type",fill="Subscription type")

# Average ride lenth by rideable type-----

#total rides by member type 
cyclistic_date %>% group_by(member_casual, rideable_type) %>% 
  summarise(avg_ride=mean(ride_length)) %>%
  ggplot(cyclistic_date,mapping= aes(x = rideable_type ,y = round(avg_ride,digits=2), fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Rideable Type", y="Avg. ride Length in mins",x= "Rideable Type",fill="Subscription type")


#average ride_length
cyclistic_date %>% group_by(rideable_type) %>% 
  summarise(avg_ride=mean(ride_length))%>%
 ggplot(cyclistic_date,mapping= aes(x = rideable_type ,y = round(avg_ride,digits=2) )) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Rideable Type", y="Avg. ride Length in mins",x= "Rideable Type ")


#-----------------------HOUR-------------------------

#average ride_length by member type
cyclistic_date %>% group_by(hour, member_casual) %>% 
  summarise(avg_ride=mean(ride_length)) %>% 
  print(n=48)  %>%
  ggplot(cyclistic_date,mapping= aes(x = hour ,y = round(avg_ride,digits=2), fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Hour", y="Avg. ride Length in mins",x= "Hour",fill="Subscription type")
#lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(hour) %>% 
  summarise(avg_ride=mean(ride_length)) %>% 
  print(n=24) %>%
  ggplot(cyclistic_date,mapping= aes(x = hour ,y = round(avg_ride,digits=2) )) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Hour", y="Avg. ride Length in mins",x= "Hour ")
#lets you view entire tibble

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
cyclistic_date %>% group_by(member_casual, day_of_week) %>% 
  summarise(avg_ride=mean(ride_length))  %>%
  ggplot(cyclistic_date,mapping= aes(x = day_of_week ,y = round(avg_ride,digits=2), fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Weekday", y="Avg. ride Length in mins",x= "Day of Week",fill="Subscription type")


#average ride_length 
cyclistic_date %>% group_by(day_of_week) %>% 
  summarise(avg_ride=mean(ride_length)) %>%
  ggplot(cyclistic_date,mapping= aes(x = day_of_week ,y = round(avg_ride,digits=2) )) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Weekday", y="Avg. ride Length in mins",x= "Day of Week")


#----- Month


#average ride_length by member type
cyclistic_date %>% group_by(month, member_casual) %>% 
  summarise(avg_ride=mean(ride_length)) %>% 
  print(n=24) %>%
  ggplot(cyclistic_date,mapping= aes(x = month,y = round(avg_ride,digits=2), fill=member_casual )) +
  geom_col(position = "dodge")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Month", y="Avg. ride Length in mins",x= "Month",fill="Subscription type")
#lets you view entire tibble

#average ride_length
cyclistic_date %>% group_by(month) %>% 
  summarise(avg_ride=mean(ride_length))  %>%
  ggplot(cyclistic_date,mapping= aes(x = month ,y = round(avg_ride,digits=2) )) +
  geom_col(position = "dodge", fill="purple")+
  geom_text(aes(label = round(avg_ride,digits=2)), vjust = -0.2, size= 3,position = position_dodge(.9) )+
  labs(title="Average Ride length by Month", y="Avg. ride Length in mins",x= "Month")

               