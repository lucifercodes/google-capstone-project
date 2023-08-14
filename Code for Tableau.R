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



#create a column for the month using the full month name
cyclistic_date <-cyclistic_date %>% mutate(month = 
                                             case_when(month == "01" ~ "January",
                                                       month == "02" ~ "February",
                                                       month == "03" ~ "March",
                                                       month == "04" ~ "April",
                                                       month == "05" ~ "May",
                                                       month == "06" ~ "June",
                                                       month == "07" ~ "July",
                                                       month == "08" ~ "August",
                                                       month == "09" ~ "September",
                                                       month == "10" ~ "October",
                                                       month == "11" ~ "November",
                                                       month == "12" ~ "December"
                                             )
)





#clean the data
cyclistic_date <- na.omit(cyclistic_date) #remove rows with NA values
cyclistic_date <- distinct(cyclistic_date) #remove duplicate rows 
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <=0),] #remove where ride_length is 0 or negative
cyclistic_date <- cyclistic_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id, time, started_at, ended_at)) 
cyclistic_date$day_of_week <- ordered(cyclistic_date$day_of_week, levels=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

#download the new data as a .csv file
fwrite(cyclistic_date,"cyclistic_data.csv")
