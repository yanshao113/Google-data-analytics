## install ggmap, DT
## read data
jul202207_df<-read_csv("0original_data_202207_2023_06/202207-divvy-tripdata.csv")
aug202208_df<-read_csv("0original_data_202207_2023_06/202208-divvy-tripdata.csv")
sep202209_df<-read_csv("0original_data_202207_2023_06/202209-divvy-tripdata.csv")
oct202210_df<-read_csv("0original_data_202207_2023_06/202210-divvy-tripdata.csv")
nov202211_df<-read_csv("0original_data_202207_2023_06/202211-divvy-tripdata.csv")
dec202212_df<-read_csv("0original_data_202207_2023_06/202212-divvy-tripdata.csv")
jan202301_df<-read_csv("0original_data_202207_2023_06/202301-divvy-tripdata.csv")
feb202302_df<-read_csv("0original_data_202207_2023_06/202302-divvy-tripdata.csv")
mar202303_df<-read_csv("0original_data_202207_2023_06/202303-divvy-tripdata.csv")
apr202304_df<-read_csv("0original_data_202207_2023_06/202304-divvy-tripdata.csv")
may202305_df<-read_csv("0original_data_202207_2023_06/202305-divvy-tripdata.csv")
jun202306_df<-read_csv("0original_data_202207_2023_06/202306-divvy-tripdata.csv")

## merge data
cyclistic_year_df<-bind_rows(jul202207_df,aug202208_df,sep202209_df,oct202210_df,nov202211_df,dec202212_df,
                             jan202301_df,feb202302_df,mar202303_df,apr202304_df,may202305_df,jun202306_df)

## create a new data frame to process data
##select rideable_type, started_at, ended_at, start_station_name, end_station_name, member_casual
cyclistic_df <- cyclistic_year_df %>% 
  select(member_casual,rideable_type,started_at,ended_at,start_station_name,start_lat,start_lng,end_station_name,end_lat,end_lng)

## remove rows with null 
cyclistic_df <- cyclistic_df%>% drop_na() 


## separate date time, create new columns
## calculate ride length
cyclistic_df <- cyclistic_df %>% 
  mutate(started_date = as.Date(started_at),weekday = wday(started_date,label=TRUE), ride_length_mins = as.numeric(difftime(ended_at,started_at,units="mins"))) 

## filter out error data(negative value in duration and end_lat = 0.000)
cyclistic_df <- cyclistic_df %>%
  filter(ride_length_mins>0 & end_lat != 0)

##find outlier
## distribution plot
hist(log(cyclistic_df$ride_length_mins,base=10))

## depending on the plot, data mainly fall in 0-2 range
## 0 so < 1minute is too short;
## 2 but cut off the data that is more than 100 minute of ride length may lose meaningful data
## so filter ride_length_mins >3000;>2000;>1000 to determine the final range
## ride length more than 3000 -> only casual
cnt_morethan3000<-cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(ride_length_mins>3000)

## ride length more than 2000 -> only casual
cnt_morethan2000<-cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(ride_length_mins>2000)

## ride length more than 1000 -> contain both
cnt_morethan1000<-cyclistic_df %>% 
  group_by(member_casual) %>% 
  filter(ride_length_mins>1000)

## filter only member -> the maximum value < 1500
cntonly_member<-cyclistic_df %>% 
  filter(member_casual == "member")

##the data ride_length_mins >1500 only contain casual customers so couldn't help me to get know the difference between casual and member  
## so I got result cut off ride_length_mins <1, and ride_length_mins >1500
cyclistic_df <- cyclistic_df %>% 
  filter(ride_length_mins<1500 & ride_length_mins > 1)



##export the cleaned data 
write.csv(cyclistic_df,"/Users/shaoyan/Cyclistic bike-share analysis/1cleaned data/cyclistic_data_cleaned.csv")





##================analysis=================================
## Descriptive analysis on ride_length
#straight average (total ride length / rides)
mean(cyclistic_df$ride_length_mins)
#midpoint number in the ascending array of ride lengths
median(cyclistic_df$ride_length_mins) 
#longest ride
max(cyclistic_df$ride_length_mins) 
#shortest ride
min(cyclistic_df$ride_length_mins) 
# =
summary(cyclistic_df$ride_length_mins)

# Compare members and casual users
aggregate(cyclistic_df$ride_length_mins ~ cyclistic_df$member_casual, FUN = mean)
aggregate(cyclistic_df$ride_length_mins ~ cyclistic_df$member_casual, FUN = median)
aggregate(cyclistic_df$ride_length_mins ~ cyclistic_df$member_casual, FUN = max)
aggregate(cyclistic_df$ride_length_mins ~ cyclistic_df$member_casual, FUN = min)

# the average ride time by each day for members vs casual users
aggregate(cyclistic_df$ride_length_mins ~ cyclistic_df$member_casual + cyclistic_df$started_date, FUN = mean)

# the average ride time by the days of the week for members vs casual users
cyclistic_df$weekday <- ordered(cyclistic_df$weekday, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat","Sun"))
aggregate(cyclistic_df$ride_length_mins ~ cyclistic_df$member_casual + cyclistic_df$weekday, FUN = mean)

# rider trip data by member type and weekday
cyclistic_df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()						
            ,average_duration = mean(ride_length_mins)) %>% 	
  arrange(member_casual, weekday)	


#===========visualization======================================
#weekday vs member type
##the number of rides by member type
cyclistic_df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_mins)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

##average duration
cyclistic_df %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_mins)) %>% 
  arrange(member_casual, weekday)  %>%  
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#month vs member type
##the number of rides by member type
##library(scales)
cyclistic_df %>% mutate(start_month = month(as.Date(started_at))) %>% 
  group_by(member_casual, start_month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_mins)) %>% 
  arrange(member_casual, start_month)  %>% 
  ggplot(aes(x = start_month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))

# rideable_type vs member type
##the number of rides by member type
cyclistic_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_mins)) %>% 
  arrange(member_casual, rideable_type)  %>%  
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

##average duration
cyclistic_df %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_mins)) %>% 
  arrange(member_casual, rideable_type)  %>%  
  ggplot(aes(x = rideable_type, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+coord_polar()


## scale_x_continuous(breaks = pretty_breaks())


