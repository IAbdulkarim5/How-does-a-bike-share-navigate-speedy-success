

#Case study: Howdoesabike-share navigate speedy success?

#library we are use

library(tidyverse)
library(readr)

#download the csv file # STEP 1

setwd('C:/Users/xxabo/Desktop/')

q1 <- read.csv('Div2019.csv')
q2 <- read.csv('Div2020.csv')

colnames(q1)
colnames(q2)


#STEP 2
#rename some columns from " q1 " to make them consistent with " q2 "
q1 <- rename(q1,
              ride_id = trip_id,
              rideable_type = bikeid,
              started_at = start_time,
              ended_at = end_time,
              start_station_name = from_station_name,
              start_station_id = from_station_id,
              end_station_name = to_station_name,
              end_station_id = to_station_id,
              member_casual = usertype,
)
  
  ##show the data after rename 
  str(q1)
  str(q2)
        head(q1)
        head(q2)
              
#conver Ride_id and rideable_type to character so that they can stack correctly
        
        q1 <- mutate(q1, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))
        
  #Stack individual quarter's frames into one big data rame
        all_trips <- bind_rows(q1, q2)
        
        #Remove some data that we don't wanna it 
        all_trips <- all_trips %>% 
          select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender, tripduration))
    
        #STEP 3
        #Clean up and add data tp prepare for analysis 
        # we will check our new table " all_trips " to know if we need to fix something
        colnames(all_trips) # names of columns
       view(all_trips)
         nrow(all_trips) #how many rows
        head(all_trips)
        str(all_trips)
        dim(all_trips) #rrows + columns 
        summary(all_trips)
        
        #in column member_casual we can see we have 4 type we will change it to two " member " and " casual "
        all_trips <- all_trips %>%  
          mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))
        table(all_trips$member_casual)## to show all things going great
        
        #Add columns that list the date (Month, day, year ) for each ride
        # This will allow us to aggregate ride data for each month, day, or year .. before completing
        #The default format is yyyy-mm-dd
        
        View(all_trips)
        
        all_trips$date <- as.Date(all_trips$started_at) 
        all_trips$month <- format(as.Date(all_trips$date), "%m")
        all_trips$day <- format(as.Date(all_trips$date), "%d")
        all_trips$year <- format(as.Date(all_trips$date), "%Y")
        all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
        
        
        # Add a "ride_length" calculation to all_trips (in seconds)
        
              all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
              str(all_trips)
              # Convert "ride_length" from Factor to numeric so we can run calculations on the data
              
              all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
              is.numeric(all_trips$ride_length)
              
              View(all_trips)
              # The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
              # We will create a new version of the dataframe (v2) since data is being removed
              
              all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
              
              View(all_trips_v2)
              
              # Descriptive analysis on ride_length (all figures in seconds)
              #we can use summary to make the four lines above to one but also like that easy to read and understand
              mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
              median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
              max(all_trips_v2$ride_length) #longest ride
              min(all_trips_v2$ride_length) #shortest ride
                
              
              # Compare member and casual users
              aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
              aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
              aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
              aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
              
              # See the average ride time by each day for members vs casual users
              aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
                    #the days of the week are out of order we will fix that by doing
              all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
                        all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
                        
                        #after we fix it
                        aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
                        
                        # analyze ridership data by type and weekday
                        all_trips_v2 %>%
                          mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
                          group_by(member_casual, weekday) %>%  #groups by usertype and weekday
                          summarise(number_of_rides = n()							#calculates the number of rides and average duration
                                    ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
                          arrange(member_casual, weekday)								# sorts
                        
                        # Let's visualize the number of rides by rider type
                        all_trips_v2 %>%
                          mutate(weekday = wday(started_at, label = TRUE)) %>%
                          group_by(member_casual, weekday) %>%
                          summarise(number_of_rides = n()
                                    ,average_duration = mean(ride_length)) %>%
                          arrange(member_casual, weekday)  %>%
                          ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
                          geom_col(position = "dodge")
                        
                        # Let's create a visualization for average duration
                        all_trips_v2 %>%
                          mutate(weekday = wday(started_at, label = TRUE)) %>%
                          group_by(member_casual, weekday) %>%
                          summarise(number_of_rides = n()
                                    ,average_duration = mean(ride_length)) %>%
                          arrange(member_casual, weekday)  %>%
                          ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
                          geom_col(position = "dodge")
                        
                        #make file to save it 
                        
                        write.csv("C:\\Users\\xxabo\\Desktop\\Case_Study_in_R", row.names = FALSE)
                        
                        
                        
              
        