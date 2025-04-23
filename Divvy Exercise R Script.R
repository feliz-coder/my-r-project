#helps wrangle data
library(tidyverse) 

# Use the conflicted package to manage conflicts
library(conflicted)

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#=====================
# STEP 1: COLLECT DATA
#=====================
# # Upload Divvy datasets (csv files) here
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare structures and data of each file
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
glimpse(q1_2019)
glimpse(q1_2020)

# Rename columns to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
(q1_2019 <- rename(q1_2019
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

# Remove trip_id, bikeid, gender, birthyear from q1_2019 data frame as there is no matching record of them in q1_2020.
q1_2019  <- q1_2019 %>% 
  select(-c(trip_id, bikeid, gender, birthyear))

# Remove ride_id, rideable_type, start_lat, start_lng, end_lat, end_lng from q1_2020 data frame as there is no matching record of them in q1_2019.
q1_2020 <- q1_2020 %>%
  select(-c(ride_id, rideable_type, start_lat, start_lng, end_lat, end_lng)) %>%
  mutate(tripduration = as.numeric(ended_at - started_at, units = "secs"))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q1_2019, q1_2020)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame?
head(all_trips) #See the first 6 rows of data frame. 
tail(all_trips)
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics

# Fixing problem 1
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Fixing problem 2
# Add columns that list the date, month, day, and year of each ride. This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Fixing problem 3
# Some tripdurations have negative values and bikes that were taken out of docks and checked for quality. We need to drop them for this analysis.
all_trips_v2 <- all_trips %>%
  filter(tripduration >= 0 & start_station_name != "HQ QR")

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on tripduration (all figures in seconds)
summary(all_trips_v2$tripduration)

# Compare members and casual users
aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# Analyze ridership data by type and weekday
summary_table <- all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>% #groups by usertype and weekday
  summarize(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(tripduration) # calculates the average duration
            ,median_duration = median(tripduration)
            ,min_duration = min(tripduration),
            ,max_duration = max(tripduration)
            ,stddv = sd(tripduration)) %>%
            arrange(member_casual, day_of_week) # sorts

# Visualize the average duration
summary_table %>% ggplot(aes(x=day_of_week, y=median_duration, fill=member_casual)) +
  geom_col(position="dodge") +
  labs(x="Day of Week",
       y="Median Duration",
       title="Median trip duration comparison between members and casual riders")

# Visualize the number of rides
summary_table %>% ggplot(aes(x=day_of_week, y=number_of_rides, fill=member_casual)) +
  geom_col(position="dodge") +
  labs(x="Day of Week",
       y="Number of Rides",
       title="Number of rides comparison between members and casual riders")

# Visualize using scatterplot
ggplot(data=all_trips_v2, mapping=aes(x=day_of_week, y=tripduration, color=member_casual)) +
  geom_boxplot() + 
  labs(x="Day of Week",
       y="Tripduration",
       title="Tripduration comparison between memberes and casual riders") +
  theme_minimal()

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
avg_ride_length <- aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(avg_ride_length, file = 'avg_ride_length.csv')

median_ride_length <- aggregate(all_trips_v2$tripduration ~ all_trips_v2$member_casual + 
                      all_trips_v2$day_of_week, FUN = median)
write.csv(median_ride_length, file = 'median_ride_length.csv')

write.csv(summary_table, file = 'summary_table.csv')

write.csv(all_trips_v2, file = 'all_trips_v2.csv')