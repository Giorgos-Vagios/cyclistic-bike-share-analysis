library(tidyverse)
library(lubridate)

#  Import data from csv. ----
q1_2019 <- read_csv("Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv")

# Rename Q1 2019 ----
q1_2019 <- q1_2019 %>%
  rename(
    ride_id  = trip_id,
    started_at = start_time,
    ended_at = end_time,
    start_station_name = from_station_name,
    end_station_name = to_station_name,
    member_casual = usertype
  )

# Convert usertype to same labels as 2020 ----
q1_2019 <- q1_2019 %>%
  mutate(member_casual = if_else(member_casual == "Subscriber", "member", "casual"))

# Keep only the necessary columns ----
q1_2019 <- q1_2019 %>%
  select(ride_id, started_at, ended_at,
         start_station_name, end_station_name, member_casual)

q1_2020 <- q1_2020 %>%
  select(ride_id, started_at, ended_at,
         start_station_name, end_station_name, member_casual)


q1_2019 <- q1_2019 %>% mutate(ride_id = as.character(ride_id))
q1_2020 <- q1_2020 %>% mutate(ride_id = as.character(ride_id))


# Merge datasets ----
all_trips <- bind_rows(q1_2019, q1_2020)

# Check user types ----
unique(q1_2019$member_casual)
unique(q1_2020$member_casual)

# Convert timestamps ----
all_trips <- all_trips %>%
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at   = as.POSIXct(ended_at,   format = "%Y-%m-%d %H:%M:%S")
  )
# Remove missing and invalid rows ----
all_trips <- all_trips %>%
  filter(
    !is.na(started_at),
    !is.na(ended_at),
    !is.na(start_station_name),
    !is.na(end_station_name),
    !is.na(member_casual),
    ended_at > started_at
  )

# Create ride_length in minutes ----
all_trips <- all_trips %>%
  mutate(ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")))

# Remove extreme outliers (rides > 24h) ----
all_trips <- all_trips %>%
  filter(ride_length > 0, ride_length < 1440)

summary(all_trips$ride_length)

# Create day_of_week ----
all_trips <- all_trips %>%
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE))

head(all_trips)

all_trips %>%
  group_by(member_casual) %>%
  summarise(
    mean_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length),
    min_ride_length = min(ride_length),
    number_of_rides = n()
  )

all_trips %>%
  count(day_of_week, sort = TRUE)

all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = "drop")



all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(mean_ride_length = mean(ride_length), .groups = "drop")


# Total rides per user type ----
all_trips %>%
  count(member_casual) %>%
  ggplot(aes(x = member_casual, y = n, fill = member_casual)) +
  geom_col() +
  labs(
    title = "Total Rides: Members vs Casual Riders",
    x = "User Type",
    y = "Number of Rides"
  ) +
  theme_minimal()


# Rides per weekday per user type ----
all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Rides per Day of Week by User Type",
    x = "Day of Week",
    y = "Number of Rides",
    fill = "User Type"
  ) +
  theme_minimal()


# Average ride length per user type ----
all_trips %>%
  group_by(member_casual) %>%
  summarise(mean_ride_length = mean(ride_length)) %>%
  ggplot(aes(x = member_casual, y = mean_ride_length, fill = member_casual)) +
  geom_col() +
  labs(
    title = "Average Ride Duration: Members vs Casual Riders",
    x = "User Type",
    y = "Average Duration (minutes)"
  ) +
  theme_minimal()

