## ANALYZE

# 1. member vs. casual total trips
tripdata_clean %>%
  group_by(member_casual) %>%
  summarize(number_of_trips = n(),
            percentage_of_total_trips = n() / nrow(tripdata_clean) * 100)

p <- ggplot(data = tripdata_clean) +
  geom_bar(mapping = aes(x = member_casual, fill = member_casual)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Total Trips by Customer Type",
       fill = "Customer Type",
       x = "Customer Type",
       y = "Number of Trips")
print(p)

# 2. member vs. casual bike types
tripdata_clean %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_trips = n(),
            percentage_of_total_trips = n() / nrow(tripdata_clean) * 100)

p <- ggplot(data = tripdata_clean) +
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual), 
           position = "dodge") +
  labs(title = "Comparison of Bike Types by Customer Type",
       fill = "Customer Type",
       x = "Bike Type",
       y = "Number of Trips")
print(p)

# 3. member vs. casual total number of trips by day of the week
p <- tripdata_clean %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_trips = n(), .groups="drop") %>% 
  ggplot() +
  geom_col(aes(x = day_of_week, y = number_of_trips, fill = member_casual), 
           position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Distribution of Total Trips by Day of the Week and Customer Type",
       fill = "Customer Type",
       x = "Day of the Week",
       y = "Number of Trips")
print(p)

# 4. member vs. casual total number of trips by month
p <- tripdata_clean %>%
  group_by(member_casual, month) %>% 
  summarise(number_of_trips = n(), .groups="drop") %>% 
  ggplot() +
  geom_col(aes(x = month, y = number_of_trips, fill = member_casual), 
           position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Distribution of Total Trips by Month and Customer Type",
       fill = "Customer Type",
       x = "Month",
       y = "Number of Trips")
print(p)

# 5. member vs. casual average trip length by day of the week
p <- tripdata_clean %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(average_trip_length = mean(trip_length), .groups="drop") %>% 
  ggplot() +
  geom_col(aes(x = day_of_week, y = average_trip_length, fill = member_casual), 
           position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Average Trip Length of Casuals and Members by Day of the Week",
       fill = "Customer Type",
       x = "Day of the Week",
       y = "Trip Length (min)")
print(p)

# 6. member vs. casual average trip length by month
p <- tripdata_clean %>%
  group_by(member_casual, month) %>% 
  summarise(average_trip_length = mean(trip_length), .groups="drop") %>% 
  ggplot() +
  geom_col(aes(x = month, y = average_trip_length, fill = member_casual), 
           position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Average Trip Length of Casuals and Members by Month",
       fill = "Customer Type",
       x = "Month",
       y = "Trip Length (min)")
print(p)

# 7. member vs. casual average trip distance
p <- tripdata_clean %>%
  group_by(member_casual) %>% 
  summarise(average_trip_distance = mean(trip_distance), .groups="drop") %>% 
  ggplot() +
  geom_col(aes(x = member_casual, y = average_trip_distance, fill = member_casual), 
           position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Average Trip Distance by Customer Type",
       fill = "Customer Type",
       x = "Customer Type",
       y = "Trip Distance (km)")
print(p)

# 8. member vs. casual average trip distance by day of week
p <- tripdata_clean %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(average_trip_distance = mean(trip_distance), .groups="drop") %>% 
  ggplot() +
  geom_col(aes(x = day_of_week, y = average_trip_distance, fill = member_casual), 
           position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Average Trip Distance by Customer Type",
       fill = "Customer Type",
       x = "Day of the Week",
       y = "Trip Distance (km)")
print(p)

# 9. member vs. casual average trip distance by month
p <- tripdata_clean %>%
  group_by(member_casual, month) %>% 
  summarise(average_trip_distance = mean(trip_distance), .groups="drop") %>% 
  ggplot() +
  geom_col(aes(x = month, y = average_trip_distance, fill = member_casual), 
           position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Monthly Average Trip Distance by Customer Type",
       fill = "Customer Type",
       x = "Month",
       y = "Trip Distance (km)")
print(p)

# 10. member vs. casual hourly demand per day
p <- ggplot(data = tripdata_clean) +
  geom_bar(mapping = aes(x = start_time, fill = member_casual)) +
  facet_wrap(~day_of_week) +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  labs(title = "Hourly Demand per Day of the Week by Customer Type",
       fill = "Customer Type",
       x = "Hour",
       y = "Number of Trips Started")
print(p)

# 11. member vs. casual most popular stations
station_popularity <- tripdata_clean %>%
  group_by(start_station_name, start_lng, start_lat, member_casual) %>%
  summarise(usage_count = n(), .groups = "drop")
member_station_popularity <- station_popularity %>% 
  filter(member_casual == "member") %>% 
  arrange(desc(usage_count)) %>% 
  head(10)
casual_station_popularity <- station_popularity %>% 
  filter(member_casual == "casual") %>% 
  arrange(desc(usage_count)) %>% 
  head(10)
# Create the leaflet map
p <- leaflet() %>%
  addTiles() %>%
  # add markers for top 10 stations for members
  addCircleMarkers(
    data = member_station_popularity,
    lng = ~start_lng, lat = ~start_lat,
    radius = ~sqrt(usage_count) / 15,
    color = "blue", fillOpacity = 0.7,
    popup = ~paste0(start_station_name, "<br>Members: ", usage_count)
  ) %>%
  # add markers for top 10 stations for casuals
  addCircleMarkers(
    data = casual_station_popularity,
    lng = ~start_lng, lat = ~start_lat,
    radius = ~sqrt(usage_count) / 15,
    color = "green", fillOpacity = 0.7,
    popup = ~paste0(start_station_name, "<br>Casuals: ", usage_count)
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Customer Type",
    colors = c("blue", "green"),
    labels = c("Members", "Casuals")
  )
print(p)
