library(ggplot2)
library(dplyr)
library(scales)
library(tidyverse)
library(readr)
library(zeallot)
library(countrycode)
rm(list = ls())

# Read the Data from CSV file into R server
hotel_data <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv"
  )

hotel_data[sapply(hotel_data, is.character)] <-
  lapply(hotel_data[sapply(hotel_data, is.character)], as.factor)
str(hotel_data)
summary(hotel_data)

# Children Column has 4 missing values, here we will replace the missing value in Children with
# the values from corresponding babies column
n <- length(hotel_data$children)
for (i in 1:n) {
  if (is.na(hotel_data$children[i]))
    hotel_data$children[i] <- hotel_data$babies[i]
}

# Check if all the missing values are replaced
colSums(is.na(hotel_data))

# Check the number of booking at respective hotels
table(hotel_data$hotel)
# Visualize the distribution
ggplot(data = hotel_data, aes(x = hotel)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Hotel type",
       x = "Hotel type",
       y = "No. of bookings") +
  theme_classic() + scale_color_brewer(palette = "Set2")

# We simply have more booking in City hotel which is expected as more people come for
# business meetings, pleasure and other reasons and people in Resorts often go there for
# Vacations and famlily holidays

# Check the distribution of hotel type for cancellation
table(hotel_data$is_canceled, hotel_data$hotel)

# Visualize the cancellation by hotek type
ggplot(data = hotel_data,
       aes(
         x = hotel,
         y = prop.table(stat(count)),
         fill = factor(is_canceled),
         label = scales::percent(prop.table(stat(count)))
       )) +
  geom_bar(position = position_dodge()) +
  geom_text(
    stat = "count",
    position = position_dodge(.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cancellation Status by Hotel Type",
       x = "Hotel Type",
       y = "Count") +
  theme_classic() +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  )

# Out of all the booking reservation made, around 67% booking request were made in City Hotel
# and only 335 booking were made for resort hotel. out of all the booking request made around
# 62% booking were cancelled and 38% booking request were confirmed.

# Cancellation ratio by Hotel Type based on the lead time. Lead time is the time gap between
# Booking made and the actual date of check in. We will visualize the data by using BoxPlot

ggplot(data = hotel_data, aes(
  x = hotel,
  y = lead_time,
  fill = factor(is_canceled)
)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Cancellation By Hotel Type",
    subtitle = "Based on Lead Time",
    x = "Hotel Type",
    y = "Lead Time (Days)"
  ) +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  ) + theme_light()

# In both the City Hotel and Resort Hotel, the cancellation is normally done in early days
# after booking, once more time elapsed between Booking and Check in the chance of Cancellation
# is less. This trend is common in both type of Hotels.
# there are few exception in both the cases which is represented here by Outliers but even the
# average outlier lead time less in case of cancellation than Not Cancelled.

unique(hotel_data$arrival_date_year)
# since the data is only for 3 years we will go into year Visualization but we will visualize
# the Month wise analysis for hotel booking

# Organize the Month in proper order
hotel_data$arrival_date_month <-
  factor(hotel_data$arrival_date_month, levels = month.name)

# Visualize Hotel traffic on Monthly basis
ggplot(data = hotel_data, aes(x = arrival_date_month)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + labs(title = "Month Wise Booking Request",
                      x = "Month",
                      y = "Count") +
  theme_classic()
# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(hotel_data, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

# where are the people coming from
hotel_data_1 <- hotel_data[hotel_data$reservation_status == "Check-Out",]

# Subset the data to include the countries which has more than 1500 reservation request
# otherwise including all the country with few or occassional request to avoid the graph
# from being clumsy
sub_hotel <- hotel_data_1 %>% 
  group_by(country) %>% 
  filter(n() > 1500)

# Visualize the Travellor by Country.
sub_hotel$county_name <- countrycode(sub_hotel$country, 
                                     origin = "iso3c",
                                     destination = "country.name")

# Traveller by Country per hotel wise
ggplot(sub_hotel, aes(county_name, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Booking Status by Country",
       x = "Country",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())

# As the data we have is for hotels from Portugal so it is expected that locals 
# books the hotels more for both city and resort hotel, apart from that the travellers
# are from nearby country people from United Kingdom book resort hotel more for sightseeing

# Total Stay Duration
ggplot(sub_hotel, aes(stays_in_weekend_nights + stays_in_week_nights)) + 
  geom_density(col = "red") +facet_wrap(~hotel) + theme_bw()

# Average daily rate by Hotel Type
ggplot(sub_hotel, aes(x = adr, fill = hotel, color = hotel)) + 
  geom_histogram(aes(y = ..density..), position = position_dodge(), binwidth = 20 ) +
  geom_density(alpha = 0.2) + 
  labs(title = "Average Daily rate by Hotel",
       x = "Hotel Price(in Euro)",
       y = "Count") + scale_color_brewer(palette = "Paired") + 
  theme_classic() + theme(legend.position = "top")

ggplot(sub_hotel, aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type",
       x = "Customer Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())

# Does the hotel charged differently for different customer type
ggplot(sub_hotel, aes(x = customer_type, y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  labs(title = "Price Charged by Hotel Type",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(in Euro)") + theme_classic()

# Does allocation of different room lead to cancellation 
# keep only that data where reserved room is different than the room allocated

df <- subset(hotel_data, 
             as.character(hotel_data$reserved_room_type) != as.character(hotel_data$assigned_room_type))

table(df$is_canceled)
# It seems that the Different room assignment does not have much impact on booking as 
# only few people have cancelled when there is a room change.


# Data Modelling

