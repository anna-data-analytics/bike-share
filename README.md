# CASE STUDY: Cyclistic Bike Share
Author: Anna Nguyen

Last update: 1 Jul 2024

#### Overview
This is my capstone project for the Google Data Analytics Professional Certificate on Coursera. In this case study, I will follow the six steps data analysis process:

Ask =\> Prepare =\> Process =\> Analyze =\> Share =\> Act

## Scenario

Cyclistic is a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, the analyst team wants to understand how casual riders and annual members use Cyclistic bikes differently. From there, the team will design a new marketing strategy to convert casual riders into annual members.

## Analysis

### 1. Ask

Three questions will guide the future marketing program: (1) How do annual members and casual riders use Cyclistic bikes differently? (2) Why would casual riders buy Cyclistic annual memberships? (3) How can Cyclistic use digital media to influence casual riders to become members?

In this case study, I will focus on analyzing Bike Share data to answer the following questions: How do annual members and casual riders use Cyclistic bikes differently?

### 2. Prepare

2.1 Data collection

I want to conduct a comprehensive examination of riding patterns and gain the insight of both casual and member riders, so I decided to work with data of one year. I collected 12 files from Jan 2023 to Dec 2023 from this open [data source](https://divvy-tripdata.s3.amazonaws.com/index.html).

The data has been made available by Motivate International Inc. under this [license](https://divvybikes.com/data-license-agreement).

2.2 Data Credibility

The data I have collected met the ROCC criteria for credibility assessment.

-   Reliable: The data is trustworthy, unbiased and undergone verification to ensure its suitability for the intended purpose.

-   Original: The data is collected directly from the primary provider.

-   Comprehensive: The data includes all necessary information to solve the questions.

-   Current: The most current data from 2023 was collected so it is up to date.

-   Cited: The data is appropriately referenced.

2.3 Analyze tool

I chose R to process the data as it is a powerful tool to handle a large set of data as well as to render data visualization.

### 3. Process

1.  Install and Load the Required Packages

```{r}
install.packages('tidyverse')
install.packages("geosphere")
```

```{r}
library(lubridate)
library(tidyverse)
library(dplyr)
library(geosphere)
```

2.  Set working directory

```{r}
setwd("C:/data/Data Course/Bikeshare/Process")
```

3.  Read Data from 12 CSV files into a Data Frame

```{r}
all_rides <-
  list.files(path = "C:/data/Data Course/Bikeshare/Process", pattern = "*.csv") %>% 
  map_df(~read_csv(.))

summary(all_rides)
```

The data frame has 5,719,877 rows.

4.  Cleaning Data

Check for NA values and duplicates.

```{r}
sum(duplicated(all_rides))
sum(is.na(all_rides))
```

There are no duplication in the data but 3,624,089 rows that has NA values in at least one of column. It is too much to delete all of NA rows which is more than half of the data and it would affect my analysis. I only delete the rows with NA values on the column that will used for later analysis.

I want to calculate the duration and of each ride so I need valid start time and end time, and the start time must be before the end time.

In order to be considered valid records, the rides also need valid coordinates of the start and end stations. So I will remove the rows with NA coordinates of the start and end stations.

There are rides that riders start and end the trip at the same stations so calculating the length of each ride base on the coordinates of the start and end point does not work for all the cases so I decided not to analyse length of each ride.

I also want to find the most popular stations for the bike riders, so I will remove the rows with NA start_station_name.

I will - filter the rows with started_at before ended_at

-   filter out the rows with NA started_at and ended_at

-   filter out the rows with NA start_lng, start_lat, end_lng, end_lat

-   filter out the rows with NA start_station_name

```{r}
all_rides_v2 <- all_rides %>%
  filter(!is.na(start_station_name))%>%
  filter(!is.na(started_at)) %>% 
  filter(!is.na(ended_at)) %>%
  filter(started_at < ended_at)%>%
  filter(!is.na(start_lng)) %>%
  filter(!is.na(start_lat)) %>% 
  filter(!is.na(end_lng)) %>% 
  filter(!is.na(end_lat))   
  
summary(all_rides_v2)
```

883,576 rows was filtered out

5.  Add a column for the length of each ride

```{r}
all_rides_v2$ride_length <- difftime(all_rides_v2$ended_at, all_rides_v2$started_at, units="mins")
head(all_rides_v2)
max(all_rides_v2$ride_length)
min(all_rides_v2$ride_length)
```

There are trips that the ride_length is less than 1 mins. It can be assumed that the riders did not really start riding so it is reasonable to remove those data.

```{r}
all_rides_v3 <- all_rides_v2 %>%
  filter(ride_length > 1)

summary(all_rides_v3)
```

105,383 rows was removed.

6.  Add a column shows the day in the week and a column shows the month of each ride

```{r}
all_rides_v3$weekday <- weekdays(all_rides_v3$started_at)
all_rides_v3$month <- format(all_rides_v3$started_at, format="%m")
head(all_rides_v3)
```

### Analyze

1.  See the different types of bikes used by member and casual riders

```{r}
all_rides_v3 %>%

group_by(member_casual, rideable_type) %>%

summarise(rideable_type_total = n())

```

There are 3 types of bike: *classic*, *electric* and *docked* bike. Both member and casual riders use more classic bikes than electric bikes. Casual riders also use the docked bikes while no member rider use this type of bike.

```{r}
library(ggplot2)

all_rides_v3 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(rideable_type_total = n())  %>%
  ggplot(aes(fill = rideable_type, y = rideable_type_total, x = member_casual)) + geom_bar(position='stack', stat = 'identity')  + labs(x = 'member_casual', y = 'rideable_type_total', title = 'Different types of bikes used by member and casual riders') + scale_y_continuous(name = "Number of Rides", labels = scales::comma)
  
```

2.  Visualize the number of rides by rider types

```{r}
all_rides_v3 %>%
  group_by(member_casual) %>%
  summarise(total_rider_type = n()) %>%
  ggplot(aes(x=member_casual, y=total_rider_type, fill=member_casual)) + geom_col(position="dodge") + geom_text(aes(label=total_rider_type, vjust=-0.5)) + scale_y_continuous(name = "Number of Rides", labels = scales::comma)
```

From the graph, it is evident that the number of member riders is greater than the number of casual riders.

3.  Visualize the number of the trips by user types on different days of the week

```{r}
all_rides_v3$day_of_week <- ordered(all_rides_v3$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

all_rides_v3 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides=n(), average_duration=mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x=day_of_week, y=number_of_rides, fill=member_casual)) + geom_col(position="dodge") + scale_y_continuous(name = "Number of Rides", labels = scales::comma)
```

The total rides of member riders is higher than the casual riders' in every day of the week. Member riders have the most rides on Tuesdays, Wednesday, Thursday and less on Saturday and Sunday. In contrast, casual riders travel more at weekend.

4.  Visualize the number of trips by user types in different months

```{r}
all_rides_v3 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides=n()) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual)) + geom_col(position="dodge") + scale_y_continuous(name = "Number of Rides", labels = scales::comma)
```

Looking at the trend in a year, both member and casual riders travel much more in summer time (from May to October) and less in winter time.

5.  Determine the most popular start stations

```{r}
Top_5_Start_Station_Names <- all_rides_v3 %>%
  count(start_station_name) %>%
  arrange(desc(n)) %>%
  slice(1:10)

view(Top_5_Start_Station_Names)
```

We can identify the most popular stations for both types of riders. This insight can be used to make an effective marketing campaigns and target potential riders.

### Share

To share the data, I have decided to use Tableau as if offers a great platform with easy "drag and drop" feature, understandable visualizations while facilitatiing the integreation of data.

1.  Save the clean data to a new .csv file to prepare for the Tableau visualization

Remove columns that are not used, to make the csv file size fit in Tableau file size limitation.

```{r}
all_rides_v4 <- all_rides_v3 %>%
  select(-c(ended_at, start_station_id, end_station_name, end_station_id, start_lng, start_lat,end_lng, end_lat, weekday, month, day_of_week))

head(all_rides_v4)
```

```{r}
all_rides_v4 %>%
  write.csv("all_rides_share_2.csv")
```

2.  [Tableau](https://public.tableau.com/app/profile/anna.nguyen4460/viz/BikeShareProject2023/Sheet1)

### Act

From the analysis above, I can make some recommendations for making an advertising campaign to target the casual riders, persuade them to register for the membership programes.

\- Make broad advertising campaign at popular stations where many casual riders can be seen.

\- Raise the awareness of benefits of memberships for casual rider regarding: foe example discounts they can have when riding more often and longer.

\- Create a variety of membership programs that are more flexible, such as for 1 month, 3 months, 6 months and 12 months. Rider can have chance to choose what fits their needs. For example, they can buy 3 months member ship as they only use it more during summer time.

\- In less popular months, and less popular days, launch a campaign to boost member usage, offering rewards or discounts.

### Conclusion

In conclusion, this analysis provides insights on how the Cyclistic members and casual riders use the service differently. By tailoring strateries to the identified differences, the company can effectively attract more casual riders to become members.
