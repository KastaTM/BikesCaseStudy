#PREPARE STEP

#Here we will prepare all the data in order to clean it and then work with it

#Import all libraries necessary
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(dplyr)


#Reading all the 2022 data into different variables
january <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202201-divvy-tripdata.csv")
february <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202202-divvy-tripdata.csv")
march <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202203-divvy-tripdata.csv")
april <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202204-divvy-tripdata.csv")
may <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202205-divvy-tripdata.csv")
june <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202206-divvy-tripdata.csv")
july <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202207-divvy-tripdata.csv")
august <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202208-divvy-tripdata.csv")
september <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202209-divvy-tripdata.csv")
october <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202210-divvy-tripdata.csv")
november <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202211-divvy-tripdata.csv")
december <- read.csv("C:/Users/Rodrigo/Desktop/DA/Datos Bici/202212-divvy-tripdata.csv")


#Checking consistency in the different datasets

#Checking if all the column names are the same on the datasets
colnames(january)
colnames(february)
colnames(march)
colnames(april)
colnames(may)
colnames(june)
colnames(july)
colnames(august)
colnames(september)
colnames(october)
colnames(november)
colnames(december)



#Checking the data format on all the datasets
str(january)
str(february)
str(march)
str(april)
str(may)
str(june)
str(july)
str(august)
str(september)
str(october)
str(november)
str(december)

#Taking a sneak peek on the head of the dataset
head(january)
head(february)
head(march)
head(april)
head(may)
head(june)
head(july)
head(august)
head(september)
head(october)
head(november)
head(december)

#Checking the number of rows of the datasets
nrow(january)
nrow(february)
nrow(march)
nrow(april)
nrow(may)
nrow(june)
nrow(july)
nrow(august)
nrow(september)
nrow(october)
nrow(november)
nrow(december)

#Checking the dimensions of the datasets
dim(january)
dim(february)
dim(march)
dim(april)
dim(may)
dim(june)
dim(july)
dim(august)
dim(september)
dim(october)
dim(november)
dim(december)


#Check a little summary of the data in the datasets
summary(january)
summary(february)
summary(march)
summary(april)
summary(may)
summary(june)
summary(july)
summary(august)
summary(september)
summary(october)
summary(november)
summary(december)


#Combine data sets
year_2022 <- bind_rows(january,february,march,april,may,june,july,august,september,october,november,december)



#PROCESS STEP

#Here we will prepare all the data in order to clean it and then work with it

#Check if the columns name are correct
colnames(year_2022)

#Check if the data type is correct
str(year_2022)

#Taking a sneak peek on the head of the dataset
head(year_2022)

#Check the number of rows of the combine dataset
nrow(year_2022)

#Check the dimension of the combined dataset
dim(year_2022)

#Check a little summary of the data in the datasets
summary(year_2022)


year_2022$date <- as.Date(year_2022$started_at) #The default format is yyyy-mm-dd
year_2022$month <- format(as.Date(year_2022$date), "%m")
year_2022$day <- format(as.Date(year_2022$date), "%d")
year_2022$year <- format(as.Date(year_2022$date), "%Y")
year_2022$day_of_week <- format(as.Date(year_2022$date), "%A")

# Convert dates into correct format
year_2022$started_at <- as.POSIXct(year_2022$started_at, format = "%Y-%m-%d %H:%M:%S")
year_2022$ended_at <- as.POSIXct(year_2022$ended_at, format = "%Y-%m-%d %H:%M:%S")
head(year_2022)

#Create a new column that shows the data of the duration of the rides
year_2022$ride_length <- difftime(year_2022$ended_at,year_2022$started_at)
head(year_2022)
year_2022$ride_length <- as.numeric(as.character(year_2022$ride_length))
head(year_2022)
is.numeric(year_2022$ride_length)

# Removed unnecesarry data
# Removed the rides that are negative (impossible) or that lasted more than a day (useless here)
min(year_2022$ride_length)
max(year_2022$ride_length)


#MODIFICAR EL CALCULO ESTÁ EN SEGUNDOS
year_2022_v2 <- year_2022[!(year_2022$ride_length <= 0 | year_2022$ride_length > 86400),]
head(year_2022_v2)

min_ride_length <- min(year_2022_v2$ride_length, na.rm = TRUE)
max_ride_length <- max(year_2022_v2$ride_length, na.rm = TRUE)
min_ride_length
max_ride_length


#Check if the columns name are correct
colnames(year_2022_v2)

#Check if the data type is correct
str(year_2022_v2)

#Taking a sneak peek on the head of the data frame
head(year_2022_v2)

#Check the number of rows of the combine data frame
nrow(year_2022_v2)

#Check the dimension of the combined data frame
dim(year_2022_v2)

#Check a little summary of the data in the data frames
summary(year_2022_v2)

#Check if there is any NA if so delete it
colSums(is.na(year_2022_v2))
year_2022_v2 <- drop_na(year_2022_v2)
colSums(is.na(year_2022_v2))

#Check the dataframe to confirm
colnames(year_2022_v2)
str(year_2022_v2)
head(year_2022_v2)
nrow(year_2022_v2)
dim(year_2022_v2)
summary(year_2022_v2)

#Check if there is any duplicate ride by id if so delete it
any(duplicated(year_2022_v2$ride_id))
year_2022_v3 <- year_2022_v2[!duplicated(year_2022_v2$ride_id),]
any(duplicated(year_2022_v3$ride_id))
dim(year_2022_v3) 

View(year_2022_v3)

#ANALYSE

#Number of rides per month

example_v3 <- year_2022_v2[!duplicated(year_2022_v2$ride_id),]

#How many people uses the bikes per month
meses = example_v3 %>%
  group_by(months = month.name[month(started_at)], member_casual) %>% 
    summarize(row_count = n()) %>% 
      arrange(match(months,month.name))

View(meses)

#Save the data frame to import into tableau
write.csv(meses, "meses.csv", row.names=FALSE)

          
#How many people uses the bikes per day
dias = example_v3 %>% 
  group_by(day_of_week, member_casual) %>% 
    summarize(row_count = n()) %>% 
      arrange(day_of_week)

View(dias)

write.csv(dias, "dias.csv", row.names=FALSE )

example_v3$month <-
  ordered(example_v3$month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July',
                                       'August', 'September', 'October', 'November', 'December'))

#Average ride per month
media_por_mes = example_v3 %>% 
  group_by(months = month.name[month(started_at)], member_casual) %>% 
    summarize(mean_ride = mean(ride_length)) %>%
      arrange(months)

#Average ride per day
media_por_dia = example_v3 %>% 
  group_by(day_of_week, member_casual) %>% 
    summarize(mean_ride = mean(ride_length)) %>% 
      arrange(match(day_of_week, c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")))



#Most used start station
top5_inicio = example_v3 %>%
    group_by(start_station_name, member_casual) %>%
      summarize(number_of_uses = n()) %>%
        arrange(desc(number_of_uses))

#Most used end station
top5_fin = example_v3 %>%
    group_by(end_station_name, member_casual) %>%
      summarize(number_of_uses = n()) %>%
        arrange(desc(number_of_uses))   %>%
          na.omit()

#Average ride
mean_travel <- example_v3 %>% 
  group_by(member_casual) %>% 
    summarize(mean(ride_length)) %>%
      na.omit()

View(mean_travel)

#How many people let the bike on the same station than the one that they took it
mismo_sitio <- example_v3 %>%
  group_by(member_casual) %>%
  summarize(bici_mismo_sitio = sum(start_station_name == end_station_name & start_station_name!="")) %>%
  filter(bici_mismo_sitio > 0)


#How many people let the bike on a different station than the one that they took it
distinto_sitio <- example_v3 %>%
  group_by(member_casual) %>%
  summarize(bici_distinto_sitio = sum(start_station_name != end_station_name & start_station_name!="")) %>%
  filter(bici_distinto_sitio > 0)


#How many people used an electric_bike
electricas_por_dia <- example_v3 %>%
  group_by(day_of_week, member_casual) %>% 
  summarize(number_of_electric = sum(rideable_type == "electric_bike"),) %>% 
  arrange(day_of_week)


#How many people used an classic_bike
clasicas_por_dia <- example_v3 %>%
  group_by(day_of_week, member_casual) %>% 
  summarize(number_of_classics = sum(rideable_type == "classic_bike"),) %>% 
  arrange(day_of_week)


##SHARE

#Times the bikes are used per month
custom_levels <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
example_v3 <- example_v3 %>%
  mutate(month_of_year = month.name[month(started_at)])

example_v3$month_of_year <- factor(example_v3$month_of_year, levels = month.name)

example_v3 %>%
  group_by(month_of_year, member_casual) %>% 
    summarize(number_of_rides = n()) %>% 
      arrange(month_of_year) %>%
        ggplot(aes(x = month_of_year, y = number_of_rides, fill = member_casual)) + 
          geom_bar(position = "dodge", stat = "identity") +
          labs(x = "Mes", y = "Número de viajes") +
          theme_minimal()

#Times the bikes are used per day
example_v3$day_of_week <- 
  ordered(example_v3$day_of_week, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

example_v3 %>%
  group_by(day_of_week, member_casual) %>% 
  summarize(number_of_rides = n(),) %>% 
  arrange(day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Día", y = "Número de viajes") +
  theme_minimal()




#Average ride duration per day
example_v3 %>%
  group_by(day_of_week, member_casual) %>% 
  summarize(average_ride_length = mean(ride_length), .groups = 'drop') %>% 
  arrange(day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Día", y = "Media de tiempo") +
  theme_minimal()


#Average ride duration per month
example_v3 %>%
  group_by(month_of_year, member_casual) %>% 
  summarize(average_ride_length = mean(ride_length), .groups = 'drop') %>% 
  arrange(month_of_year) %>%
  ggplot(aes(x = month_of_year, y = average_ride_length, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Mes", y = "Media de tiempo") +
  theme_minimal()


#Count of every person that used a electric_bike
example_v3 %>%
  group_by(day_of_week, member_casual) %>% 
  summarize(number_of_electric = sum(rideable_type == "electric_bike"),) %>% 
  arrange(day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_electric, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Día", y = "Número de electricas") +
  theme_minimal()




#Count of every person that used a classic_bike
example_v3 %>%
  group_by(day_of_week, member_casual) %>% 
  summarize(number_of_classics = sum(rideable_type == "classic_bike"),) %>% 
  arrange(day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_electric, fill = member_casual)) + 
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Día", y = "Número de electricas") +
  theme_minimal()

