#' @function: This script compares absoloute vessel pitch per minute with door distance readings
#' 
# Load libraries:
library(dplyr)
library(fuzzyjoin)
library(data.table)
library(ggplot2)

#Function to convert sensor time to timestamp
NumToTime<-function(x){
  y<-str_replace(x,"(\\d{1})(\\d{2})(\\d{2})$","\\1:\\2:\\3")
  y<-parse_time(y)
  #z<-as.POSIXct(y, format="%HH:%MM:%SS",origin = " 00:00:00")
  return(y)
  
}

Q<-head(sensor_data_processed$SensorTime,20)
NumToTime(Q)



#Read in data
sensor_data_processed <- read.csv(file="../data/processed/sensor_data_processed_igfs_2018.csv")
summarised_pitch_per_min <- read.csv(file="../data/processed/pitch_summarised_by_minute.csv")

#Need to create a time stamp for the sensor data to match by the minute
sensor_data_processed%>%
  mutate(SensorTime = NumToTime(SensorTime),
         date_shot = as.Date(date_shot))%>%
  mutate(sensor_datetime = 
           as.POSIXct(paste(date_shot,SensorTime),format="%Y-%m-%d %H:%M:%S"
))->sensor_data_processed

#reduce df and summarise per min
sensor_data_processed%>%
  select(fldCruiseStationNumber,sensor_datetime,door_dist)%>%
  group_by(sensor_datetime = as.POSIXct(cut(sensor_datetime, breaks="1 min")),fldCruiseStationNumber) %>%
  summarize(door_dist = median(door_dist,na.rm=TRUE))%>%
  rename(timestamp = sensor_datetime)->summarised_door_per_min

#rename and reformat pitch
summarised_pitch_per_min %>%
  rename(timestamp = OSS_TimeUTC)%>%
  mutate(timestamp = as.POSIXct(timestamp))->summarised_pitch_per_min

#Use data.table to join bc memory issues with dplyr
#Convert tibbles
summarised_pitch_per_min<-as.data.table(summarised_pitch_per_min)
summarised_door_per_min<-as.data.table(summarised_door_per_min)

#combine data.tables
pitch_door_combined<-as.data.table(summarised_pitch_per_min)[summarised_door_per_min, on = .(timestamp), roll = TRUE]
write.csv(pitch_door_combined,file="../data/processed/pitch_door_combined.csv")


dev.new()
pitch_door_combined%>%
  filter(fldCruiseStationNumber %in% c(1:10))%>%
ggplot(aes(x=absolute_pitch_per_min,y=door_dist))+
  geom_jitter()+
  stat_smooth(method = "loess")+
  facet_wrap(~fldCruiseStationNumber)+
  ylim(75,150)

dev.new()
pitch_door_combined%>%
  filter(fldCruiseStationNumber== 1)%>%
  ggplot(aes(x=timestamp,y=absolute_pitch_per_min))+
  geom_jitter()+
  geom_path()+
  geom_point(aes(x=timestamp,y=door_dist))+
  geom_path(aes(x=timestamp,y=door_dist))+
  scale_x_datetime(limits = as.POSIXct(c("2018-10-30 09:56:00","2018-10-30 10:30:00 PDT"), format = "%Y-%m-%d %H:%M:%S"))

  
  facet_wrap(~fldCruiseStationNumber)+
  ylim(75,150)

  
  