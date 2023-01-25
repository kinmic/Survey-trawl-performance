# Fish 572 project 
#' @Title Examining effect on Pitch on Survey trawl performance 
#' @Author: M. Kinneen (mkinneen@uw.edu)
#' @data Irish Groundfish Survey 2018 - Mrine Institute Ireland
#' @Script_use This script is required for, reading, summarising and cleaning
#' of the data
#' 
#

#Load required libraires
library(tidyverse)
library(readxl)
library(tidyverse)
library(reshape2)
library(GGally)
library(lmtest)
library(kableExtra)
library(caret)
library(lubridate)


#Read in data
# Trawl sensor recordings
fss_sum <- read_xlsx("../data/raw/SurveyObsDataProj.xlsx", sheet = 2)
#Read oss underway data
oss_underway_raw<- read_xlsx("../data/raw/SurveyObsDataProj.xlsx", sheet = 4)

#underway data from vessel
fss_nmea <- read.csv("../data/raw/nmea_underway.csv", header = T, sep = ",")

#Blank ghauls (no infor for vessel motion)
null_hauls<-c(40,41,159,161)

#Restructure and summarise data
#Pivot data out so sensor measurements in different columns
sensor_data <- fss_nmea %>%
  pivot_wider(names_from = SensorType, values_from = MeasurementValue) %>%
  pivot_wider(names_from = SensorID, values_from = DST) %>%
  rename(door_dist = "1", wing_dist = "7") %>%
  pivot_wider(
    names_from = c(SensorNo, MeasurementType),
    values_from = c(TS, TEY, TSP)
  ) %>%
  filter(!fldCruiseStationNumber %in% null_hauls)%>%
  rename(
    TS_fish_density = "TS_4_F",
    TS_opening = "TS_4_O",
    TS_clearance = "TS_4_C",
    TS_height = "TS_4_H",
    TSP_along_speed = "TSP_6_Y",
    TSP_across_speed = "TSP_6_X",
    TEY_fish_density = "TEY_3_F",
    TEY_clearance = "TEY_3_C",
    TEY_opening = "TEY_3_O",
    TEY_height = "TEY_3_H"
  ) %>%
  mutate(
    ObsHeave_m = as.numeric(ObsHeave_m),
    ObsPitch_deg = as.numeric(ObsPitch_deg),
    ObsRoll_deg = as.numeric(ObsRoll_deg)
  ) %>%
  mutate(DepthBand = cut(as.numeric(fldShotDepth), breaks = seq(0, 800, 30))) %>%
  #Delete empty columns create din the pivot matrix
  select_if( ~ sum(!is.na(.)) > 0)


#Deals with door
# Values above 250 are sensor errors, so impute with -9 and  remove
sensor_data <- sensor_data %>%
  mutate(door_dist = ifelse(door_dist >= 150, -9, door_dist)) %>%
  #values belwo 20 are physically unrealistic, replace with -9
  mutate(door_dist = ifelse(door_dist <= 20,-9, door_dist))



#Deals with wing
sensor_data <- sensor_data %>%
  #Less then 5 and greater than 100 are likley shooting/hauling values
  mutate(wing_dist = ifelse(wing_dist  >= 100, -9, wing_dist)) %>%
  mutate(wing_dist = ifelse(wing_dist <= 5,-9, wing_dist))

#Correct input errors for sweeps
sensor_data <- sensor_data %>%
  mutate(SweepLngt_m = fct_recode(SweepLngt_m,
                                  "L" = "l",
                                  "S" = "s")) %>%
  mutate(fldGearCode = as.factor(fldGearCode))

#Pull date from fss summary
#Note, this is suitable for daytime trawl data only
vars<-c("fldCruiseStationNumber","fldDateTimeShot")
haul_dates<-fss_sum[vars]
haul_dates%>%
  mutate(date_shot = as.Date(ymd_hms(fldDateTimeShot))
)->haul_dates

#Add dates to sensor data
sensor_data<-sensor_data%>%
  full_join(haul_dates,by="fldCruiseStationNumber")

#Create list of non-sensor/Ship based variables
#Crusie number, Depth, Heading, Warp , Sweep type etc
#include station number in each to act as primary key
ship_based_vars<-sensor_data[,c(1:6,8,12:14,17)]
sbv_names<-c(colnames(ship_based_vars))

ship_based_summary<-ship_based_vars%>%
  group_by(fldCruiseStationNumber)%>%
  distinct()

ship_based_vars%>%
  group_by(fldCruiseStationNumber)%>%
  distinct()

#Create a list of sensor vars
sensor_vars<-sensor_data[,c(2,23:31)]
sv_names<-c(colnames(sensor_vars))

Sensor_var_sum<-sensor_data%>%
  group_by(fldCruiseStationNumber)%>%
  summarise(across(
    .cols = c(door_dist:TSP_across_speed), 
    .fns = list(Median = median, sd = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

ship_motion_vars<-sensor_data[,c(2,9:11)]
smv_vars<-c(colnames(ship_motion_vars))

ship_motion_vars_summary<-ship_motion_vars%>%
  group_by(fldCruiseStationNumber)%>%
  distinct()


#Join summary columns together using station number as a foreign/primary key
sensor_data_summary <-
  full_join(ship_based_summary, ship_motion_vars_summary, by = "fldCruiseStationNumber")
sensor_data_summary <-
  full_join(sensor_data_summary, Sensor_var_sum, by = "fldCruiseStationNumber") %>%
  mutate(fldShotDepth = as.numeric(fldShotDepth),
         Warp_m = as.numeric(Warp_m)) %>%
  #Filter out '-9' or flagged values
  filter(door_dist_Median != -9) %>%
  filter(wing_dist_Median != -9) %>%
  filter(TS_height_Median != -9)



#Save processed data files
write.csv(sensor_data_summary,file="../data/processed/sensor_data_summary_igfs_2018.csv")
write.csv(sensor_data,file="../data/processed/sensor_data_processed_igfs_2018.csv")


##Summarise OSS data for Pitch, Heave and roll by minute
oss_underway_raw%>%
  mutate(OSS_TimeUTC = as.factor(OSS_TimeUTC))%>%
  #group by factor( minutes)
  group_by(OSS_TimeUTC,fldCruiseStationNumber)%>%
  summarise(absolute_pitch_per_min = sum(abs(as.numeric(OSS_VesselPitch))))%>%
  rename(timestamp = OSS_TimeUTC)%>%
  mutate(timestamp = as.POSIXct(timestamp))->pitch_summarised_by_minute

  write.csv(pitch_summarised_by_minute,file="../data/processed/pitch_summarised_by_minute.csv")

#Check for largest mean wind per haul (worst conditions)

oss_underway_raw%>%
  group_by(fldCruiseStationNumber)%>%
  summarise(mean_wind = mean(as.numeric(OSS_TrueWindSpeed),na.rm=TRUE))%>%
  arrange(desc(mean_wind))%>%
  head(10)%>%
  select(fldCruiseStationNumber)%>%
  write.csv("../data/processed/poor_weather_hauls.csv")
