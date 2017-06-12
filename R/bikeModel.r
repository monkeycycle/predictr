library(tidyverse)
library(lubridate)
library(jsonlite)
library(broom)

setwd("/Users/nick/rpackages/predictr/R/data")
trips <- read_csv("trip.csv") 
stations <- read_csv("station.csv")
weather <- read_csv("weather.csv")

trips_time <- trips %>% 
  mutate(starttime = mdy_hm(starttime),
         Date = format(starttime, "%Y%m%d"),
         day_of_week = weekdays(starttime),
         hour_of_day = hour(starttime),
         tripduration = round(tripduration)) %>% 
  select(-stoptime, -from_station_name, -to_station_name)

weather_time <- weather %>% 
  mutate(Date = format(mdy(Date), "%Y%m%d"))

all_data <- trips_time %>% 
  left_join(stations, by = c("from_station_id" = "station_id")) %>% 
  left_join(stations, by = c("to_station_id" = "station_id"), suffix = c("_from", "_to")) %>% 
  left_join(weather_time, by = c("Date")) %>% 
  filter(usertype != "starttime" & gender != "stoptime" & usertype == "Member") %>% 
  mutate(decomissioned = is.na(decommission_date_to),
         age = 2016 - birthyear,
         age_dock_from = difftime(starttime, mdy(install_date_from), units = "days"),
         age_dock_to   = difftime(starttime, mdy(install_date_to), units = "days"),
         raining       = ifelse(Precipitation_In>0, "yes", "no"),
         going_nowhere = from_station_id == to_station_id, 
         log_tripduration = log(tripduration)
  ) %>% 
  select(-starttime, -name_from, -name_to,  
         -install_dockcount_to, -install_dockcount_from, 
         -modification_date_from, -modification_date_to, 
         -decommission_date_from, -decommission_date_to,
         -install_date_from, -install_date_to, 
         -birthyear,-Date, -bikeid, -usertype, -Events,
         -age_dock_from, -age_dock_to)


model_formula <- log_tripduration ~ 
  gender + 
  going_nowhere + 
  current_dockcount_from + 
  current_dockcount_to + 
  Mean_Humidity + 
  Mean_Sea_Level_Pressure_In + 
  Mean_Visibility_Miles + 
  Mean_Wind_Speed_MPH + 
  age + 
  hour_of_day*day_of_week + 
  Mean_Temperature_F*Precipitation_In

ols_fit <- lm(model_formula, data = all_data)
tidy_fit <- tidy(ols_fit)


model_predictors <- attr(ols_fit$terms,"dataClasses") %>% 
  data_frame(
    variable = names(.),
    type = .
  )


#Turn original data into a tidy dataframe
tidy_data <-  all_data %>% 
  gather(variable, value) 


predictor_info <- list()

for(i in 1: dim(model_predictors)[1]){
  predictor      <- model_predictors$variable[i]
  type           <- model_predictors$type[i]

  if(type == "numeric"){
    values <- tidy_data %>% 
      filter(variable == predictor) %>% 
      mutate(value = as.numeric(value)) %>% 
      .$value
    
    predictor_info[[predictor]] <- 
      list(
        type = type[1], 
        mean    = mean(values, na.rm = T),
        median  = median(values, na.rm = T),
        min     = min(values, na.rm = T),
        max     = max(values, na.rm = T),
        std_dev = sd(values, na.rm = T)
      )
  } else {
    values <- tidy_data %>% 
      filter(variable == predictor) %>% 
      group_by(value) %>% 
      summarise(times_seen = n()) %>% 
      arrange(-times_seen)

    predictor_info[[predictor]] <- 
      list(
        type = type[1], 
        unique = values$value,
        most_common = values$value[1]
      )
  }
  print(predictor)
}


toJSON(predictor_info, pretty = T)
