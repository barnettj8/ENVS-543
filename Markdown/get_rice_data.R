getRiceData <- function() {
  
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  
  url <- "https://docs.google.com/spreadsheets/d/1Mk1YGH9LqjF7drJE-td1G_JkdADOU0eMlrP01WFBT8s/pub?gid=0&single=true&output=csv"
  
  rice <- read_csv(url)
  
  result <- rice |>
    # Get rid of extra data (only choose the variables we want)
    select(DateTime, H2O_TempC, AirTempF, PH, WindDir, Rain_in, 
           Turbidity_ntu, SpCond_mScm, ODO_sat, Depth_m, Chla_ugl, RelHumidity, BP_HG, WindSpeed_mph) |>
    # Make date object
    mutate(Date = mdy_hms(DateTime, tz = "EST")) |>
    mutate(AirTemp = (AirTempF - 32) * 5 / 9) |>
    mutate(Rain_m = Rain_in*0.0254) |>
    mutate(WindSpeed_kph = WindSpeed_mph*1.609344) |>
    # Make Weekday, day, and month object
    mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) |>
    mutate(month = month(Date, label = TRUE, abbr = FALSE)) |>
    mutate(day = day(Date)) |>
    select(Date, month, day, Weekday, AirTemp, Rain_m,PH, WindDir, 
           Turbidity_ntu, SpCond_mScm, ODO_sat, Depth_m, Chla_ugl, RelHumidity, BP_HG, WindSpeed_kph)
 
  
#returns the result of the data
  
  return(result)
}



  


