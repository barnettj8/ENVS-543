

#creation of the function to download the rice data 

getRiceData <- function(){
  
  library(tidyverse)
  library(knitr)
  
  url <- "https://docs.google.com/spreadsheets/d/1Mk1YGH9LqjF7drJE-td1G_JkdADOU0eMlrP01WFBT8s/pub?gid=0&single=true&output=csv"
  
  read_csv(url) -> rice
  
  names(rice)
  
  rice |>
    # get rid of extra data (only choose the variables we want)
    select( DateTime, H2O_TempC, AirTempF, Rain_in, PH, WindDir, Turbidity_ntu, SpCond_mScm, ODO_sat, Depth_m, Chla_ugl) |>
    # make date object
    mutate( Date = mdy_hms( DateTime, tz= "EST")) |>
    mutate( AirTemp = (AirTempF - 32) * 1.8) |>
    # make Weekday, day, and month object
    mutate( Weekday = wday( Date,
                            label = TRUE,
                            abbr = FALSE)) |>
    mutate( month = month( Date, 
                           label = TRUE,
                           abbr = FALSE)) |>
    mutate( day = day(Date)) |>
    group_by( Date) |>
    # calling all objects for table summary
    summarise( `Weekday` = Weekday,
               `Month` = month,
               `Day` = day,
               # reorder the columns
               `Depth` = mean(Depth_m),
               `Water Temp` = mean(H2O_TempC, na.rm = TRUE),
               `Air Temp` = mean(AirTemp),
               `Rain Inches` = mean(Rain_in),
               `PH` = mean(PH),
               `Wind Direction` = mean(WindDir),
               `Turbidity` = mean(Turbidity_ntu),
               `Specific Conductivity` = mean(SpCond_mScm),
               `Dissolved Oxygen (sat)` = mean(ODO_sat),
               `Chlorophyll-a` = mean(Chla_ugl)) |>
    kable() |>
    kable_minimal()

  return(rice)

  }

# returns the rice center data summarized by the parameters in the getRiceData function
getRiceData()
  


