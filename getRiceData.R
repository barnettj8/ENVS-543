getRiceData <- function() {
  
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  
  url <- "https://docs.google.com/spreadsheets/d/1Mk1YGH9LqjF7drJE-td1G_JkdADOU0eMlrP01WFBT8s/pub?gid=0&single=true&output=csv"
  
  rice <- read_csv(url)
  
  # Print the names of the columns to check
  print(names(rice))
  
  result <- rice |>
    # Get rid of extra data (only choose the variables we want)
    select(DateTime, H2O_TempC, AirTempF, PH, WindDir, 
           Turbidity_ntu, SpCond_mScm, ODO_sat, Depth_m, Chla_ugl) |>
    # Make date object
    mutate(Date = mdy_hms(DateTime, tz = "EST")) |>
    mutate(AirTemp = (AirTempF - 32) * 5 / 9) |>
    # Make Weekday, day, and month object
    mutate(Weekday = wday(Date, label = TRUE, abbr = FALSE)) |>
    mutate(month = month(Date, label = TRUE, abbr = FALSE)) |>
    mutate(day = day(Date)) |>
    group_by(Date) |>
    # Calling all objects for table summary
    summarise(
      Weekday = first(Weekday),
      Month = first(month),
      Day = first(day),
      `Depth (m)` = mean(Depth_m, na.rm = TRUE),
      `Water Temp (C)` = mean(H2O_TempC, na.rm = TRUE),
      `Air Temp (C)` = mean(AirTemp, na.rm = TRUE),
      PH = mean(PH, na.rm = TRUE),
      `Wind Direction` = mean(WindDir, na.rm = TRUE),
      `Turbidity (NTU)` = mean(Turbidity_ntu, na.rm = TRUE),
      `Specific Conductivity (mScm)` = mean(SpCond_mScm, na.rm = TRUE),
      `Dissolved Oxygen (sat)` = mean(ODO_sat, na.rm = TRUE),
      `Chlorophyll-a (ug/L)` = mean(Chla_ugl, na.rm = TRUE)
    ) |>
    kable() |>
    kable_minimal()
  
#returns the result of the data
  
  return(result)
}

# Call the function

getRiceData()

  


