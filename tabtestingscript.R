library(tidyverse)
library(knitr)
library(kableExtra)

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR5cQc84ZkekZSLx5SfWwxqAmsEZqKOGljVuG4jPS-N6-8SL5TgSLSSdgLeaozQ9IzVzPJk07L7qkAO/pub?gid=0&single=true&output=csv"

read_csv( url ) -> barfoot

names( barfoot )

barfoot |> 
  select ( ActivityStartDate, `ActivityDepthHeightMeasure/MeasureValue`, MonitoringLocationIdentifier, CharacteristicName, ResultMeasureValue, `ResultMeasure/MeasureUnitCode` ) |>
  group_by( ActivityStartDate ) |>
  summarise ( `Location` = MonitoringLocationIdentifier,
              `Depth` = `ActivityDepthHeightMeasure/MeasureValue`,
              `Parameter` = CharacteristicName,
              `Result` = ResultMeasureValue,
              `Unit` = `ResultMeasure/MeasureUnitCode`,) |>
  kable() |>
  kable_minimal()

#filter -fecal coliform...

barfoot |>
  select( -`ActivityStartTime/Time`, -`ActivityStartTime/TimeZoneCode`, -ActivityEndDate, -`ActivityEndTime/Time`, -`ActivityEndTime/TimeZoneCode`, -LastUpdated)
  
wide_data <- barfoot|>
  pivot_wider(names_from = CharacteristicName, values_from = ResultMeasureValue)

view(wide_data)

names(wide_data)

str(wide_data)

wide_data$`Total suspended solids` <- as.numeric(as.character(wide_data$`Total suspended solids`))
wide_data$`Chlorophyll a, corrected for pheophytin` <- as.numeric(as.character(wide_data$`Chlorophyll a, corrected for pheophytin`))
wide_data$`Temperature, water` <- as.numeric(as.character(wide_data$`Temperature, water`))
wide_data$Turbidity <- as.numeric(as.character(wide_data$Turbidity))
wide_data$`Total solids` <- as.numeric(as.character(wide_data$`Total solids`))
wide_data$Nitrogen <- as.numeric(as.character(wide_data$Nitrogen))
wide_data$`Dissolved oxygen (DO)` <- as.numeric(as.character(wide_data$`Dissolved oxygen (DO)`))

str(wide_data)

wide_data |>
  group_by(ActivityStartDate)|>
  summarise ( 
              `Turbidity (ntu)` = mean(Turbidity, na.rm = TRUE),
              `TSS (mg/L)` = mean(`Total suspended solids`, na.rm = TRUE),
              `CHLa (ug/L)` = mean(`Chlorophyll a, corrected for pheophytin`, na.rm = TRUE),
              `Total solids (mg/L)` = mean(`Total solids`, na.rm = TRUE),
              `Nitrogen (mg/L)` = mean(Nitrogen, na.rm = TRUE),
              `Dissolved Oxygen (%)` = mean(`Dissolved oxygen (DO)`, na.rm = TRUE),
              `Water Temp (C)` = mean(`Temperature, water`, na.rm = TRUE)
              
              ) |>
  
  kable() |>
  kable_minimal()
