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

