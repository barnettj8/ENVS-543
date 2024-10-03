library(tidyverse)
library(knitr)
library(kableExtra)

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSV00nz0KzXglqC23_yLEFB7tNyNM1cTrKw6onP_Egn2HgmigDlTs0JBfDHjvBhZy6U8P5dSHcpAybU/pub?gid=0&single=true&output=csv"

read_csv( url ) -> anna

names( anna )

#anna[is.na(anna)] <- ""

anna |> 
  select ( Date, Site, Temp, `DO (%)`, TSS, `CHLa (pheophytin corrected)`, `TN-N`, `TP-P`) |>
  group_by( Date) |>
  summarise ( `Site Location` = Site,
              `Water Temperature (C)` = mean(Temp, na.rm = TRUE),
              `DO (%)` = mean(`DO (%)`, na.rm = TRUE),
              `TSS (mg/L)` = mean(TSS, na.rm = TRUE),
              `Corrected CHLa (ug/L)` = mean(`CHLa (pheophytin corrected)`, na.rm = TRUE),
              `TN-N` = mean(`TN-N`, na.rm = TRUE),
              `TP-P` = mean(`TP-P`, na.rm = TRUE) ) |>
  kable() |>
  kable_minimal()
