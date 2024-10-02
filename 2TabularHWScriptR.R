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
              `Water Temperature (C)` = mean(Temp),
              `DO (%)` = mean(`DO (%)`),
              `TSS (mg/L)` = mean(TSS),
              `Corrected CHLa (ug/L)` = mean(`CHLa (pheophytin corrected)`),
              `TN-N` = mean(`TN-N`),
              `TP-P` = mean(`TP-P`) ) |>
  kable() |>
  kable_minimal()
