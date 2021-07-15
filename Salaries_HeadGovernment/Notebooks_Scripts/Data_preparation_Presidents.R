

# General Utilities -------------------------------------------------------

contienent_colors <- c("Americas" = "#00a43f",
                       "Asia" = "#ffd100",
                       "Europe" = "#005eb2", 
                       "Africa" = "#ff6d00",
                       "Oceania" = "#ff001c")

# Data Load and preparation -----------------------------------------------

df <- read.csv("data/Presidents_Ministers_incomes.csv", stringsAsFactors = FALSE, header = TRUE)

df_incomes <- df %>% 
  separate(Head, c("HeadOfState", "Status"), sep = " USD ")

df_incomes <- df_incomes %>% 
  mutate(HeadOfState = as.numeric(str_replace(HeadOfState, ",", "")))

df_incomes <-  df_incomes %>% 
  mutate(Status = str_replace(Status, "(\\[\\d*\\])+", ""),
         Status = str_replace_all(Status, "[\\(\\)]", ""))

df_incomes <- df_incomes %>% 
  separate(Ministers, c("HeadGoverment", "StatusGoverment"), sep = " USD ") %>% 
  mutate(StatusGoverment = str_replace(StatusGoverment, "(\\[\\d*\\])+", ""),
         StatusGoverment = str_replace_all(StatusGoverment, "[\\(\\)]", ""),
         HeadGoverment = as.numeric(str_replace(HeadGoverment, ",", ""))) 

continents <- read.csv("data/Country_Continents.csv", header = TRUE, stringsAsFactors = FALSE)

df_incomes <- df_incomes %>% 
  left_join(continents, by = c("Country"="Entity"))

## Intervals 
bins <- c(-1, 1.0e+03, 1.0e+05, 1.0e+06, 1.0e+012)/1e+05
labels <- c("<1000","1000 - 100000","100000 - 1M", ">=1M")

df_incomes <- df_incomes %>% 
  mutate(HeadOfState_incomes = HeadOfState/1e+05,
         Bins = cut(HeadOfState_incomes, breaks = bins, labels = labels, right = FALSE))


## For maps
world <- map_data("world")

worldSubset <- left_join(world, df_incomes,
                         by = c("region"="Country"))

# Modify the plot format
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

## Interactive Map
dat <- iso3166 %>% 
  rename("iso-a3" = a3)

df_income_countries <- df_incomes %>% 
  mutate(Country = str_replace(Country, "China", "China(?!:Hong Kong|:Macao)"),
         Country = str_replace(Country, "Finland", "Finland(?!:Aland)"),
         Country = str_replace(Country, "^Hong Kong", "China:Hong Kong"),
         Country = str_replace(Country, "Norway", "Norway(?!:Bouvet|:Svalbard|:Jan Mayen)"),
         Country = str_replace(Country, "UK", "UK(?!r)")
  )

df_income_countries <- df_income_countries %>% 
  left_join(dat[,c(2,4)], by = c("Country" = "mapname"))



