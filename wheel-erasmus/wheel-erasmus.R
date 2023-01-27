library(highcharter)
library(tidyverse)
library(countrycode)

#import data from tidytuesday github
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

countries = c("Germany","France","Italy","Spain","Belgium","United Kingdom","Poland")

data<-df%>%
  #use the countrycode function to convert codes to country names for both receiving and sending countries
  mutate(
    to= countrycode(receiving_country_code, origin="iso2c", destination="iso.name.en"),
    from= countrycode(sending_country_code , origin="iso2c", destination="iso.name.en"),
  )%>%
  #United Kingdom and Greece not translated with ISO country codes, override with dplyr "replace"
  mutate(
    to = replace(to, receiving_country_code=="UK","United Kingdom"),
    from = replace(from, sending_country_code=="UK","United Kingdom"),
    to = replace(to, receiving_country_code=="EL","Greece"),
    from = replace(from, sending_country_code=="EL","Greece")
  )%>%
  #summarise number of participants by sending and receiving country code
  group_by(from, to)%>%
  summarise(weight=sum(participants))%>%
  arrange(-weight)%>%
  ungroup()%>%
  filter(from!=to & from %in% countries & to %in% countries)


data_list <- lapply(split(data,seq(nrow(data))), as.list)


wheel_data = list()
for(i in 1:length(data_list)){
  wheel_data = append(wheel_data,list(data_list[[i]]))
}

pal<-c("#0061fd","#1cc6ff","#00b661","#5bf34d","#ffdd00","#ff7d00","#da2818","#ff006d","#8f00ff","#453435","black","grey80")

my_own_theme <- hc_theme(
  colors = pal,
  chart = list(
    backgroundColor = NULL
  ),
  title = list(
    style = list(
      color = "#333333",
      fontFamily = "Lato",
      fontWeight="bold"
    )
  ),
  subtitle = list(
    style = list(
      color = "#666666",
      fontFamily = "Lato"
    )
  ),
  caption = list(
    style = list(
      color = "#666666",
      fontFamily = "Lato"
    )
  ),
  tooltip = list(
    style = list(
      fontFamily = "Lato"
    )
  ),
  plotOptions = list(
    series = list(
      dataLabels = list(style=list(fontFamily = "Lato")
      ))
  ),
  legend = list(
    itemStyle = list(
      fontFamily = "Lato",
      color = "black"
    ),
    itemHoverStyle = list(
      color = "gray"
    )
  )
)

highchart()%>%
  hc_chart(type = 'dependencywheel') %>%
  hc_add_series(
    data = wheel_data,
    name = "Number of Students"
  )%>%
  hc_title(
    text = "<span style='font-size:15pt;'>ERASMUS STUDENT MOBILITY</span>"
  )%>%
  hc_subtitle(
    text= "Movement of participants in top participating countries between 2014-2020"
  )%>%
  hc_add_theme(my_own_theme)
