library(ggplot2)
library(esquisse)
library(dplyr)
library(ggthemes)
library(extrafont)
library(readr)
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyr)
library(waffle)
library(data.table)
library(rvest)
library(stringr)
library(rlist)

dictionary <- read.csv("dictionary.csv")
summer_data <- read.csv("summer.csv")
winter_data <- read.csv("winter.csv")

################# FUNCTIONS ################# 

### author: Victorp
### source:  https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

change_countries_codes <- function(a) {
  
  a <- a %>%
    
    mutate(Code = recode(Code, "TRI" = "TTO")) %>%
    mutate(Code = recode(Code, "ROM" = "ROU")) %>%
    mutate(Code = recode(Code, "GER" = "DEU")) %>%
    mutate(Code = recode(Code, "POR" = "PRT")) %>%
    mutate(Code = recode(Code, "CHI" = "CHL")) %>%
    mutate(Code = recode(Code, "SIN" = "SGP")) %>%
  
    mutate(Code = recode(Code, "ALG" = "DZA")) %>%
    mutate(Code = recode(Code, "BAH" = "BHS")) %>%
    mutate(Code = recode(Code, "BRN" = "BHR")) %>%
    mutate(Code = recode(Code, "BAR" = "BRB")) %>%
    mutate(Code = recode(Code, "BER" = "BMU")) %>%
    
    mutate(Code = recode(Code, "BOT" = "BWA")) %>%
    mutate(Code = recode(Code, "BUL" = "BGR")) %>%
    mutate(Code = recode(Code, "CRC" = "CRI")) %>%
    mutate(Code = recode(Code, "CRO" = "HRV")) %>%
    mutate(Code = recode(Code, "DEN" = "DNK")) %>%
    
    mutate(Code = recode(Code, "GRE" = "GRC")) %>%
    mutate(Code = recode(Code, "GRN" = "GRD")) %>%
    mutate(Code = recode(Code, "GUA" = "GTM")) %>%
    mutate(Code = recode(Code, "HAI" = "HTI")) %>%
    mutate(Code = recode(Code, "INA" = "IDN")) %>%
    
    mutate(Code = recode(Code, "IRI" = "IRN")) %>%
    mutate(Code = recode(Code, "KUW" = "KWT")) %>%
    mutate(Code = recode(Code, "LAT" = "LVA")) %>%
    mutate(Code = recode(Code, "LIB" = "LBN")) %>%
    mutate(Code = recode(Code, "MAS" = "MYS")) %>%
    
    mutate(Code = recode(Code, "MGL" = "MNG")) %>%
    mutate(Code = recode(Code, "NED" = "NLD")) %>%
    mutate(Code = recode(Code, "AHO" = "ANT")) %>%
    mutate(Code = recode(Code, "NIG" = "NER")) %>%
    mutate(Code = recode(Code, "NGR" = "NGA")) %>%
    
    mutate(Code = recode(Code, "PAR" = "PRY")) %>%
    mutate(Code = recode(Code, "PHI" = "PHL")) %>%
    mutate(Code = recode(Code, "PUR" = "PRI")) %>%
    mutate(Code = recode(Code, "KSA" = "SAU")) %>%
    mutate(Code = recode(Code, "SLO" = "SVN")) %>%
    
    mutate(Code = recode(Code, "SCG" = "SRB")) %>%
    mutate(Code = recode(Code, "RSA" = "ZAF")) %>%
    mutate(Code = recode(Code, "SRI" = "LKA")) %>%
    mutate(Code = recode(Code, "SUD" = "SDN")) %>%
    mutate(Code = recode(Code, "SUI" = "CHE")) %>%
    
    mutate(Code = recode(Code, "TOG" = "TGO")) %>%
    mutate(Code = recode(Code, "TGA" = "TON")) %>%
    mutate(Code = recode(Code, "UAE" = "ARE")) %>%
    mutate(Code = recode(Code, "TPE" = "TWN")) %>%
    mutate(Code = recode(Code, "TAN" = "TZA")) %>%
    
    mutate(Code = recode(Code, "URU" = "URY")) %>%
    mutate(Code = recode(Code, "VIE" = "VNM")) %>%
    mutate(Code = recode(Code, "ISV" = "VIR")) %>%
    mutate(Code = recode(Code, "ZAM" = "ZMB")) %>%
    mutate(Code = recode(Code, "ZIM" = "ZWE")) %>%
    
    mutate(Code = recode(Code, "BUR" = "BFA")) %>%
    mutate(Code = recode(Code, "FIJ" = "FJI"))
  
  return(a)
}

change_countries_names <- function(a) {
  
  a <- a %>%
    
    mutate(Country = recode(Country, "Korea, North" = "North Korea")) %>%
    mutate(Country = recode(Country, "Korea, South" = "South Korea")) %>%
    mutate(Country = recode(Country, "American Samoa*" = "American Samoa")) %>%
    mutate(Country = recode(Country, "Aruba*" = "Aruba")) %>%
    mutate(Country = recode(Country, "Bermuda*" = "Bermuda")) %>%
    mutate(Country = recode(Country, "Cayman Islands*" = "Cayman Islands")) %>%
    mutate(Country = recode(Country, "Hong Kong*" = "Hong Kong")) %>%
    mutate(Country = recode(Country, "Netherlands Antilles*" = "Netherlands Antilles")) %>%
    mutate(Country = recode(Country, "Palestine, Occupied Territories" = "Palestine")) %>%
    mutate(Country = recode(Country, "Puerto Rico*" = "Puerto Rico")) %>%
    mutate(Country = recode(Country, "Virgin Islands*" = "Virgin Islands"))
  
  return(a)
}

################# DICTIONARY #################

dictionary <- change_countries_codes(dictionary)
dictionary <- change_countries_names(dictionary)
dictionary <- dictionary %>%
    bind_rows(c(Country = "Montenegro", Code = "MNE")) %>%
  arrange(Country)

################# SUMMER #################

summer_data <- summer_data %>%
  rename(Code = Country) %>%
  mutate(Code = recode(Code, "EUA" = "DEU")) %>%
  mutate(Code = recode(Code, "FRG" = "DEU")) %>%
  mutate(Code = recode(Code, "GDR" = "DEU")) %>%
  mutate(Code = recode(Code, "RU1" = "RUS")) %>%
  mutate(Code = recode(Code, "URS" = "RUS")) %>%
  mutate(Code = recode(Code, "BOH" = "CZE"))

summer_data <- change_countries_codes(summer_data)

summer_data <- summer_data %>%
  left_join(dictionary, by = "Code") %>%
  select(Country, Code, City, Year, Sport, Discipline, Event, Athlete, Gender, Medal)

summer_data <- summer_data %>% drop_na()

summer_data_distinct <- summer_data %>%
  select(Country, Code, City, Year, Sport, Discipline, Event, Gender, Medal) %>%
  distinct()
  
summer_data_group <- summer_data_distinct %>%
  group_by(Country, Code, Year) %>%
  summarize(Count_overall =  n())

summer_data2 <- summer_data_distinct %>%
  group_by(Country, Code, Year, Medal) %>%
  summarize(Count_by_groups =  n())

summer_data2 <- summer_data2 %>%
  left_join(summer_data_group, by = c("Country", "Code", "Year")) %>%
  mutate(Medals = Medal) %>%
  mutate(Count_by_medal = Count_by_groups) %>%
  pivot_wider(names_from = Medal, values_from = Count_by_groups) %>%
  select(Country, Code, Year, Medals, Count_by_medal, Count_overall, Bronze, Silver, Gold)

rm(summer_data_distinct)
#rm(summer_data_group)

summer_data2$CplusM <- paste(summer_data2$Country, summer_data2$Medals)

summer_data2 <- summer_data2 %>%
  mutate(hover = paste0(Code, "\n",  "Medals: ", Count_overall))

myColors = c("#CD7F32", "#FFD700", "#C0C0C0")

################# WINTER #################

winter_data <- winter_data %>%
  rename(Code = Country) %>%
  mutate(Code = recode(Code, "EUA" = "DEU")) %>%
  mutate(Code = recode(Code, "FRG" = "DEU")) %>%
  mutate(Code = recode(Code, "GDR" = "DEU")) %>%
  mutate(Code = recode(Code, "RU1" = "RUS")) %>%
  mutate(Code = recode(Code, "URS" = "RUS")) %>%
  mutate(Code = recode(Code, "BOH" = "CZE"))
  
winter_data <- change_countries_codes(winter_data)

winter_data <- winter_data %>%
  left_join(dictionary, by = "Code") %>%
  select(Country, Code, City, Year, Sport, Discipline, Event, Athlete, Gender, Medal)

winter_data <- winter_data %>% drop_na()

winter_data_distinct <- winter_data %>%
  select(Country, Code, City, Year, Sport, Discipline, Event, Gender, Medal) %>%
  distinct()

winter_data_group <- winter_data_distinct %>%
  group_by(Country, Code, Year) %>%
  summarize(Count_overall =  n())

winter_data2 <- winter_data_distinct %>%
  group_by(Country, Code, Year, Medal) %>%
  summarize(Count_by_groups =  n())

winter_data2 <- winter_data2 %>%
  left_join(winter_data_group, by = c("Country", "Code", "Year")) %>%
  mutate(Medals = Medal) %>%
  mutate(Count_by_medal = Count_by_groups) %>%
  pivot_wider(names_from = Medal, values_from = Count_by_groups) %>%
  select(Country, Code, Year, Medals, Count_by_medal, Count_overall, Bronze, Silver, Gold)

rm(winter_data_distinct)
#rm(winter_data_group)

winter_data2$CplusM <- paste(winter_data2$Country, winter_data2$Medals)

winter_data2 <- winter_data2 %>%
  mutate(hover = paste0(Code, "\n",  "Medals: ", Count_overall))

###################### WEB SCRAPING SINCE 2014   ###################### 

####### SUMMER ####### 

summer_since_2016 = data.frame()

links = c("rio-2016")

for (k in 1:length(links)){
  
  link = paste0("https://olympics.com/en/olympic-games/", links[k], "/medals")
  page = read_html(link)
  countries = page %>% html_nodes(".dzZgbG") %>% html_text()
  medals = page %>% html_nodes(".Medalstyles__Medal-sc-1tu6huk-1") %>% html_text()
  
  j = 0
  for (i in 1:(length(medals)/4)) {
    
    Country = countries[i]
    Year = str_sub(links[k], start= -4)
    Gold = medals[i+3*j]
    Silver = medals[(i+1)+3*j]
    Bronze = medals[(i+2)+3*j]
    Count_overall = medals[(i+3)+3*j]
    
    summer_since_2016 = rbind(summer_since_2016, data.frame(Country, Year, Gold, Silver, Bronze, Count_overall, stringsAsFactors = FALSE))
    j = j + 1
    
  }
}

link = "https://www.marca.com/en/olympic-games/tokyo/medals"
page = read_html(link)

countries <- page %>% html_nodes(".country strong") %>% html_text() %>% data.frame() 
Country <- countries[-1,]

Year = c(rep(2020, length(countries)))

Gold <- page %>% html_nodes("td.gold") %>% html_text() %>% data.frame()
Gold <- Gold[-1,]

Silver <- page %>% html_nodes("td.silver") %>% html_text() %>% data.frame()
Silver <- Silver[-1,]

Bronze <- page %>% html_nodes("td.bronze") %>% html_text() %>% data.frame()
Bronze <- Bronze[-1,]

Count_overall <- page %>% html_nodes("td.total") %>% html_text() %>% data.frame()
Count_overall <- Count_overall[-1,]

summer_since_2016 = rbind(summer_since_2016, data.frame(Country, Year, Gold, Silver, Bronze, Count_overall, stringsAsFactors = FALSE))

summer_since_2016 <- summer_since_2016 %>%
                      mutate(Country = recode(Country, "United States of America" = "United States" )) %>%
                      mutate(Country = recode(Country, "Great Britain" = "United Kingdom" )) %>%
                      mutate(Country = recode(Country, "People's Republic of China" = "China" )) %>%
                      mutate(Country = recode(Country, "Russian Federation" = "Russia" )) %>%
                      mutate(Country = recode(Country, "Republic of Korea" = "South Korea" )) %>%
                      mutate(Country = recode(Country, "Islamic Republic of Iran" = "Iran" )) %>%
                      mutate(Country = recode(Country, "Democratic People's Republic of Korea" = "South Korea" )) %>%
                      mutate(Country = recode(Country, "Chinese Taipei" = "Taiwan" )) %>%
                      mutate(Country = recode(Country, "Taipei" = "Taiwan" )) %>%
                      mutate(Country = recode(Country, "Ivory Coast" = "Cote d'Ivoire" )) %>%
                      mutate(Country = recode(Country, "Russian Olympic Committee" = "Russia" )) %>%
                      mutate(Country = recode(Country, "The Bahamas" = "Bahamas" )) 
                      
summer_since_2016 <- summer_since_2016 %>%
                      left_join(dictionary, by = "Country") %>%
                      select(Country, Code, Year, Gold, Silver, Bronze, Count_overall) %>%
                      filter(!is.na(Code)) %>%
                      mutate(Gold = recode(Gold, "-" = "0")) %>%
                      mutate(Silver = recode(Silver, "-" = "0")) %>%
                      mutate(Bronze = recode(Bronze, "-" = "0")) %>%
                      mutate(Year = as.double(Year)) %>%
                      mutate(Gold = as.double(Gold)) %>%
                      mutate(Silver = as.double(Silver)) %>%
                      mutate(Bronze = as.double(Bronze)) %>%
                      mutate(Count_overall = as.double(Count_overall)) %>%
                      mutate(hover = paste0(Code, "\n",  "Medals: ", Count_overall))

#write.csv(summer_since_2016, 'summer_since_2016.csv')
summer_since_2016 <- read.csv("summer_since_2016.csv")

####### WINTER ####### 

winter_since_2018 = data.frame()

links = c("pyeongchang-2018")

for (k in 1:length(links)){
  
  link = paste0("https://olympics.com/en/olympic-games/", links[k], "/medals")
  page = read_html(link)
  countries = page %>% html_nodes(".kFXANP") %>% html_text()
  medals = page %>% html_nodes(".Medalstyles__Medal-sc-1tu6huk-1") %>% html_text()
  
  j = 0
  for (i in 1:(length(medals)/4)) {
    
    Country = countries[i]
    Year = str_sub(links[k], start= -4)
    Gold = medals[i+3*j]
    Silver = medals[(i+1)+3*j]
    Bronze = medals[(i+2)+3*j]
    Count_overall = medals[(i+3)+3*j]
    
    winter_since_2018 = rbind(winter_since_2018, data.frame(Country, Year, Gold, Silver, Bronze, Count_overall, stringsAsFactors = FALSE))
    j = j + 1
    
  }
}

winter_since_2018 <- winter_since_2018 %>%
  mutate(Country = recode(Country, "United States of America" = "United States" )) %>%
  mutate(Country = recode(Country, "Great Britain" = "United Kingdom" )) %>%
  mutate(Country = recode(Country, "People's Republic of China" = "China" )) %>%
  mutate(Country = recode(Country, "Russian Federation" = "Russia" )) %>%
  mutate(Country = recode(Country, "Republic of Korea" = "South Korea" )) %>%
  mutate(Country = recode(Country, "Olympic Athletes from Russia" = "Russia" )) 

winter_since_2018 <- winter_since_2018 %>%
                      left_join(dictionary, by = "Country") %>%
                      select(Country, Code, Year, Gold, Silver, Bronze, Count_overall) %>%
                      filter(!is.na(Code)) %>%
                      mutate(Gold = recode(Gold, "-" = "0")) %>%
                      mutate(Silver = recode(Silver, "-" = "0")) %>%
                      mutate(Bronze = recode(Bronze, "-" = "0")) %>%
                      mutate(Year = as.double(Year)) %>%
                      mutate(Gold = as.double(Gold)) %>%
                      mutate(Silver = as.double(Silver)) %>%
                      mutate(Bronze = as.double(Bronze)) %>%
                      mutate(Count_overall = as.double(Count_overall)) %>%
                      mutate(hover = paste0(Code, "\n",  "Medals: ", Count_overall))

#write.csv(winter_since_2018, 'winter_since_2018.csv')
winter_since_2018 <- read.csv("winter_since_2018.csv")

###################### GRAPHS ###################### 

####### MAPS ####### 

graph_properties <- list(showframe = FALSE, showcoastlines = TRUE, projection = list(type = 'Mercator'), showland = TRUE, 
                         landcolor = toRGB("white"), color = toRGB("white"))
font = list(family = "Verdana", size = 10, color = "white")
label = list(bgcolor = "#EEEEEE", bordercolor = "transparent", font = font)
l <- list(color = toRGB("grey"), width = 0.5, font = font)
m <- list(l = 100, r = 100, b = 200, t = 200, pad = 25)

####### SUMMER MAP ####### 

summer_for_plots <- bind_rows(summer_data2, summer_since_2016)

summer_map <- plot_geo(summer_for_plots, frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Count_overall,
            zmin = 0,
            zmax = max(summer_for_plots$Count_overall),
            color = ~Count_overall,
            colorscale = "Portland",
            text = ~hover,
            hoverinfo = 'text') %>%
  layout(geo = graph_properties,
         title = "Olympics Summer Total Medals \n1896 - 2020",
         font = list(family = "Verdana")) %>%
  config(displayModeBar = FALSE) %>%
  style(hoverlabel = l)

summer_map <- summer_map %>% layout(autosize = T, height=800, width = 1175, margin = m) %>% 
  colorbar(title = "Medals")

####### WINTER MAP ####### 

winter_for_plots <- bind_rows(winter_data2, winter_since_2018)

winter_map <- plot_geo(winter_for_plots, frame = ~Year) %>%
  add_trace(locations = ~Code,
            z = ~Count_overall,
            zmin = 0,
            zmax = max(winter_for_plots$Count_overall),
            color = ~Count_overall,
            colorscale = "Portland",
            text = ~hover,
            hoverinfo = 'text') %>%
  layout(geo = graph_properties,
         title = "Olympics Winter Total Medals \n1924 - 2018",
         font = list(family = "Verdana")) %>%
  config(displayModeBar = FALSE) %>%
  style(hoverlabel = l)

winter_map <- winter_map %>% layout(autosize = T, height=800, width = 1175, margin = m) %>% 
  colorbar(title = "Medals")

###### GRAPH TOP10 SUMMER ######

summer_total_medals <- summer_data2 %>%
  data.frame() %>%
  select(Country, Bronze, Silver, Gold) %>%
  rowwise() %>%
  mutate(Bronze = sum(Bronze, na.rm = TRUE)) %>%
  mutate(Silver = sum(Silver, na.rm = TRUE)) %>%
  mutate(Gold = sum(Gold, na.rm = TRUE)) %>%
  mutate(Medals = sum(Bronze, Silver, Gold, na.rm = TRUE)) %>%
  select(Country, Medals, Bronze, Silver, Gold) %>%
  group_by(Country) %>%
  summarize(Medals_tot = sum(Medals), Bronze = sum(Bronze), Silver = sum(Silver), Gold = sum(Gold)) %>%
  select(Country, Medals_tot, Gold, Silver, Bronze) %>%
  arrange(desc(Medals_tot))

summer_total_medals <- bind_rows(summer_total_medals, summer_since_2016) %>%
                        select(Country, Gold, Silver, Bronze, Count_overall, Medals_tot) %>%
                        group_by(Country) %>%
                        summarize(Medals_total = sum(Medals_tot, na.rm = TRUE) + sum(Count_overall, na.rm = TRUE), Bronze = sum(Bronze), 
                                  Silver = sum(Silver), Gold = sum(Gold)) %>%
                        select(Country, Medals_total, Gold, Silver, Bronze) %>%
                        arrange(desc(Medals_total))

summer_total_medals2 <- slice_head(summer_total_medals, n = 10)
summer_total_medals2 <- summer_total_medals2 %>% pivot_longer(3:5, names_to="Medal", values_to="Medals_by_place")

###### GRAPH WAFFLE SUMMER ######

perc_s <- summer_total_medals %>%
  mutate(Medals_top_perc = (Medals_total/sum(Medals_total))) %>%
  mutate(Gold_perc = (Gold/sum(Gold))) %>%
  mutate(Silver_perc = (Silver/sum(Silver))) %>%
  mutate(Bronze_perc = (Bronze/sum(Bronze))) %>%
  select(Country, Medals_total, Medals_top_perc, Gold, Gold_perc, Silver, Silver_perc, Bronze, Bronze_perc)

###### GRAPH TOP10 WINTER ######

winter_total_medals <- winter_data2 %>%
  data.frame() %>%
  select(Country, Bronze, Silver, Gold) %>%
  rowwise() %>%
  mutate(Bronze = sum(Bronze, na.rm = TRUE)) %>%
  mutate(Silver = sum(Silver, na.rm = TRUE)) %>%
  mutate(Gold = sum(Gold, na.rm = TRUE)) %>%
  mutate(Medals = sum(Bronze, Silver, Gold, na.rm = TRUE)) %>%
  select(Country, Medals, Bronze, Silver, Gold) %>%
  group_by(Country) %>%
  summarize(Medals_tot = sum(Medals), Bronze = sum(Bronze), Silver = sum(Silver), Gold = sum(Gold)) %>%
  select(Country, Medals_tot, Gold, Silver, Bronze) %>%
  arrange(desc(Medals_tot))

winter_total_medals <- bind_rows(winter_total_medals, winter_since_2018) %>%
  select(Country, Gold, Silver, Bronze, Count_overall, Medals_tot) %>%
  group_by(Country) %>%
  summarize(Medals_total = sum(Medals_tot, na.rm = TRUE) + sum(Count_overall, na.rm = TRUE), Bronze = sum(Bronze), 
            Silver = sum(Silver), Gold = sum(Gold)) %>%
  select(Country, Medals_total, Gold, Silver, Bronze) %>%
  arrange(desc(Medals_total))

winter_total_medals2 <- slice_head(winter_total_medals, n = 10)
winter_total_medals2 <- winter_total_medals2 %>% pivot_longer(3:5, names_to="Medal", values_to="Medals_by_place")

###### GRAPH WAFFLE WINTER ######

perc_w <- winter_total_medals %>%
  mutate(Medals_top_perc = (Medals_total/sum(Medals_total))) %>%
  mutate(Gold_perc = (Gold/sum(Gold))) %>%
  mutate(Silver_perc = (Silver/sum(Silver))) %>%
  mutate(Bronze_perc = (Bronze/sum(Bronze))) %>%
  select(Country, Medals_total, Medals_top_perc, Gold, Gold_perc, Silver, Silver_perc, Bronze, Bronze_perc)

###### GRAPH COUNTRIES-OLYMPIANS SUMMER ######

olympics1_s <- summer_data %>%
  group_by(Country, Discipline, Athlete) %>%
  summarize(Count_overall =  n())

olympics2_s <- summer_data %>%
  group_by(Country, Discipline, Athlete, Medal) %>%
  summarize(Count_by_groups =  n())

olympics_s <- olympics1_s %>%
  left_join(olympics2_s, by = c("Country", "Discipline", "Athlete")) %>%
  mutate(Medals = Medal) %>%
  mutate(Count_by_medal = Count_by_groups) %>%
  pivot_wider(names_from = Medal, values_from = Count_by_groups) %>%
  select(Country, Discipline, Athlete, Medals, Count_overall, Bronze, Silver, Gold)

top_olympian_s <- olympics_s %>%
  data.frame() %>%
  select(Country, Discipline, Athlete, Bronze, Silver, Gold) %>%
  rowwise() %>%
  mutate(Bronze = sum(Bronze, na.rm = TRUE)) %>%
  mutate(Silver = sum(Silver, na.rm = TRUE)) %>%
  mutate(Gold = sum(Gold, na.rm = TRUE)) %>%
  mutate(Medals = sum(Bronze, Silver, Gold, na.rm = TRUE)) %>%
  select(Country, Discipline, Athlete, Medals, Bronze, Silver, Gold) %>%
  group_by(Country, Discipline, Athlete) %>%
  summarize(Medals_tot = sum(Medals), Bronze = sum(Bronze), Silver = sum(Silver), Gold = sum(Gold)) %>%
  select(Country, Discipline, Athlete, Medals_tot, Gold, Silver, Bronze) %>%
  mutate(AthleteAccCountry = paste(str_extract(Athlete, "(^[A-ZÄÖÜÁÉ -']{2,})"), Country)) %>%
  arrange(desc(Medals_tot)) %>%
  data.frame()

###### GRAPH COUNTRIES-OLYMPIANS WINTER ######

olympics1_w <- winter_data %>%
  group_by(Country, Discipline, Athlete) %>%
  summarize(Count_overall =  n())

olympics2_w <- winter_data %>%
  group_by(Country, Discipline, Athlete, Medal) %>%
  summarize(Count_by_groups =  n())

olympics_w <- olympics1_w %>%
  left_join(olympics2_w, by = c("Country", "Discipline", "Athlete")) %>%
  mutate(Medals = Medal) %>%
  mutate(Count_by_medal = Count_by_groups) %>%
  pivot_wider(names_from = Medal, values_from = Count_by_groups) %>%
  select(Country, Discipline, Athlete, Medals, Count_overall, Bronze, Silver, Gold)

top_olympian_w <- olympics_w %>%
  data.frame() %>%
  select(Country, Discipline, Athlete, Bronze, Silver, Gold) %>%
  rowwise() %>%
  mutate(Bronze = sum(Bronze, na.rm = TRUE)) %>%
  mutate(Silver = sum(Silver, na.rm = TRUE)) %>%
  mutate(Gold = sum(Gold, na.rm = TRUE)) %>%
  mutate(Medals = sum(Bronze, Silver, Gold, na.rm = TRUE)) %>%
  select(Country, Discipline, Athlete, Medals, Bronze, Silver, Gold) %>%
  group_by(Country, Discipline, Athlete) %>%
  summarize(Medals_tot = sum(Medals), Bronze = sum(Bronze), Silver = sum(Silver), Gold = sum(Gold)) %>%
  select(Country, Discipline, Athlete, Medals_tot, Gold, Silver, Bronze) %>%
  mutate(AthleteAccCountry = paste(str_extract(Athlete, "(^[A-ZÄÖÜÁÉ -']{2,})"), Country)) %>%
  arrange(desc(Medals_tot)) %>%
  data.frame()

###################### SHINY ###################### 

ui <- dashboardPage(
  dashboardHeader(title = "Olympics through time"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("home")), #dashboard, th
      menuItem("Olympics over time", tabName = "calendar", icon = icon("calendar-day")), 
      menuItem("Summer map", tabName = "summermap", icon = icon("globe-africa")),
      menuItem("Winter map", tabName = "wintermap", icon = icon("globe-americas")),
      menuItem("Countries", tabName = "countries", icon = icon("trophy")),
      menuItem("Gold Silver Bronze", tabName = "gsb", icon = icon("trophy")),
      menuItem("Gold Silver Bronze Summer", tabName = "medals_summer", icon = icon("award")),
      menuItem("Gold Silver Bronze Winter", tabName = "medals_winter", icon = icon("award")), 
      menuItem("Gender", tabName = "gender", icon = icon("venus-mars")),
      menuItem("Top Olympians", tabName = "olympians", icon = icon("medal")),
      menuItem("DataTables", tabName = "datatables", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
                
        fluidRow(
          box(title = "Introduction", width = 4, solidHeader = TRUE, status = "primary", height = 390,
              h4(strong("This project aim is to check the countries’ achievements in Summer Olympics and Winter Olympics through the time."), align = "justify", style="line-height: 1.3"),
              br(),
              h4("The original data consisted of Summer Olympics (1896-2012) and Winter Olympics (1924-2014). 
                  The latest Olympics 2016 and 2020 for Summer Olympics and 2014 for Winter Olympics data were scraped 
                  as sums of medals (bronzes, silvers, golds, total), so the sportsmen’s names were not included.", align = "justify", style="line-height: 1.2")),
          box(title = "Top 10 summer countries", width = 8, solidHeader = TRUE, status = "primary", plotOutput("top10_s", height = 330), height = 390)),

        fluidRow(
          box(title = "Simplifications", width = 4, solidHeader = TRUE, status = "primary", height = 390,
              h4("- Historical countries names were changed into current ones (e.g. Russia, Germany)"),
              h4("- Some countries were omitted due to a problem with athletes classification (Yugoslavia)"),
              h4("- Gender-mixed disciplines (like relay race) were treated as different activities due to a problem with defining all those disciplines"),
              h4("- The wage for medals colours were not used, places were treated equally")),
          box(title = "Top 10 winter countries", width = 8, solidHeader = TRUE, status = "primary", plotOutput("top10_w", height = 330), height = 390)),
      ),
      
      tabItem(tabName = "calendar",
              
              fluidRow(
                box(title = "Summer Olympics over time", width = 6, solidHeader = TRUE, status = "primary", height = 200,
                    
                  tags$style(".container { width: 100%; height: 180px; overflow-y: scroll; }"),
                  dropdownButton(
                    label = "Choose summer countries:", status = "default",
                    tags$div(
                      class = "container",
                      checkboxGroupInput("calendar_s_input", "Countries:", choices = sort(unique(summer_data2$Country))),
                    )
                  ), verbatimTextOutput(outputId = "calendar_s_text")),
                    
                box(title = "Winter Olympics over time", width = 6, solidHeader = TRUE, status = "primary", height = 200, 

                  tags$style(".container { width: 100%; height: 180px; overflow-y: scroll; }"),
                  dropdownButton(
                    label = "Choose winter countries:", status = "default",
                    tags$div(
                      class = "container",
                      checkboxGroupInput("calendar_w_input", "Countries:", choices = sort(unique(winter_data2$Country))),
                    )
                  ), verbatimTextOutput(outputId = "calendar_w_text"))),
                    
              fluidRow(
                box(title = "Summer countries", width = 6, solidHeader = TRUE, status = "primary", plotOutput("calendar_s", height = 470), height = 550),
                box(title = "Winter countries", width = 6, solidHeader = TRUE, status = "primary", plotOutput("calendar_w", height = 470), height = 550)),
      ),
      
      tabItem(tabName = "summermap",  width = 12, height = 900, width = 1200, summer_map),
      tabItem(tabName = "wintermap",  width = 12, height = 900, width = 1200, winter_map),

      tabItem(tabName = "countries",
          
              fluidRow(
                box(title = "Summer", width = 4, solidHeader = TRUE, status = "primary", height = 350,
                    
                  tags$style(".container { width: 100%; height: 240px; overflow-y: scroll; }"),
                  dropdownButton(
                    label = "Choose summer countries:", status = "default",
                    tags$div(
                      class = "container",
                      checkboxGroupInput("countries_year_s_input", "Countries:", choices = sort(unique(summer_data2$Country))),
                    )
                ), verbatimTextOutput(outputId = "countries_s")),
                
                box(title = "Summer countries", width = 8, solidHeader = TRUE, status = "primary", plotOutput("countries_year_s", height = 330), height = 390)),
              
              fluidRow(
                box(title = "Winter", width = 4, solidHeader = TRUE, status = "primary", height = 350,
                    
                  tags$style(".container { width: 100%; height: 240px; overflow-y: scroll; }"),
                  dropdownButton(
                    label = "Choose winter countries:", status = "default",
                    tags$div(
                      class = "container",
                      checkboxGroupInput("countries_year_w_input", "Countries:", choices = sort(unique(winter_data2$Country))), 
                    )
                  ), verbatimTextOutput(outputId = "countries_w")),
                
                box(title = "Winter countries", width = 8, solidHeader = TRUE, status = "primary", plotOutput("countries_year_w", height = 330), height = 390)),
      ),
      
      tabItem(tabName = "gsb",
              
              fluidRow(
                box(title = "Summer", width = 4, solidHeader = TRUE, status = "primary", height = 350,
                    selectInput("gsb_s_input", "Country:", choices=sort(unique(summer_data2$Country)))),
                
                box(title = "Summer countries", width = 8, solidHeader = TRUE, status = "primary", plotOutput("gsb_s", height = 330), height = 390)),
              
              fluidRow(
                box(title = "Winter", width = 4, solidHeader = TRUE, status = "primary", height = 350,
                    selectInput("gsb_w_input", "Country:", choices=sort(unique(winter_data2$Country)))),

                box(title = "Winter countries", width = 8, solidHeader = TRUE, status = "primary", plotOutput("gsb_w", height = 330), height = 390)),
      ),
      
      tabItem(tabName = "medals_summer",
              
              fluidRow(
                box(title = "Top countries according to Discipline", width = 8, solidHeader = TRUE, status = "primary", height = 415,
                    selectInput("top_countries_discipline_s_input", "Discipline:", choices=sort(unique(summer_data$Discipline))),
                    plotOutput("top_countries_discipline_s", height = 275)),
                
                box(title = "Total medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("tot_s", height = 350), height = 415)),
              
              fluidRow(
                box(title = "Gold medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("gold_s", height = 300), height = 360),
                box(title = "Silver medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("silver_s", height = 300), height = 360),
                box(title = "Bronze medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("bronze_s", height = 300), height = 360)),
      ),
      
      tabItem(tabName = "medals_winter",
              
              fluidRow(
                box(title = "Top countries according to Discipline", width = 8, solidHeader = TRUE, status = "primary", height = 415,
                    selectInput("top_countries_discipline_w_input", "Discipline:", choices=sort(unique(winter_data$Discipline))),
                    plotOutput("top_countries_discipline_w", height = 275)),
                
                box(title = "Total medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("tot_w", height = 350), height = 415)),
              
              fluidRow(
                box(title = "Gold medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("gold_w", height = 300), height = 360),
                box(title = "Silver medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("silver_w", height = 300), height = 360),
                box(title = "Bronze medals", width = 4, solidHeader = TRUE, status = "primary", plotOutput("bronze_w", height = 300), height = 360)),
      ),
      
      tabItem(tabName = "gender",
              
        fluidRow(
          box(title = "Summer medals acc. to gender", width = 6, solidHeader = TRUE, status = "primary", 
              selectInput("gender_s_input", "Country:", choices=sort(unique(summer_data2$Country))), height = 200),
       
        box(title = "Winter medals acc. to gender", width = 6, solidHeader = TRUE, status = "primary", 
            selectInput("gender_w_input", "Country:", choices=sort(unique(winter_data2$Country))), height = 200)),
        
        fluidRow(
          box(title = "Gender summer ranking", width = 6, solidHeader = TRUE, status = "primary", plotOutput("gender_s", height = 470), height = 550),
          box(title = "Gender winter ranking", width = 6, solidHeader = TRUE, status = "primary", plotOutput("gender_w", height = 470), height = 550)),
      ),
    
    tabItem(tabName = "olympians",
            
        fluidRow(
          box(title = "Top 5 summer Olympians", width = 4, solidHeader = TRUE, status = "primary", 
              selectInput("top_olympian_s_input", "Discipline:", choices=sort(unique(summer_data$Discipline))), height = 200),
      
          box(title = "Top summer Olympians", width = 8, solidHeader = TRUE, status = "primary", plotOutput("top_olympian_s_plot", height = 330), height = 390)),
        
        fluidRow(
          box(title = "Top 5 winter Olympians", width = 4, solidHeader = TRUE, status = "primary", 
            selectInput("top_olympian_w_input", "Discipline:", choices=sort(unique(winter_data$Discipline))), height = 200),
    
          box(title = "Top winter Olympians", width = 8, solidHeader = TRUE, status = "primary", plotOutput("top_olympian_w_plot", height = 330), height = 390)),
    ),
      
      tabItem(tabName = "datatables",
        fluidPage(
          titlePanel("Olympics DataTables"),
          
          fluidRow(
            tabBox(title = "Olympics from 1896 to 2014", width = 12,
              tabPanel("Summer",
                  column(2, selectInput("country_s", "Country:", c("All", sort(unique(as.character(summer_data$Country)))))),
                  column(2, selectInput("year_s", "Year:", c("All", sort(unique(as.character(summer_data$Year)))))),
                  column(2, selectInput("discipline_s", "Discipline:", c("All", sort(unique(as.character(summer_data$Discipline)))))),
                  column(2, selectInput("gender_s", "Gender:", c("All", sort(unique(as.character(summer_data$Gender)))))),
                  column(2, selectInput("athlete_s", "Athlete:", c("All", sort(unique(as.character(summer_data$Athlete)))))),
                  column(2, selectInput("medal_s", "Medal:", c("All", sort(unique(as.character(summer_data$Medal)))))),
                  DT::dataTableOutput("summer")
              ),
              tabPanel("Winter",
                 column(2, selectInput("country_w", "Country:", c("All", sort(unique(as.character(winter_data$Country)))))),
                 column(2, selectInput("year_w", "Year:", c("All", sort(unique(as.character(winter_data$Year)))))),
                 column(2, selectInput("discipline_w", "Discipline:", c("All", sort(unique(as.character(winter_data$Discipline)))))),
                 column(2, selectInput("gender_w", "Gender:", c("All", sort(unique(as.character(winter_data$Gender)))))),
                 column(2, selectInput("athlete_w", "Athlete:", c("All", sort(unique(as.character(winter_data$Athlete)))))),
                 column(2, selectInput("medal_w", "Medal:", c("All", sort(unique(as.character(winter_data$Medal)))))),
                 DT::dataTableOutput("winter")
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {

  output$top10_s <- renderPlot({ 
    
    top10_s <- ggplot(summer_total_medals2) +
      aes(x = reorder(Country, -Medals_by_place, sum), fill = Medal, weight = Medals_by_place) +
      geom_bar() +
      scale_fill_manual(values = c(Bronze = "#CD7F32", Gold = "#FFD700", Silver = "#C0C0C0")) +
      labs(title = "Medals over summer olympics",
           subtitle = "How many medals were gained by top 10 country?",
           x = "",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(axis.text = element_text(size = 9))
    
    top10_s 
  })
  
  output$top10_w <- renderPlot({ 
    
    top10_w <- ggplot(winter_total_medals2) +
      aes(x = reorder(Country, -Medals_by_place, sum), fill = Medal, weight = Medals_by_place) +
      geom_bar() +
      scale_fill_manual(values = c(Bronze = "#CD7F32", Gold = "#FFD700", Silver = "#C0C0C0")) +
      labs(title = "Medals over winter olympics",
           subtitle = "How many medals were gained by top 10 country?",
           x = "",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(axis.text = element_text(size = 9))
    
    top10_w 
  })
  
  output$tot_s <- renderPlot({ 
    
    tot_s <- perc_s %>% filter(Medals_top_perc > 0.01) %>% select(Country, Medals_top_perc)
    tot_s <- slice_head(tot_s, n = 9) %>% add_row(Country = "Others", Medals_top_perc = (1-sum(tot_s$Medals_top_perc[1:9])))
    parts <- data.frame(names = tot_s$Country, vals = ceiling(tot_s$Medals_top_perc*100))
    tot_s <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                               "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    tot_s
  })
  
  output$gold_s <- renderPlot({ 
  
    gold_s <- perc_s %>% filter(Gold_perc > 0.01) %>% select(Country, Gold_perc)
    gold_s <- slice_head(gold_s, n = 9) %>% add_row(Country = "Others", Gold_perc = (1-sum(gold_s$Gold_perc[1:9])))
    parts <- data.frame(names = gold_s$Country, vals = ceiling(gold_s$Gold_perc*100))
    gold_s <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                                "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    gold_s 
  })
  
  output$silver_s <- renderPlot({ 
    
    silver_s <- perc_s %>% filter(Silver_perc > 0.01) %>% select(Country, Silver_perc)
    silver_s <- slice_head(silver_s, n = 9) %>% add_row(Country = "Others", Silver_perc = (1-sum(silver_s$Silver_perc[1:9])))
    parts <- data.frame(names = silver_s$Country, vals = ceiling(silver_s$Silver_perc*100))
    silver_s <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                                  "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    silver_s 
  })
  
  output$bronze_s <- renderPlot({ 
    
    bronze_s <- perc_s %>% filter(Bronze_perc > 0.01) %>% select(Country, Bronze_perc)
    bronze_s <- slice_head(bronze_s, n = 9) %>% add_row(Country = "Others", Bronze_perc = (1-sum(bronze_s$Bronze_perc[1:9])))
    parts <- data.frame(names = bronze_s$Country, vals = ceiling(bronze_s$Bronze_perc*100))
    bronze_s <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                                  "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    bronze_s 
  })
  
  output$tot_w <- renderPlot({ 
    
    tot_w <- perc_w %>% filter(Medals_top_perc > 0.01) %>% select(Country, Medals_top_perc)
    tot_w <- slice_head(tot_w, n = 9) %>% add_row(Country = "Others", Medals_top_perc = (1-sum(tot_w$Medals_top_perc[1:9])))
    parts <- data.frame(names = tot_w$Country, vals = ceiling(tot_w$Medals_top_perc*100))
    tot_w <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                                 "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    tot_w
  })
  
  output$gold_w <- renderPlot({ 
    
    gold_w <- perc_w %>% filter(Gold_perc > 0.01) %>% select(Country, Gold_perc)
    gold_w <- slice_head(gold_w, n = 9) %>% add_row(Country = "Others", Gold_perc = (1-sum(gold_w$Gold_perc[1:9])))
    parts <- data.frame(names = gold_w$Country, vals = ceiling(gold_w$Gold_perc*100))
    gold_w <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                                  "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    gold_w 
  })
  
  output$silver_w <- renderPlot({ 
    
    silver_w <- perc_w %>% filter(Silver_perc > 0.01) %>% select(Country, Silver_perc)
    silver_w <- slice_head(silver_w, n = 9) %>% add_row(Country = "Others", Silver_perc = (1-sum(silver_w$Silver_perc[1:9])))
    parts <- data.frame(names = silver_w$Country, vals = ceiling(silver_w$Silver_perc*100))
    silver_w <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                                    "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    silver_w 
  })
  
  output$bronze_w <- renderPlot({ 
    
    bronze_w <- perc_w %>% filter(Bronze_perc > 0.01) %>% select(Country, Bronze_perc)
    bronze_w <- slice_head(bronze_w, n = 9) %>% add_row(Country = "Others", Bronze_perc = (1-sum(bronze_w$Bronze_perc[1:9])))
    parts <- data.frame(names = bronze_w$Country, vals = ceiling(bronze_w$Bronze_perc*100))
    bronze_w <- waffle(parts, rows = 10, colors = c("#F29E4C", "#F1C453", "#EFEA5A", "#B9E769", "#83E377",
                                                    "#16DB93", "#0DB39E", "#048BA8", "#2C699A", "#54478C"))
    bronze_w 
  })
  
  output$top_countries_discipline_s <- renderPlot({
    
    countries_discipline <- top_olympian_s %>% select(Country, Discipline, Medals_tot, Gold, Silver, Bronze) %>%
      group_by(Country, Discipline) %>%
      summarise(Medals_tot = sum(Medals_tot), Gold = sum(Gold), Silver = sum(Silver), Bronze = sum(Bronze)) %>%
      arrange(desc(Medals_tot)) %>%
      data.frame()
    
    top_ol <- input$top_countries_discipline_s_input
    top_olympian_data <- subset(countries_discipline, Discipline == top_ol)
    top_olympian_data <- slice_head(top_olympian_data, n = 5)
    top_olympian_data <- top_olympian_data %>% pivot_longer(4:6, names_to="Medal", values_to="Medals_by_place")
    
    top_olympian_plot <- ggplot(top_olympian_data) +
      aes(x = reorder(Country, -Medals_by_place, sum), fill = Medal, weight = Medals_by_place) +
      geom_bar() +
      scale_fill_manual(values = c(Bronze = "#CD7F32", Gold = "#FFD700", Silver = "#C0C0C0")) +
      labs(title = "Top 5 summer countries",
           subtitle = "How many medals were gained by countries in given discipline?",
           x = "",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(axis.text = element_text(size = 9))
    
    top_olympian_plot
    
  })
  
  output$top_countries_discipline_w <- renderPlot({
    
    countries_discipline <- top_olympian_w %>% select(Country, Discipline, Medals_tot, Gold, Silver, Bronze) %>%
      group_by(Country, Discipline) %>%
      summarise(Medals_tot = sum(Medals_tot), Gold = sum(Gold), Silver = sum(Silver), Bronze = sum(Bronze)) %>%
      arrange(desc(Medals_tot)) %>%
      data.frame()
    
    top_ol <- input$top_countries_discipline_w_input
    top_olympian_data <- subset(countries_discipline, Discipline == top_ol)
    top_olympian_data <- slice_head(top_olympian_data, n = 5)
    top_olympian_data <- top_olympian_data %>% pivot_longer(4:6, names_to="Medal", values_to="Medals_by_place")
    
    top_olympian_plot <- ggplot(top_olympian_data) +
      aes(x = reorder(Country, -Medals_by_place, sum), fill = Medal, weight = Medals_by_place) +
      geom_bar() +
      scale_fill_manual(values = c(Bronze = "#CD7F32", Gold = "#FFD700", Silver = "#C0C0C0")) +
      labs(title = "Top 5 summer countries",
           subtitle = "How many medals were gained by countries in given discipline?",
           x = "",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(axis.text = element_text(size = 9))
    
    top_olympian_plot
    
  })
  
  output$countries_s <- renderText({input$countries_year_s_input })
  
  output$countries_year_s <- renderPlot({
    
    b <- input$countries_year_s_input
    
    countries <- summer_for_plots %>%
      filter (Country %in% b) %>%
      ggplot(aes(x = Year, y = Count_overall, col=Code)) +
      geom_line(size = 1.0, alpha = 0.8) + 
      labs(title = "Medals over summer olympics",
           subtitle = "How many medals were gained by countries?",
           x = "Year",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      scale_linetype_manual() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    
    countries
  })
  
  output$countries_w <- renderText({ input$countries_year_w_input })
  
  output$countries_year_w <- renderPlot({
    
    b <- input$countries_year_w_input
    
    countries <- winter_for_plots %>%
      filter (Country %in% b) %>%
      ggplot(aes(x = Year, y = Count_overall, col=Code)) +
      geom_line(size = 1.0, alpha = 0.8) + 
      labs(title = "Medals over winter olympics",
           subtitle = "How many medals were gained by countries?",
           x = "Year",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      scale_linetype_manual() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    
    countries
  })
  
  output$calendar_s_text <- renderText({ input$calendar_s_input })
  
  output$calendar_s <- renderPlot ({ 
    
    summer_data_group2 <- summer_since_2016 %>%
      bind_rows(summer_data_group, summer_since_2016) %>%
      select(Country, Year, Count_overall) %>%
      arrange(Country)
    
    b <- input$calendar_s_input
    
    if (is.null(b)){
      b <- c(sort(unique(summer_data_group2$Country))[1])
    }
    
    summer_data_group2 <- summer_data_group2 %>%
      filter(Country %in% b)
    
    summer_heatmap <- ggplot(summer_data_group2, aes(Year, Country, fill= Count_overall)) + 
      geom_tile() +
      scale_fill_distiller(palette = "Spectral") + 
      labs(fill = "Medals") + 
      scale_y_discrete(limits=rev) +
      labs(title = "Medals over summer olympics",
           subtitle = "How many medals were gained by countries?",
           x = "Year",
           y = "Country") +
      theme_fivethirtyeight() +
      scale_linetype_manual() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(panel.grid.major = element_line(linetype = "longdash"))
    
    summer_heatmap
  })
  
  output$calendar_w_text <- renderText({ input$calendar_w_input })
  
  output$calendar_w <- renderPlot ({ 
    
    winter_data_group2 <- winter_since_2018 %>%
      bind_rows(winter_data_group, winter_since_2018) %>%
      select(Country, Year, Count_overall) %>%
      arrange(Country)
    
    b <- input$calendar_w_input
    
    if (is.null(b)){
      b <- c(sort(unique(winter_data2$Country))[1])
    }
    
    winter_data_group2 <- winter_data_group2 %>%
      filter(Country %in% b)
    
    winter_heatmap <- ggplot(winter_data_group2, aes(Year, Country, fill= Count_overall)) + 
      geom_tile() +
      scale_fill_distiller(palette = "Spectral") + 
      labs(fill = "Medals") + 
      scale_y_discrete(limits=rev) +
      labs(title = "Medals over winter olympics",
           subtitle = "How many medals were gained by countries?",
           x = "Year",
           y = "Country") +
      theme_fivethirtyeight() +
      scale_linetype_manual() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(panel.grid.major = element_line(linetype = "longdash"))
    
    winter_heatmap
  })
  
  output$gsb_s <- renderPlot({ 
    
    country <- input$gsb_s_input

    summer_for_plots2 <- summer_for_plots %>% 
      filter(Year %in% c(2016, 2020)) %>%
      select(Country, Code, Year, Bronze, Silver, Gold) %>%
      pivot_longer(4:6, names_to="Medals", values_to="Count_by_medal") %>%
      select(Country, Code, Year, Medals, Count_by_medal) %>%
      mutate(CplusM = paste(Country, Medals)) %>%
      data.frame()
    
    summer_for_plots2 <- bind_rows(summer_for_plots, summer_for_plots2) %>%
      select(Country, Code, Year, Medals, Count_by_medal, CplusM)
    
    summer_plot <- summer_for_plots2[summer_for_plots2$CplusM %like% country,]
    
    summer_plot <- summer_plot %>%
      ggplot(aes(x = Year, y = Count_by_medal, col=CplusM)) +
      geom_line(size = 1.0, alpha = 0.8) + 
      labs(title = "Medals over summer olympics",
           subtitle = "How many medals were gained by country?",
           x = "Year",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      scale_linetype_manual() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) +
      scale_color_manual(values = myColors)
    
    summer_plot
  })
  
  output$gsb_w <- renderPlot({ 
    
    country <- input$gsb_w_input
    
    winter_for_plots2 <- winter_for_plots %>% 
      filter(Year %in% c(2018)) %>%
      select(Country, Code, Year, Bronze, Silver, Gold) %>%
      pivot_longer(4:6, names_to="Medals", values_to="Count_by_medal") %>%
      select(Country, Code, Year, Medals, Count_by_medal) %>%
      mutate(CplusM = paste(Country, Medals)) %>%
      data.frame()
    
    winter_for_plots2 <- bind_rows(winter_for_plots, winter_for_plots2) %>%
      select(Country, Code, Year, Medals, Count_by_medal, CplusM)
    
    winter_plot <- winter_for_plots2[winter_for_plots2$CplusM %like% country,]
    
    winter_plot <- winter_plot %>%
      ggplot(aes(x = Year, y = Count_by_medal, color = CplusM)) +
      geom_line(size = 1.0, alpha = 0.8) + 
      labs(title = "Medals over winter olympics",
           subtitle = "How many medal were gained by country?",
           x = "Year",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      scale_linetype_manual() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) +
      scale_color_manual(values = myColors)
    
    winter_plot
    
  })
  
  output$gender_s <- renderPlot({
    
    gender <- summer_data %>%
      select(Country, Code, City, Year, Sport, Discipline, Event, Gender, Medal) %>%
      distinct() %>%
      data.frame() %>%
      select(Country, Code, Year, Gender) %>%
      group_by(Country, Code, Year, Gender) %>%
      summarize(Count_overall =  n()) %>%
      mutate(Count_overall = ifelse(Gender == "Men", -Count_overall, Count_overall))
    
    gndr <- input$gender_s_input
    gender_data <- subset(gender, Country == gndr)
    
    if(max(gender_data$Count_overall)-min(gender_data$Count_overall) < 25) {
      brks <- seq(-10, 15, 1)
      lbls = paste0(as.character(c(seq(10, 0, -1), seq(1, 15, 1))))
    } else if(max(gender_data$Count_overall)-min(gender_data$Count_overall) < 50) {
      brks <- seq(-20, 30, 5)
      lbls = paste0(as.character(c(seq(20, 0, -5), seq(5, 30, 5))))
    } else if (max(gender_data$Count_overall)-min(gender_data$Count_overall) < 100) {
      brks <- seq(-30, 70, 10)
      lbls = paste0(as.character(c(seq(30, 0, -10), seq(10, 70, 10))))
    } else {
      brks <- seq(-75, 250, 25)
      lbls = paste0(as.character(c(seq(75, 0, -25), seq(25, 250, 25))))
    }
    
    gender_plot <- ggplot(gender_data, aes(x = Year, y = Count_overall, fill = Gender)) +
      geom_bar(stat = "identity", width = 2) +  
      scale_y_continuous(breaks = brks, labels = lbls) + 
      coord_flip() +
      #theme_tufte() +
      scale_fill_brewer(palette = "Dark2") +
      labs(title = "Medals acc. to gender",
           subtitle = "How many medals were achieved acc. to gender?",
           x = "Year",
           y = "Number of medals") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    
    gender_plot
    
  })
  
  output$gender_w <- renderPlot({
    
    gender <- winter_data %>%
      select(Country, Code, City, Year, Sport, Discipline, Event, Gender, Medal) %>%
      distinct() %>%
      data.frame() %>%
      select(Country, Code, Year, Gender) %>%
      group_by(Country, Code, Year, Gender) %>%
      summarize(Count_overall =  n()) %>%
      mutate(Count_overall = ifelse(Gender == "Men", -Count_overall, Count_overall))
    
    gndr <- input$gender_w_input
    gender_data <- subset(gender, Country == gndr)
    
    if(max(gender_data$Count_overall)-min(gender_data$Count_overall) < 25) {
      brks <- seq(-10, 15, 1)
      lbls = paste0(as.character(c(seq(10, 0, -1), seq(1, 15, 1))))
    } else if(max(gender_data$Count_overall)-min(gender_data$Count_overall) < 50) {
      brks <- seq(-20, 30, 5)
      lbls = paste0(as.character(c(seq(20, 0, -5), seq(5, 30, 5))))
    } else if (max(gender_data$Count_overall)-min(gender_data$Count_overall) < 100) {
      brks <- seq(-30, 70, 10)
      lbls = paste0(as.character(c(seq(30, 0, -10), seq(10, 70, 10))))
    } else {
      brks <- seq(-75, 250, 25)
      lbls = paste0(as.character(c(seq(75, 0, -25), seq(25, 250, 25))))
    }
    
    gender_plot <- ggplot(gender_data, aes(x = Year, y = Count_overall, fill = Gender)) +
      geom_bar(stat = "identity", width = 2) +  
      scale_y_continuous(breaks = brks, labels = lbls) + 
      coord_flip() +
      scale_fill_brewer(palette = "Dark2") +
      labs(title = "Medals acc. to gender",
           subtitle = "How many medals were achieved acc. to gender?",
           x = "Year",
           y = "Number of medals") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    
    gender_plot
    
  })
  
  output$top_olympian_s_plot <- renderPlot({
    
    top_ol <- input$top_olympian_s_input
    top_olympian_data <- subset(top_olympian_s, Discipline == top_ol)
    top_olympian_data <- slice_head(top_olympian_data, n = 5)
    top_olympian_data <- top_olympian_data %>% pivot_longer(5:7, names_to="Medal", values_to="Medals_by_place")
    
    top_olympian_plot <- ggplot(top_olympian_data) +
      aes(x = reorder(AthleteAccCountry, -Medals_by_place, sum), fill = Medal, weight = Medals_by_place) +
      geom_bar() +
      scale_fill_manual(values = c(Bronze = "#CD7F32", Gold = "#FFD700", Silver = "#C0C0C0")) +
      labs(title = "Top 5 summer olympians",
           subtitle = "How many medals were gained by top 5 olympians?",
           x = "",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(axis.text = element_text(size = 9))
    
    top_olympian_plot
    
  })
  
  output$top_olympian_w_plot <- renderPlot({
      
    top_ol <- input$top_olympian_w_input
    top_olympian_data <- subset(top_olympian_w, Discipline == top_ol)
    top_olympian_data <- slice_head(top_olympian_data, n = 5)
    top_olympian_data <- top_olympian_data %>% pivot_longer(5:7, names_to="Medal", values_to="Medals_by_place")
    
    top_olympian_plot <- ggplot(top_olympian_data) +
      aes(x = reorder(AthleteAccCountry, -Medals_by_place, sum), fill = Medal, weight = Medals_by_place) +
      geom_bar() +
      scale_fill_manual(values = c(Bronze = "#CD7F32", Gold = "#FFD700", Silver = "#C0C0C0")) +
      labs(title = "Top 5 winter olympians",
           subtitle = "How many medals were gained by top 5 olympians?",
           x = "",
           y = "Number of medals",
           color = "Medal") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) + 
      theme(axis.text = element_text(size = 9))
    
    top_olympian_plot
    
  })
    
  output$summer <- DT::renderDataTable(DT::datatable({
    data <- summer_data
    if (input$country_s != "All") {
      data <- data[data$Country == input$country_s,]
    }
    if (input$year_s != "All") {
      data <- data[data$Year == input$year_s,]
    }
    if (input$discipline_s != "All") {
      data <- data[data$Discipline == input$discipline_s,]
    }
    if (input$gender_s != "All") {
      data <- data[data$Gender == input$gender_s,]
    }
    if (input$athlete_s != "All") {
      data <- data[data$Athlete == input$athlete_s,]
    }
    if (input$medal_s != "All") {
      data <- data[data$Medal == input$medal_s,]
    }
    data
  }))
  
  output$winter <- DT::renderDataTable(DT::datatable({
    data <- winter_data
    if (input$country_w != "All") {
      data <- data[data$Country == input$country_w,]
    }
    if (input$year_w != "All") {
      data <- data[data$Year == input$year_w,]
    }
    if (input$discipline_w != "All") {
      data <- data[data$Discipline == input$discipline_w,]
    }
    if (input$gender_w != "All") {
      data <- data[data$Gender == input$gender_w,]
    }
    if (input$athlete_w != "All") {
      data <- data[data$Athlete == input$athlete_w,]
    }
    if (input$medal_w != "All") {
      data <- data[data$Medal == input$medal_w,]
    }
    data
  }))
}

shinyApp(ui, server)
