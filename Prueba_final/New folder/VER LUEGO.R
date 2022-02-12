# Entregables

## Arquivo .Rmd (gostaríamos de conseguir rodá-lo, ou seja, se ele for reprodutível é melhor)
## Códigos auxiliares em R (se existirem)
## O output final (em HTML, PDF, Word, etc). 

# Relatório/apresentação deve conter

## 1) uma breve descrição da base de dados.

## 2) os resultados da sua análise, com explicações/interpretações das visualizações construídas. Os resultados devem conter pelo menos um gráfico em {ggplot2}, de preferência estilizado com um tema legal

## #) uma conclusão tirada a partir da análise

# Preparación de los datos

# Origen de la base de datos
##"https://www.openweedsci.org/post/2021/03/05/web-scraping-herbicide-weed-resistance-data/"

# Cargo las librerias
library(tidyverse)
library(xml2)
library(rvest)

# Cargo la tabla de la pagina web y la ajusto
world_resistance <- read_html("http://weedscience.org/Summary/CountrySummary.aspx")

resistance_chart <- world_resistance %>% 
  html_node("table") %>% # selector = table
  html_table(fill = TRUE) # get a table

resistance_chart %>% 
  slice_head(n = 5) # display 5 rows 

resistance_chart1 <- resistance_chart %>% 
  janitor::row_to_names(row_number = 2) %>% # make second column header
  janitor::clean_names() %>% # clean header
  as_tibble() %>% # tibble is better than data.frame 
  mutate_at(vars(-country), as.integer) # make column numbers as integers

resistance_chart <- resistance_chart1 %>% 
  pivot_longer(cols = a:other, # select columns to pivot
               names_to = "soa", # new column name
               values_to = "resistant") # new values column name

head(resistance_chart)
# Save an object to a file
#save(resistance_chart, file = "0.project_GitHub/202107-visualizacao/Prueba_final/resistance_chart.RData")

# DataFrame 2

# change function
country_function <- function(id) {
  
  url <- paste0("http://weedscience.org/Summary/Country.aspx?CountryID=",id,"")
  #id will change by each country id number
  
  # Read url
  resistance <- read_html(url)
  
  # Extract herbicide resistance data
  chart <- resistance %>% 
    html_node(".rgMasterTable") %>% # selector
    html_table(fill = TRUE) # get the table
  
  # Tidy dataset
  final_chart <- chart %>% 
    janitor::row_to_names(row_number = 2) %>% # make second column header
    janitor::clean_names() %>% # clean header
    as_tibble() %>% # tibble is better than data.frame
    drop_na() %>% # drop NA values
    mutate_at(c("number", "first_year", 
                "country_id", "resist_id"), 
              as.integer) # make columns numbers as integer 
  # Get final dataset
  final_chart 
}
country_function(id = 1)

country <- tribble(
  ~country_name, ~id,
  "argentina", 48,
  "australia", 1,
  "austria", 2,
  "belgium ", 3,
  "bolivia", 4,
  "brazil", 5,
  "bulgaria", 6,
  "canada", 7,
  "chile", 8,
  "china", 9,
  "colombia", 10,
  "costa rica", 11,
  "czech republic", 12,
  "denmark", 13,
  "ecuador", 14,
  "egypt", 15,
  "fiji", 16,
  "france", 17,
  "germany", 18,
  "greece", 19,
  "hungary", 20,
  "india", 21,
  "indonesia", 22,
  "israel", 23,
  "italy", 24,
  "japan", 25,
  "kenya", 26,
  "south korea", 27,
  "malaysia", 28,
  "mexico", 29,
  "new zealand", 30,
  "norway", 31,
  "philippines", 32,
  "poland", 33,
  "portugal", 34,
  "saudi arabia", 35,
  "slovenia", 36,
  "south africa", 37,
  "spain", 38,
  "sri lanka", 39,
  "sweden", 40,
  "switzerland", 41,
  "taiwan", 42,
  "netherlands", 43,
  "united kingdom", 44,
  "united states", 45,
  "paraguay", 46,
  "thailand", 47,
  "cyprus", 53,
  "jordan", 55,
  "nicaragua", 60,
  "russia", 65,
  "syria", 69,
  "turkey", 71,
  "uruguay", 73,
  "ethiopia", 76,
  "tunisia", 78,
  "iran", 79,
  "venezuela", 80,
  "ireland", 81,
  "panama", 82,
  "el salvador", 83,
  "guatemala", 84,
  "honduras", 85,
  "pakistan", 86,
  "finland", 139,
  "kazakhstan", 157,
  "latvia", 174,
  "lithuania", 185,
  "serbia", 245,
  "ukraine", 230
)

head(country)

resistance_data <- country %>% 
  arrange(country_name) %>% #arrange country in alphabetical order
  mutate(resistance_data = map(id, country_function)) %>% # iterate function over id resistance data is in a list by each country
  unnest(resistance_data) %>% # unlist resistance data
  dplyr::select(-x) # removing column x

head(resistance_data)

#save(resistance_data, file = "0.project_GitHub/202107-visualizacao/Prueba_final/resistance_data.RData")


head(resistance_chart)


small_trial <- resistance_chart %>%
  mutate_if(is.character,as.factor)

# Boxplot
small_trial %>% 
  ggplot() + 
  aes(y = resistant, fill = soa) +
  geom_boxplot() +
  theme(legend.position = "top")


# Histograma
small_trial %>% 
  filter(country==c('United States','Australia','Brazil','Canada','China')) %>%
  ggplot() + 
  aes(x = total, fill = country) +
  geom_histogram() +
  theme(legend.position = "top")

# Tabla
small_trial %>%
  select(country, total, soa, resistant) %>% 
  group_by(country) %>% 
  summarise(average_Country = mean (resistant)) %>% 
  knitr::kable()

small_trial %>%
  select(country, total, soa, resistant) %>% 
  group_by(soa) %>% 
  summarise(average_SOA = mean (resistant)) %>% 
  knitr::kable()


# figure
data_trial <- resistance_data %>%
  mutate_if(is.character,as.factor)

data_trial %>% 
  filter(country==c('Argentina','Brazil','United States','Spain')) %>% 
  ggplot(aes(x = first_year, y = resist_id, color = country)) +
  geom_line() 

library(wordcloud2)

s_freq <- data_trial %>%
  group_by(species) %>%
  summarise(n = n())

wordcloud2(data=s_freq, size = 0.7)
wordcloud2(data=s_freq, size = 0.7, color = 'random-light',backgroundColor = 'Black')
wordcloud2(data=s_freq,size = 0.5,shape = 'pentagon')

s_freq <- data_trial %>%
  group_by(common_name) %>%
  summarise(n = n())

wordcloud2(data=s_freq, size = 0.7)
wordcloud2(data=s_freq, size = 0.7, color = 'random-light',backgroundColor = 'Black')
wordcloud2(data=s_freq,size = 0.5,shape = 'pentagon')

s_freq <- data_trial %>%
  group_by(country) %>%
  summarise(n = n())

wordcloud2(data=s_freq, size = 0.7)
wordcloud2(data=s_freq, size = 0.7, color = 'random-light',backgroundColor = 'Black')
wordcloud2(data=s_freq,size = 0.5,shape = 'pentagon')

s_freq <- data_trial %>%
  group_by(site_of_action) %>%
  summarise(n = n())

wordcloud2(data=s_freq, size = 0.7)
wordcloud2(data=s_freq, size = 0.7, color = 'random-light',backgroundColor = 'Black')
wordcloud2(data=s_freq,size = 0.5,shape = 'pentagon')


getwd()
load(file = "0.project_GitHub/202107-visualizacao/Prueba_final/resistance_chart.RData")
