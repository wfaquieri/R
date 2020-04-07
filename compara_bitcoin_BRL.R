
#' O presente script tem por objetivo comparar os preços de BItcoin em BRL
#' coletados na plataforma Bloomberg e na exchange Marcado Bitcoin.
#'

library(readxl)                  # Leitura de arquivos de Excel.
library(magrittr)                # Operador pipe
library(dplyr)                   # Manipulacao de dados
library(timeSeries)              # Funções para trabalhar com séries de tempo
library(lubridate)               # Trabalhar com datas
library(ggplot2)
library(stringr)

btc_price_old <- read_excel("~/Dropbox/Winicius/Artigo Cryptocurrencies/database/bloomberg/btc_price.xlsx") %>% rename(Date = date)
btc_price_new <- read.csv("~/Dropbox/Winicius/Artigo Cryptocurrencies/database/BTC_BRL_MercadoBitcoin.csv") %>% 
  as_tibble() %>% rename(Date = Data) 

gold_spot_USD <- read.csv("~/Dropbox/Winicius/Artigo Cryptocurrencies/database/XAU_USD_spot_gold2.csv") %>% 
  as_tibble() %>% rename(Date = Data, gold_usd = Último) %>% select(1,2)

CRB_USD <- read.csv("~/Dropbox/Winicius/Artigo Cryptocurrencies/database/TR_CC_CRB_Excess_Return.csv") %>% 
  as_tibble() %>% rename(price_usd_crb = Price) %>% select(1,2) 

btc_usd <- read.csv("~/Dropbox/Winicius/Artigo Cryptocurrencies/database/BTC_USD_Bitfinex.csv") %>% 
  as_tibble() %>% rename(btc_usd_price = Price) %>% select(1,2)
  
remove_Commas<-function(x){
  x<-as.character(gsub("\\,", ".", x))
}

replace_Dot<-function(x){
  x<-as.character(gsub("\\.", "", x))
}

btc_price_new$Último <- replace_Dot(btc_price_new$Último) 
btc_price_new$Último <- remove_Commas(btc_price_new$Último)
# btc_price_new$Último <- gsub('.$', '', btc_price_new$Último)

btc_price_new <- btc_price_new %>% mutate(last_price = as.double(Último)) %>% select(c(1,8))

btc_price_new$Date <- as.Date(parse_date_time(btc_price_new$Date,"dmy")) 

btc_price_old$Date <- as_date(btc_price_old$Date)
dataset <- btc_price_old %>% inner_join(btc_price_new) 


# covert date to Date class
dataset$Date <- as.Date(dataset$Date)

dataset <- dataset %>% mutate(diff = (last_price - btc_brl),
                   mean_dif = mean(diff))

# dataset$Último=as.numeric(levels(dataset$Último))[dataset$Último]

ggplot(data = dataset) + 
  # Bitcoin in brl (bloomberg)
  geom_line(mapping = aes(x = Date, y = btc_brl), color = "#d8b365") +
  geom_point(mapping = aes(x = Date, y = btc_brl), shape = 105, size = 0.1) +
  # Bitcoin in brl (Mercado Bitcoin)  
  geom_line(mapping = aes(x = Date, y = last_price), color = "#5ab4ac") +
  geom_point(mapping = aes(x = Date, y = last_price), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "16 months") +
  labs(
    y = "",
    title = "Figure 5. Bitcoin in brl (Bloomberg x Mercado Bitcoin)",
    subtitle = "Bitcoin Real Brasileiro, 2013–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()


