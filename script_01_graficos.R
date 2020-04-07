library(readxl)        # Leitura de arquivos de Excel.
library(magrittr)      # Operador pipe
library(dplyr)         # Manipulacao de dados
library(timeSeries)    # Funções para trabalhar com séries de tempo
library(lubridate)     # Trabalhar com datas
library(ggplot2)       # Graficos elegantes
library(stringr)       # Manipulando character
library(purrr)         # Progrmaação funcional
library(readr)


# Salvando dados em BRL
dataset %>% write_csv("database/dataset_brl.csv")

# A tibble: 1,461 x 9
dataset <- read_csv("database/dataset_brl.csv")  

# Periodo: 2014-01-17 to 2020-01-07
dataset %>% summary()

# covert date to Date class
dataset$Date <- as.Date(dataset$Date)

# showLty(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))

# Figure 1
ggplot(data = dataset) + 
  # Ibovespa
  geom_line(mapping = aes(x = Date, y = ibov_index), linetype = 'solid') +
  # geom_point(mapping = aes(x = date, y = ls_ibov), shape = 105, size = 0.1) +
  # coord_cartesian(ylim = c(0, 145000)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
    geom_vline(
      aes(xintercept = as.numeric(Date[Date == "2016-02-01"])), 
      linetype = 2, 
      color = "red"
    ) +
  labs(
    y = "BVSP",
    title = "Figura 1.",
    subtitle = "Ibovespa, 2011–2020.",
    caption = "Fonte: Bloomberg") +
  theme_minimal()

# Figure 2
ggplot(data = dataset) + 
  # Gold
  geom_line(mapping = aes(x = Date, y = gold_price), color = "aquamarine3") +
  geom_point(mapping = aes(x = Date, y = gold_price), shape = 105, size = 0.1) +
  # coord_cartesian(ylim = c(50, 240)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "Brazilian Reals (R$)",
    title = "Figure 2.",
    subtitle = "Gold Spot Brazil, 2011–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# Figure 3
ggplot(data = dataset) + 
  # LFT
  geom_line(mapping = aes(x = Date, y = titulos), color = "aquamarine3") +
  geom_point(mapping = aes(x = Date, y = titulos), shape = 105, size = 0.1) +
  # coord_cartesian(ylim = c(3, 14)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "13 months") +
  labs(
    y = "price",
    title = "Letra Financeira do Tesouro Nacional (LFT)",
    subtitle = "LFT (pós-fixada, indexada à taxa Selic), 2014–2020.",
    caption = "Source: Tesouro Direto") +
  theme_minimal()

n <- length(dataset$titulos);
dataset <- dataset %>% mutate(logret = timeSeries::returns(titulos)) %>% na.omit()


ggplot(data = dataset) + 
  # LFT log-returns
  geom_line(mapping = aes(x = Date, y = logret), color = "aquamarine3") +
  geom_point(mapping = aes(x = Date, y = logret), shape = 105, size = 0.1) +
  # coord_cartesian(ylim = c(3, 14)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "13 months") +
  labs(
    y = "Log-returns",
    title = "Figure 3. Letra Financeira do Tesouro Nacional (LFT)",
    subtitle = "LFT (pós-fixada, indexada à taxa Selic), 2016–2020.",
    caption = "Source: Tesouro Direto") +
  theme_minimal()

  # Figure 4
ggplot(data = dataset) + 
  # Taxa de Câmbio
  geom_line(mapping = aes(x = Date, y = tx_cambio), color = "aquamarine3") +
  geom_point(mapping = aes(x = Date, y = tx_cambio), shape = 105, size = 0.1) +
  # coord_cartesian(ylim = c(0, 15)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "Figure 4. Taxa de Câmbio",
    subtitle = "Taxa de câmbio à vista - Preço de 1 USD em BRL, 2011–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# Figure 5
ggplot(data = dataset) + 
  # Bitcoin
  geom_line(mapping = aes(x = Date, y = btc_brl), color = "aquamarine3") +
  geom_point(mapping = aes(x = Date, y = btc_brl), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "Figure 5. Taxa de Câmbio BTC/BRL",
    subtitle = "Bitcoin Real Brasileiro, 2011–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# Figure 6
ggplot(data = dataset) + 
  # smaL11
  geom_line(mapping = aes(x = Date, y = smaL11), color = "aquamarine3") +
  geom_point(mapping = aes(x = Date, y = smaL11), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "Figure 6. SMAL11",
    subtitle = "iShares Small Cap Fundo de Índice, 2011–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# Figure 7
ggplot(data = dataset) + 
  # 
  geom_line(mapping = aes(x = Date, y = bova11), color = "aquamarine2") +
  geom_point(mapping = aes(x = Date, y = bova11), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "Figure 7. BOVA11",
    subtitle = "iShares Ibovespa Fundo de Índice (BOVA11), 2011–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# covert date to Date class
comm_index$Date <- as.Date(comm_index$Date)

# Figure 8
ggplot(data = comm_index) + 
  # 
  geom_line(mapping = aes(x = Date, y = commodity), color = "aquamarine2") +
  geom_point(mapping = aes(x = Date, y = commodity), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "Figure 8. Índice de Commodities",
    subtitle = "Dow Jones/B3 Índice de Commodities, 2011–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# Figure 9
ggplot(data = dataset) + 
  # Tesouro IPCA+  com Juros Semestrais (NTN-B)
  geom_line(mapping = aes(x = Date, y = NTNb), color = "aquamarine2") +
  geom_point(mapping = aes(x = Date, y = NTNb), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "Figure 9. Notas do Tesouro Nacional série B (NTN-B)",
    subtitle = "Tesouro IPCA+  com Juros Semestrais, 2010–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

n <- length(dataset$NTNb);
logret <- log(dataset$NTNb[-1]/dataset$NTNb[-n])

n <- length(dataset$NTNb);
dataset <- dataset %>% mutate(logret = timeSeries::returns(NTNb), logret2 = timeSeries::returns(NTNf)) %>% na.omit()

# Figure 10
ggplot(data = dataset) + 
  # log return (NTN-B)
  geom_line(mapping = aes(x = Date, y = logret), color = "aquamarine2") +
  geom_point(mapping = aes(x = Date, y = logret), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "log-returns",
    title = "Notas do Tesouro Nacional série B (NTN-B)",
    subtitle = "Tesouro IPCA+  com Juros Semestrais, 2010–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# Figure 11
ggplot(data = dataset) + 
  # Notas do Tesouro Nacional série F (NTN-F)
  geom_line(mapping = aes(x = Date, y = NTNf), color = "aquamarine2") +
  geom_point(mapping = aes(x = Date, y = NTNf), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "Notas do Tesouro Nacional série F (NTN-F)",
    subtitle = "Tesouro Prefixado com Juros Semestrais (NTN-F), 2010–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()


# Figure 12
ggplot(data = dataset) + 
  # Notas do Tesouro Nacional série F (NTN-F)
  geom_line(mapping = aes(x = Date, y = logret2), color = "aquamarine2") +
  geom_point(mapping = aes(x = Date, y = logret2), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "log-returns",
    title = "Notas do Tesouro Nacional série F (NTN-F)",
    subtitle = "Tesouro Prefixado com Juros Semestrais (NTN-F), 2010–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()

# Figure 13
ggplot(data = dataset) + 
  # Taxa DI - Cetip
  geom_line(mapping = aes(x = Date, y = taxa_di), color = "aquamarine2") +
  geom_point(mapping = aes(x = Date, y = taxa_di), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "20 months") +
  labs(
    y = "",
    title = "",
    subtitle = "Taxa DI 2010–2020.",
    caption = "Source: Bloomberg") +
  theme_minimal()




# Mais alguns gráficos in USD

setwd("~/Dropbox/Winicius/Artigo Cryptocurrencies")

# Gold
gold_spot_USD <- read.csv("database/investing/XAU_USD_spot_gold2.csv") %>% 
  as_tibble() %>% rename(Date = Data, gold_usd = Último) %>% select(1,2)

# CRB
CRB_USD <- read.csv("database/investing/TR_CC_CRB_Excess_Return.csv") %>% 
  as_tibble() %>% rename(price_usd_crb = Price) %>% select(1,2) 

# Bitcoin
btc_usd <- read.csv("database/investing/BTC_USD_Bitfinex.csv") %>% 
  as_tibble() %>% rename(btc_usd_price = Price) %>% select(1,2)

# Título do tesouro emitido pelo governo dos EUA com vencimento em 10 anos (NYSE)
bonds_usd <- read.csv("database/investing/United States 10-Year Bond Yield Historical Data.csv") %>% 
  as_tibble() %>% rename(bonds_price = Price) %>% select(1,2)


# Definindo funções 
replace_Commas<-function(x){
  x<-as.character(gsub("\\,", ".", x))
}

replace_Commas2<-function(x){
  x<-as.character(gsub("\\, ", "-", x))
}

replace_space<-function(x){
  x<-as.character(gsub("\\ ", "-", x))
}

remove_Dot<-function(x){
  x<-as.character(gsub("\\.", "", x))
}

remove_Commas<-function(x){
  x<-as.character(gsub("\\,", "", x))
}


replace_Dot<-function(x){
  x<-as.character(gsub("\\.", ",", x))
}

# Aplicando funções
gold_spot_USD$gold_usd <- remove_Dot(gold_spot_USD$gold_usd)
gold_spot_USD$gold_usd <- replace_Commas(gold_spot_USD$gold_usd)

btc_usd$btc_usd_price <- remove_Commas(btc_usd$btc_usd_price) %>% as.double()

bonds_usd$bonds_price <- remove_Commas(bonds_usd$bonds_price) %>% as.double()

gold_spot_USD <- gold_spot_USD %>% 
  mutate(gold_usd = as.double(gold_usd))



# Manipulando datas
gold_spot_USD$Date <- as.Date(parse_date_time(gold_spot_USD$Date,"dmy"))

CRB_USD$Date <- CRB_USD$Date %>% replace_Commas2() %>% replace_space()
dia <- CRB_USD$Date %>% str_sub(5,6)
mes <- CRB_USD$Date %>% str_sub(1,3) 
ano <- CRB_USD$Date %>% str_sub(8,11)
CRB_USD$Date <- dmy(paste(dia,mes,ano, sep = "-"))

btc_usd$Date <- btc_usd$Date %>% replace_Commas2() %>% replace_space()
dia <- btc_usd$Date %>% str_sub(5,6)
mes <- btc_usd$Date %>% str_sub(1,3) 
ano <- btc_usd$Date %>% str_sub(8,11)
btc_usd$Date <- dmy(paste(dia,mes,ano, sep = "-"))

bonds_usd$Date <- bonds_usd$Date %>% replace_Commas2() %>% replace_space()
dia <- bonds_usd$Date %>% str_sub(5,6)
mes <- bonds_usd$Date %>% str_sub(1,3) 
ano <- bonds_usd$Date %>% str_sub(8,11)
bonds_usd$Date <- dmy(paste(dia,mes,ano, sep = "-"))


gold_spot_USD$Date <- as.Date(gold_spot_USD$Date)
CRB_USD$Date <- as.Date(CRB_USD$Date)
btc_usd$Date <- as.Date(btc_usd$Date)

dataset2 <- gold_spot_USD %>% inner_join(CRB_USD) %>% 
  inner_join(btc_usd) %>% inner_join(bonds_usd)  %>% arrange(Date)

dataset2 <- read_csv("database/dataset_USD.csv") %>% as_tibble() %>% select(-1)
dataset2$Date <- as.Date(dataset2$Date)

# Sincronizando os dados em BRL e USD, by = "Date"
mydata <- dataset %>% inner_join(dataset2) %>% select(1,10,11,12,13)

# 2014-01-17 to 2020-01-07
mydata %>% summary()

write_csv(mydata,"database/dataset_usd.csv")

# Importando dados para os gráficos.
library(readr)
mydata <- read_csv("database/dataset_usd.csv")

# plotting ----------------------------------------------------------------

# Spot gold (XAU USD)
ggplot(data = mydata) + 
  geom_line(mapping = aes(x = Date, y = gold_usd), color = "#1c9099") +
  geom_point(mapping = aes(x = Date, y = gold_usd), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "16 months") +
  labs(
    y = "USD",
      title = "XAU/USD - Gold Spot US Dollar ",
    subtitle = "Preço spot do ouro Dólar Americano, 2013–2020.",
    caption = "Source: Investing.com") +
  theme_minimal()

# Thomson Reuters/CoreCommodity CRB Index
ggplot(data = mydata) + 
  geom_line(mapping = aes(x = Date, y = price_usd_crb), color = "#1c9099") +
  geom_point(mapping = aes(x = Date, y = price_usd_crb), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "18 months") +
  labs(
    y = "USD",
    title = "TR/CC CRB Excess Return (TRCCRB)",
    subtitle = "Thomson Reuters/CoreCommodity CRB Commodity Index, 2013–2020.",
    caption = "Source: Investing.com") +
  theme_minimal()

# BTC USD
ggplot(data = mydata) + 
  geom_line(mapping = aes(x = Date, y = btc_usd_price), color = "#1c9099") +
  geom_point(mapping = aes(x = Date, y = btc_usd_price), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "18 months") +
  labs(
    y = "USD",
    title = "BTC/USD - Bitcoin US Dollar",
    subtitle = "Bitcoin Dólar Americano, 2013–2020.",
    caption = "Source: Investing.com") +
  theme_minimal()

# Título do tesouro emitido pelo governo dos EUA com vencimento em 10 anos (NYSE)
ggplot(data = mydata) + 
  geom_line(mapping = aes(x = Date, y = bonds_price), color = "#1c9099") +
  geom_point(mapping = aes(x = Date, y = bonds_price), shape = 105, size = 0.1) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "18 months") +
  labs(
    y = "USD",
    title = "United States 10-Year Bond Yield",
    subtitle = "Título do tesouro com vencimento em 10 anos, 2013–2020.",
    caption = "Source: Investing.com") +
  theme_minimal()














# # Figure 2
# ggplot(data = dataset) + 
#   # # Ibovespa
#   # geom_line(mapping = aes(x = date, y = ls_ibov), linetype = "longdash", color = "red") +
#   # geom_point(mapping = aes(x = date, y = ls_ibov), shape = 105, size = 0.1) +
#   # # Exchange rate USD/BRL            
#   # geom_line(mapping = aes(x = Date, y = xrate), color = "aquamarine1") +
#   # geom_point(mapping = aes(x = Date, y = xrate), shape = 105, size = 0.1) +
#   # # S&P GSCI
#   # geom_line(mapping = aes(x = Date, y = ls_comm), color = "aquamarine2") +
#   # geom_point(mapping = aes(x = Date, y = ls_comm), shape = 105, size = 0.1) +
#   # Gold
#   geom_line(mapping = aes(x = Date, y = gold_price), color = "aquamarine3") +
#   geom_point(mapping = aes(x = Date, y = gold_price), shape = 105, size = 0.1) +
#   # Bitcoin
#   # geom_line(mapping = aes(x = Date, y = ls_btc), color = "aquamarine4") +
#   # geom_point(mapping = aes(x = Date, y = ls_btc), shape = 105, size = 0.1) +
#   coord_cartesian(ylim = c(0, 240)) +
#   scale_x_date(NULL, date_labels = "%b %y", date_breaks = "18 months") +
#   labs(
#     y = "Market price, BRL",
#     title = "Figure 1.",
#     subtitle = "Exchange rate USD/BRL and Ibovespa (in logarithmic scale), 2010–2019.",
#     caption = "Source: Bloomberg") +
#   theme_minimal()

# # Plot returns
# ggplot(data = df) + 
#   # Ibovespa
#   geom_line(mapping = aes(x = Date, y = ret_ibov), color = "aquamarine1") +
#   geom_point(mapping = aes(x = Date, y = ret_ibov), shape = 105, size = 0.1) +
#   # Exchange rate USD/BRL            
#   geom_line(mapping = aes(x = Date, y = ret_xrate), color = "aquamarine4") +
#   geom_point(mapping = aes(x = Date, y = ret_xrate), shape = 105, size = 0.1) +
#   coord_cartesian(ylim = c(-.2, .12)) +
#   scale_x_date(NULL, date_labels = "%b %y", date_breaks = "18 months") +
#   labs(
#     y = "",
#     title = "Figure 2",
#     subtitle = "Returns of Bovespa index...",
#     caption = "Source: Bloomberg") +
#   theme_minimal()
