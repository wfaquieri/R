
{if(!require("ggplot2")){
  # Caso contrário, instalar
  install.packages("ggplot2", dependencies = TRUE)
  # Carregando pacotes após a instalação
  library("ggplot2")
}

if(!require("readxl")){
  # Caso contrário, instalar
  install.packages("readxl", dependencies = TRUE)
  # Carregando pacotes após a instalação
  library("readxl")
}

if(!require("purrr")){
  # Caso contrário, instalar
  install.packages("purrr", dependencies = TRUE)
  # Carregando pacotes após a instalação
  library("purrr")
}

if(!require("dplyr")){
  # Caso contrário, instalar
  install.packages("dplyr", dependencies = TRUE)
  # Carregando pacotes após a instalação
  library("dplyr")
}}



# INDEX -------------------------------------------------------------------

mydata <- read_excel("path/index.xlsx", sheet = 1) %>% 
  rename(Index = `Index_private`,
         FIPE = `IPC-FIPE - Geral`,
         IPCA = `IPCA Brasil`
  )

View(mydata)


## # covert date to Date class
mydata$date <- as.Date(mydata$Mês)

# Cores #1380A1 #FAAB18 #333333 #6495ed ##66CDAA ##800080

# Fugura 1
ggplot(data = mydata) + 
  # ISSVS
  geom_line(mapping = aes(x = date, y = ISSVS, color = I("#6495ed")), linetype = "solid", size = .5) +
  geom_point(mapping = aes(x = date, y = ISSVS,  color = I("#6495ed")), shape = 8, size = .1) +
  # IPC-FIPE
  geom_line(mapping = aes(x = date, y = FIPE, color = I("#66CDAA")), linetype = "solid", size = .5) +
  geom_point(mapping = aes(x = date, y = FIPE,  color = I("#66CDAA")), shape = 8, size = .1) +
  # IPCA
  geom_line(mapping = aes(x = date, y = IPCA, color = I("#800080")), linetype = "solid", size = .5) +
  geom_point(mapping = aes(x = date, y = IPCA,  color = I("#800080")), shape = 8, size = .1) +
  coord_cartesian(ylim = c(74, 102)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "12 months") +
  labs(
    x = NULL, y = NULL,
    title = "título, 2014–2019.",
    subtitle = "subtitulo.",
    caption = "Source: fonte") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("#6495ed", "#800080", "#66CDAA"), 
                     name=NULL,
                     labels=c("Index","IPCA-Brasil", "IPC-FIPE")) +
  theme(legend.position = "bottom") +
  ggsave("figure1.jpeg", path = "path/2-R/plots" )




# Simulacao IPCA ----------------------------------------------------------

mydata2 <- read_excel("path/3-simula_IPCA/simula_IPCA.xlsx", sheet = 1) %>% 
  rename(ISSVS = `ISS-Vigilância e Segurança (ISSVS)`)

View(mydata2)

## # covert date to Date class
mydata2$date <- as.Date(mydata2$Mês)

# Plot with ggplot
ggplot() + 
  # Sindicato
  geom_line(data = mydata2, mapping = aes(x = date, y = Index, color = "#6495ed"), size = 0.5) +
  geom_point(data = mydata2, mapping = aes(x = date, y = Index), shape = 105, size = 0.1) +
  # IPCA-Brasil
  geom_line(data = mydata, mapping = aes(x = date, y = IPCA, color = "#66CDAA"), size = 0.5) +
  geom_point(data = mydata, mapping = aes(x = date, y = IPCA), shape = 105, size = 0.1) +
  coord_cartesian(ylim = c(72, 104)) +
  scale_x_date(NULL, date_labels = "%b %y", date_breaks = "12 months") +
  labs(
    y = "",
    title = "titulo",
    subtitle = "Simulação IPCA vs Index, 2014–2019.",
    caption = "Fonte: fonte") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("#6495ed", "#66CDAA"), 
                     name=NULL,
                     labels=c("Index (ipca)","Index (sindicato)")) +
  theme(legend.position = "bottom") +
  ggsave("figure2.jpeg", path = "path/2-R/plots" )




# end ---------------------------------------------------------------------


















