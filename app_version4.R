

# Verificar se o pacote já está instalado
if(!require("shiny")){
  # Caso contrário, instalar
  install.packages("shiny", dependencies = TRUE)
  # Carregando pacotes após a instalação
  library("shiny")
}

if(!require("readxl")){
  install.packages("readxl", dependencies = TRUE)
  library("readxl")
}

if(!require("dplyr")){
  install.packages("dplyr", dependencies = TRUE)
  library("dplyr")
}

if(!require("stringr")){
  install.packages("stringr", dependencies = TRUE)
  library("stringr")
}

if(!require("purrr")){
  install.packages("purrr", dependencies = TRUE)
  library("purrr")
}

if(!require("openxlsx")){
  install.packages("openxlsx", dependencies = TRUE)
  library("openxlsx")
}

if(!require("shinyWidgets")){
  install.packages("shinyWidgets", dependencies = TRUE)
  library("shinyWidgets")
}


# ui ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  sidebarLayout(
    wellPanel(
      width = 4,
      h1("Aplicativo name"),
      hr(),
      h4("subtitulo"),
      hr(),
      h5("Combina diferentes arquivos xlsx e gera um relatório com os itens que possuem o mesmo código."),
      hr(),
      fileInput("xlsx", "Upload dos arquivos aqui", multiple = T),
      actionButton("go", "Gerar Relatório"),
      downloadButton("goD", "Download")
    ),
    mainPanel()
  )
)

# server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Cria uma base única, independentemente da qtde de arquivos inputados.
  mydata <- 
    reactive({ 
      # Obter caminhos dos arquivos:
      xlsx_files <- input$xlsx$datapath %>% .[str_detect(., ".xlsx")]
      
      n <- length(xlsx_files)
      
      filenames <- dir(".path/arquivos", full.names = F)
      
      map_df(1:n,
             ~read_excel(xlsx_files[.x], sheet=1, skip = 10, col_names = TRUE,  col_types = "text") %>% 
               select(c(1,2,4)) %>% 
        mutate(Contrato = filenames[.x],
               `PREÇO REFERENCIAL` = round(as.double(`PREÇO REFERENCIAL`),2)
        ))
    })

  observeEvent(input$go,{
    
    # Seleciona apenas os elementos duplicados, se houver.
    Alerta <- 
      mydata() %>%
      group_by(CÓDIGO) %>%
      filter(!n_distinct(`PREÇO REFERENCIAL`)==1)
    
    n_occur <- data.frame(table(mydata()$CÓDIGO)) %>% 
      arrange(desc(Freq))
    
    sendSweetAlert(session = session, "Concluído.", "Objetos criados com sucesso",type = "success")
    
    # Downloadable xlsx of selected dataset ----
    output$goD <- downloadHandler(
      
      filename = "Alerta_file.xlsx",
      content = function(file) {
        wb <- createWorkbook()
        addWorksheet(wb, "Database")
        addWorksheet(wb, "Alerta", tabColour = "#ff4500")
        addWorksheet(wb, "Frequência")
        writeData(wb, 1, mydata())
        writeData(wb, 2, Alerta)
        writeData(wb, 3, n_occur)
        
        # Salvando workbook
        saveWorkbook(wb, file, overwrite = TRUE)}
    )
    
  })}

shinyApp(ui, server)
