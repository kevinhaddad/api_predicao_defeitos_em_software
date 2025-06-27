library(shiny)
library(tidyverse)
library(ggplot2)
library(httr)
library(readxl)  # Necessário para ler o Excel

# Interface do usuário
ui <- fluidPage(
  titlePanel("Predição de Defeitos em Software"),
  sidebarLayout(
    sidebarPanel(
      numericInput("valor", "Valor da Métrica:", value = 1, min = 0),
      actionButton("predict", "Calcular Predição"),
      hr(),
      h4("Informações do Modelo:"),
      verbatimTextOutput("model_info"),
      tags$small("A aplicação faz predições baseadas em análise estatística.")
    ),
    mainPanel(
      h3("Resultado da Predição:"),
      verbatimTextOutput("prediction"),
      plotOutput("scatterPlot"),
      h4("Diagnóstico do Modelo:"),
      plotOutput("diagnosticPlot")
    )
  ),
  tags$footer(
    style = "text-align: center; padding: 10px;",
    tags$p("Sistema de Predição de Defeitos - Trabalho G2")
  )
)

# Lógica do servidor
server <- function(input, output, session) {
  
  # Carregar o modelo com tratamento de erro
  modelo <- reactive({
    tryCatch({
      readRDS("modelo.rds")
    }, error = function(e) {
      showNotification("Erro ao carregar o modelo", type = "error")
      NULL
    })
  })
  
  # Carregar dados com tratamento de erro
  dados_analise <- reactive({
    tryCatch({
      read_excel("dataset_KC1_classlevel_numdefect.xlsx")
    }, error = function(e) {
      showNotification("Erro ao carregar dados", type = "error")
      NULL
    })
  })
  
  # Informações do modelo
  output$model_info <- renderPrint({
    if (!is.null(modelo())) {
      cat("Variável preditora:", names(modelo()$coefficients)[2], "\n")
      cat("Fórmula do modelo:", deparse(formula(modelo())), "\n")
      cat("R² ajustado:", round(summary(modelo())$adj.r.squared, 3))
    } else {
      cat("Modelo não disponível")
    }
  })
  
  # Fazer predição diretamente com o modelo
  predicao <- eventReactive(input$predict, {
    tryCatch({
      modelo_atual <- modelo()
      if (is.null(modelo_atual)) stop("Modelo não carregado")
      variavel <- names(modelo_atual$coefficients)[2]
      dados_novos <- data.frame(setNames(list(input$valor), variavel))
      predict(modelo_atual, newdata = dados_novos)[[1]]
    }, error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error")
      NULL
    })
  })
  
  # Saída da predição
  output$prediction <- renderPrint({
    if (!is.null(predicao())) {
      cat("Número previsto de defeitos:", round(predicao(), 2))
    } else {
      cat("Predição indisponível")
    }
  })
  
  # Gráfico de dispersão
  output$scatterPlot <- renderPlot({
    req(dados_analise(), modelo())
    variavel <- names(modelo()$coefficients)[2]
    ggplot(dados_analise(), aes_string(x = variavel, y = "NUMDEFECTS")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      geom_vline(xintercept = input$valor, color = "blue", linetype = "dashed", size = 1) +
      labs(title = "Relação entre Variáveis", x = variavel, y = "Número de Defeitos") +
      theme_minimal(base_size = 14)
  })
  
  # Gráficos de diagnóstico
  output$diagnosticPlot <- renderPlot({
    req(modelo())
    par(mfrow = c(2, 2))
    plot(modelo(), which = 1:4)
  })
  
  # Manutenção de conexão
  observe({
    invalidateLater(30000)
    Sys.sleep(0.1)
  })
}

# Executar a aplicação
shinyApp(ui = ui, server = server)
