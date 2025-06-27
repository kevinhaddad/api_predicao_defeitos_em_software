library(plumber)
library(here)

# CAMINHO ABSOLUTO para o modelo
model_path <- "C:/Users/kevin/Documents/Trabalho_G2/api/modelo.rds"

# Carregar modelo
modelo <- tryCatch(
  {
    readRDS(model_path)
  },
  error = function(e) {
    message("ERRO ao carregar modelo: ", e$message)
    message("Procurando em: ", normalizePath(model_path))
    NULL
  }
)

#* @apiTitle API de Predição de Defeitos
#* @get /health
function() {
  list(
    status = ifelse(is.null(modelo), "ERROR", "OK"),
    model_path = model_path,
    file_exists = file.exists(model_path)
  )
}

#* @param valor Valor numérico
#* @post /predict
function(valor) {
  if (is.null(modelo)) {
    stop("Modelo não foi carregado corretamente")
  }
  
  valor_numerico <- as.numeric(valor)
  if (is.na(valor_numerico)) {
    return(list(error = "O valor deve ser numérico"))
  }
  
  # Criar dataframe com nome correto da variável
  novos_dados <- data.frame(x = valor_numerico)
  names(novos_dados) <- names(modelo$coefficients)[2]
  
  list(
    predicao = predict(modelo, newdata = novos_dados),
    variavel_utilizada = names(modelo$coefficients)[2]
  )
}