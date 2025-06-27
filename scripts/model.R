library(tidyverse)
library(car)

# Carregar dados
dados <- read_excel("C:/Users/kevin/Documents/Trabalho_G2/data/dataset_KC1_classlevel_numdefect.xlsx")

# Ler a variável identificada no analysis.R
melhor_var <- readLines("../api/variavel_preditora.txt")

# Criar modelo DINAMICAMENTE com a variável correta
modelo <- lm(paste("NUMDEFECTS ~", melhor_var), data = dados)

# Resumo do modelo
summary(modelo)

# Diagnóstico dos resíduos
par(mfrow = c(2, 2))
plot(modelo)

# Salvar modelo (caminho relativo)
saveRDS(modelo, "../api/modelo.rds")