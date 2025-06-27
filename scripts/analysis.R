# Carregar pacotes
library(tidyverse)
library(readxl)
library(ggpubr)
library(PerformanceAnalytics)

# 1. Carregar os dados
dados <- read_excel("C:/Users/kevin/Documents/Trabalho_G2/data/dataset_KC1_classlevel_numdefect.xlsx")

# 2. Função para calcular moda
calc_moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 3. Estatísticas descritivas para cada coluna numérica
estatisticas <- dados %>% 
  select_if(is.numeric) %>% 
  map_df(~data.frame(
    Média = mean(.x, na.rm = TRUE),
    Mediana = median(.x, na.rm = TRUE),
    Moda = calc_moda(.x),
    Desvio_Padrão = sd(.x, na.rm = TRUE),
    Mínimo = min(.x, na.rm = TRUE),
    Máximo = max(.x, na.rm = TRUE),
    Amplitude = max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)
  ), .id = "NUMDEFECTS")

print(estatisticas)

# 4. Visualizações
# Histogramas com densidade
histogramas <- dados %>% 
  select_if(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  facet_wrap(~key, scales = "free") +
  theme_minimal()

print(histogramas)

# Boxplots
boxplots <- dados %>% 
  select_if(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(y = value)) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~key, scales = "free") +
  theme_minimal()

print(boxplots)

# 5. Testes de normalidade
normalidade <- dados %>% 
  select_if(is.numeric) %>% 
  map_df(~data.frame(
    Variável = deparse(substitute(.x)),
    Shapiro_Wilk_p = shapiro.test(.x)$p.value
  ), .id = "NUMDEFECTS")

print(normalidade)

# Matriz de correlação
correlacao <- dados %>% 
  select_if(is.numeric) %>% 
  chart.Correlation(histogram = TRUE, pch = 19)

# Identificar variáveis mais correlacionadas com NUMDEFECTS
cor_target <- cor(dados %>% select_if(is.numeric)) %>% 
  as.data.frame() %>% 
  select(NUMDEFECTS) %>% 
  arrange(desc(abs(NUMDEFECTS)))

print(cor_target)

# Gráficos de dispersão para as 3 variáveis mais correlacionadas
top_cor <- rownames(cor_target)[2:4]  # exclui a correlação consigo mesma

scatter_plots <- list()
for (var in top_cor) {
  scatter_plots[[var]] <- ggplot(dados, aes_string(x = var, y = "NUMDEFECTS")) +
    geom_point() +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal()
}

# Exibir os gráficos
ggarrange(plotlist = scatter_plots, ncol = length(scatter_plots))
# 6. Identificar e salvar a variável mais correlacionada - VERSÃO CORRIGIDA (sem duplicação)

# Identificar a melhor variável
melhor_var <- rownames(cor_target)[1]
message("Variável mais correlacionada com NUMDEFECTS: ", melhor_var)

# Criar caminho seguro para a pasta api
caminho_api <- file.path(dirname(getwd()), "api")

# Criar pasta se não existir
if (!dir.exists(caminho_api)) {
  dir.create(caminho_api)
  message("Pasta 'api' criada em: ", caminho_api)
}

# Salvar a variável preditora
caminho_arquivo <- file.path(caminho_api, "variavel_preditora.txt")
writeLines(melhor_var, caminho_arquivo)

# Mensagem de confirmação final
message("Variável preditora salva em: ", normalizePath(caminho_arquivo))