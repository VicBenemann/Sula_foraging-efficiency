# Carregar pacotes necessários
library(ggplot2)
library(diveMove)
library(patchwork)
library(dplyr)

#------Mergulho em SPSP e MS--------

# Carregar os dados de Moleques do Sul (MS) e de São Pedro e São Paulo (SP)
data_MS <- MS_vedba_odba_por_trip
data_SP <- SP_vedba_odba_por_trip

# Ordenar os dados pelo Timestamp para garantir que estejam em ordem crescente
data_MS <- data_MS[order(data_MS$Timestamp), ]
data_SP <- data_SP[order(data_SP$Timestamp), ]

# Verificar se a variável "sexo" está presente nos dados
if (!"SEX" %in% colnames(data_MS)) {
  stop("A coluna 'Sexo' não foi encontrada no conjunto de dados.")
}

if (!"SEX" %in% colnames(data_SP)) {
  stop("A coluna 'Sexo' não foi encontrada no conjunto de dados.")
}


# Converter a pressão (mbar) em profundidade (m), assumindo que a pressão está em milibares
if ("Pressure" %in% colnames(data_MS)) {
  data_MS$Depth <- (data_MS$Pressure - 1013.25) / 10.1325
}

if ("Pressure" %in% colnames(data_SP)) {
  data_SP$Depth <- (data_SP$Pressure - 1013.25) / 10.1325
}

# Verificar as primeiras linhas com a nova coluna de profundidade
head(data_MS$Depth)
head(data_SP$Depth)

# Agora vamos adicionar uma coluna "Source" em cada uma das planilhas pra identificar cada ilha no gráfico

# Adicionar a coluna de origem (SP ou MS)
data_MS <- data_MS %>%
  mutate(Source = "MS")  # Adiciona a coluna "Source" com valor "MS"

data_SP <- data_SP %>%
  mutate(Source = "SP")  # Adiciona a coluna "Source" com valor "SP"


#Visualizando graficamente

#----Por indivíduo-----

# Converter data_MS$DateTime para POSIXct
data_MS$DateTime <- as.POSIXct(data_MS$DateTime, format = "%d/%m/%Y %H:%M", tz = "UTC")

# Gráfico para Moleques do Sul (MS)
plot_MS <- ggplot(data_MS, aes(x = DateTime, y = Depth, color = Source)) +
  geom_line() +
  facet_wrap(~ ID, scales = "free_x", ncol = 4) +  # Limitar para 4 colunas
  labs(
    title = "Diving depth in Moleques do Sul (m)",
    x = "Time",
    y = "Depth (m)"
  ) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%H:%M") +  # Configurar o eixo x
  scale_color_manual(values = c("MS" = "lightblue4")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),  # Rotacionar os rótulos do eixo x
    axis.ticks.x = element_blank(),                    # Remover os ticks do eixo x
    strip.text = element_text(size = 8)                # Ajustar o tamanho do texto no facet
  )

# Gráfico para São Pedro e São Paulo (SP)
plot_SP <- ggplot(data_SP, aes(x = Timestamp, y = Depth, color = Source)) +
  geom_line() +
  facet_wrap(~ ID, scales = "free_x", ncol = 4) +  # Limitar para 4 colunas
  labs(
    title = "Diving depth in São Pedro e São Paulo (m)",
    x = "Time",
    y = "Depth (m)"
  ) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%H:%M") +  # Configurar o eixo x
  scale_color_manual(values = c("deepskyblue4")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),  # Rotacionar os rótulos do eixo x
    axis.ticks.x = element_blank(),                    # Remover os ticks do eixo x
    strip.text = element_text(size = 8)                # Ajustar o tamanho do texto no facet
  )

plot_SP

# Combinar os dois gráficos lado a lado
combined_plot <- plot_MS + plot_SP
print(combined_plot)

#-----CALCULO ODBA e VeDBA--------

# Função para calcular ODBA e VeDBA para cada indivíduo
calcular_vedba_odba <- function(df) {
  mean_x <- mean(df$X, na.rm = TRUE)
  mean_y <- mean(df$Y, na.rm = TRUE)
  mean_z <- mean(df$Z, na.rm = TRUE)
  
  df$X_dynamic <- df$X - mean_x
  df$Y_dynamic <- df$Y - mean_y
  df$Z_dynamic <- df$Z - mean_z
  
  df$ODBA <- abs(df$X_dynamic) + abs(df$Y_dynamic) + abs(df$Z_dynamic)
  df$VeDBA <- sqrt(df$X_dynamic^2 + df$Y_dynamic^2 + df$Z_dynamic^2)
  
  return(df)
}

# Aplicar a função de cálculo para cada indivíduo
data <- data %>% group_by(ID) %>% do(calcular_vedba_odba(.))

# Verificar as primeiras linhas dos dados
head(data)

# Criar gráficos de VeDBA para cada indivíduo com a variável Sexo
plots_vedba <- list()
for (individuo in individuos) {
  # Filtrar dados por indivíduo
  data_individuo <- subset(data, ID == individuo)
  
  # Gerar o gráfico de VeDBA ao longo do tempo para cada indivíduo
ggplot(data_individuo, aes(x = Timestamp, y = VeDBA, color = SEX)) +
    geom_line() +
    labs(title = paste("VeDBA para o indivíduo", individuo),
         x = "Tempo",
         y = "VeDBA (m/s²)") +
    theme_minimal()+
  scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M")

  
  # Adicionar o gráfico à lista
  plots_vedba[[individuo]] <- p
}

# Combinar todos os gráficos de VeDBA em um único layout com patchwork
combined_vedba_plots <- wrap_plots(plots_vedba)

# Exibir todos os gráficos combinados de VeDBA
print(combined_vedba_plots)

# Salvar os dados com as novas colunas calculadas
write.csv(data, "dados_com_vedba_odba_por_individuo_e_sexo.csv", row.names = FALSE)

#------Média de profundidade de cada viagem de forrageio (por Foraging trip) ------ 

# Calcular estatísticas de profundidade por viagem (tripID) para MS
depth_stats_MS <- data_MS %>%
  group_by(tripID) %>%
  summarise(
    Mean_Depth = mean(Depth, na.rm = TRUE),
    Median_Depth = median(Depth, na.rm = TRUE),
    SD_Depth = sd(Depth, na.rm = TRUE),
    Max_Depth = max(Depth, na.rm = TRUE),
    Min_Depth = min(Depth, na.rm = TRUE)
  )

# Calcular estatísticas de profundidade por viagem (tripID) para SP
depth_stats_SP <- data_SP %>%
  group_by(tripID) %>%
  summarise(
    Mean_Depth = mean(Depth, na.rm = TRUE),
    Median_Depth = median(Depth, na.rm = TRUE),
    SD_Depth = sd(Depth, na.rm = TRUE),
    Max_Depth = max(Depth, na.rm = TRUE),
    Min_Depth = min(Depth, na.rm = TRUE)
  )

# Visualizar as primeiras linhas das estatísticas calculadas para MS
print("Statistics for Moleques do Sul (MS):")
head(depth_stats_MS)

# Visualizar as primeiras linhas das estatísticas calculadas para SP
print("Statistics for São Pedro e São Paulo (SP):")
head(depth_stats_SP)
  )

# Agora eu vou incluir esses resultados na planilha original (trip_metrics):

trip_metrics

# Carregar os dados de profundidade calculados e combinar as duas planilhas acima

# Adicionar a coluna de origem (SP ou MS)
depth_stats_SP <- depth_stats_SP %>%
  mutate(Source = "SP")  # Adiciona a coluna "Source" com valor "SP"

depth_stats_MS <- depth_stats_MS %>%
  mutate(Source = "MS")  # Adiciona a coluna "Source" com valor "MS"

depth_stats_combined <- bind_rows(depth_stats_SP, depth_stats_MS)

# Verificar se as colunas correspondem
head(trip_metrics$tripID)  # TripID na planilha original
head(depth_stats_combined$tripID)   # TripID na planilha de profundidade

# Unir os dois conjuntos de dados com base na coluna TripID
trip_metrics_updated <- trip_metrics %>%
  left_join(depth_stats_combined, by = "tripID") %>% # Faz a união com base em TripID
  select(-mean_depth)  # Exclui a coluna "mean_depth" que estava duplicada (só anotação)

trip_metrics_updated

# Salvar o resultado final em um novo arquivo CSV
write.csv(trip_metrics_updated, "trip_metrics_with_depth.csv", row.names = FALSE)


