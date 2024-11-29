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

# Carregar os dados de profundidade calculados e combinar as duas planilhas acima

# Adicionar a coluna de origem (SP ou MS)
depth_stats_SP <- depth_stats_SP %>%
  mutate(Source = "SP")  # Adiciona a coluna "Source" com valor "SP"

depth_stats_MS <- depth_stats_MS %>%
  mutate(Source = "MS")  # Adiciona a coluna "Source" com valor "MS"

# Salvar o resultado final em um novo arquivo CSV
write.csv(depth_stats_SP, "depth_stats_SP_cf", row.names = FALSE)
write.csv(depth_stats_MS, "depth_stats_MS_cf", row.names = FALSE)

#Adicionei os resultados das estatísticas de mergulho de MS e SPSP manualmente na planilha trip_metrics e atualizei no GitHub
