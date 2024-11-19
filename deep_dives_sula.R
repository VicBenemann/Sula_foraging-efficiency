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


#Visualizando graficamente

ggplot(data_MS, aes(x = Timestamp, y = Depth, color = SEX)) +
  geom_line() +
  labs(title = "Profundidade ao Longo do Tempo - Moleques do Sul (MS)",
       x = "Tempo",
       y = "Profundidade (m)") +
  theme_minimal()

theme_set(theme_minimal())

#----Por indivíduo-----

# Converter data_MS$DateTime para POSIXct
data_MS$DateTime <- as.POSIXct(data_MS$DateTime, format = "%d/%m/%Y %H:%M", tz = "UTC")

# Gráfico para Moleques do Sul (MS)
plot_MS <- ggplot(data_MS, aes(x = DateTime, y = Depth, color = island)) +
  geom_line() +
  facet_wrap(~ ID, scales = "free_x", ncol = 4) +  # Limitar para 4 colunas
  labs(
    title = "Depth over time of Brown Boobies in Moleques do Sul",
    x = "Time",
    y = "Depth (m)"
  ) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%H:%M") +  # Configurar o eixo x
  scale_color_manual(values = c("MS" = "lightblue")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),  # Rotacionar os rótulos do eixo x
    axis.ticks.x = element_blank(),                    # Remover os ticks do eixo x
    strip.text = element_text(size = 8)                # Ajustar o tamanho do texto no facet
  )

# Gráfico para São Pedro e São Paulo (SP)
plot_SP <- ggplot(data_SP, aes(x = Timestamp, y = Depth, color = island)) +
  geom_line() +
  facet_wrap(~ ID, scales = "free_x", ncol = 4) +  # Limitar para 4 colunas
  labs(
    title = "Depth over time of Brown Boobies in São Pedro e São Paulo",
    x = "Time",
    y = "Depth (m)"
  ) +
  scale_x_datetime(date_breaks = "12 hours", date_labels = "%H:%M") +  # Configurar o eixo x
  scale_color_manual(values = c("SP" = "lightgreen")) +
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

