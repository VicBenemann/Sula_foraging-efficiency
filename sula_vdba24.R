# Carregar pacotes necessários
library(ggplot2)
library(diveMove)
library(patchwork)
library(dplyr)
library(car)

data <- dataset_foraging_efficiency

#-----Avaliando a NORMALIDADE, VARIÂNCIA E HOMOGENEIDADE dos dados -----

#-------------Normalidade----------------------------

# Teste de normalidade para vdba24 por ilha
shapiro_test_MS <- shapiro.test(data$vdba24[data$island == "MS"])
print(shapiro_test_MS)

shapiro_test_SP <- shapiro.test(data$vdba24[data$island == "SP"])
print(shapiro_test_SP)

# Teste de normalidade para DBAkJ por ilha
shapiro_test_DBAkj_MS <- shapiro.test(data$DBAkj[data$island == "MS"])
print(shapiro_test_DBAkj_MS)

shapiro_test_DBAkj_SP <- shapiro.test(data$DBAkj[data$island == "SP"])
print(shapiro_test_DBAkj_SP)

# Teste de normalidade para Eficiência de Forrageio por ilha

shapiro_test_eficiencia_MS <- shapiro.test(data$foraging_efficiency_kJ[data$island == "MS"])
print(shapiro_test_eficiencia_MS)

shapiro_test_eficiencia_SP <- shapiro.test(data$foraging_efficiency_kJ[data$island == "SP"])
print(shapiro_test_eficiencia_SP)

# -----------------Homogeneidade e variância dos dados--------------

# Teste de Levene para vdba24 por ilha
levene_test_vdba24 <- leveneTest(vdba24 ~ island, data = data)
print(levene_test_vdba24)

# Teste de Levene para DBAkJ por grupo
levene_test_DBAkj <- leveneTest(DBAkj ~ island, data = data)
print(levene_test_DBAkj)

# Histograma de vdba24 separado por ilhas
ggplot(data, aes(x = vdba24, fill = island)) +
  geom_histogram(binwidth = 50, color = "black", alpha = 0.6) +
  facet_wrap(~ island) +
  labs(title = "Histogram of VDBA24 by Island", x = "VDBA24", y = "Frequency") +
  theme_minimal()

# Histograma de DBAkJ separado por ilhas
ggplot(data, aes(x = DBAkj, fill = island)) +
  geom_histogram(binwidth = 50, color = "black", alpha = 0.6) +
  facet_wrap(~ island) +
  labs(title = "Histogram of DBA kJ by Island", x = "DBAkJ", y = "Frequency") +
  theme_minimal()

# Q-Q Plot para vdba24 separado por ilhas
ggplot(data, aes(sample = vdba24, color = island)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ island) +
  labs(title = "Q-Q Plot of VDBA24 by Island", x = "Theoretical Quantiles", y = "Observed Quantiles") +
  theme_minimal()

# Q-Q Plot para DBAkJ separado por grupo (ilhas)
ggplot(data, aes(sample = DBAkj, color = island)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ island) +
  labs(title = "Q-Q Plot of DBAkj by Island", x = "Theoretical Quantiles", y = "Observed Quantiles") +
  theme_minimal()



#--------Teste estatístico (T test) entre ilhas para o VDBA24--------

# Garantir que as categorias de 'Island' sejam MS e SP e não contenham valores ausentes
data <- data %>%
  filter(!is.na(vdba24) & !is.na(island)) %>%  # Remover linhas com NA em vdba24 ou Island
  filter(island %in% c("MS", "SP"))  # Garantir que estamos apenas com MS e SP

# Rodar o teste t para comparar vdba24 entre MS e SP
t_test_result_vdba24 <- t.test(vdba24 ~ island, data = data, var.equal = TRUE)

# Exibir o resultado do teste t
print(t_test_result_vdba24)


#--------Teste estatístico (T test) entre ilhas para o DBAkJ--------

t_test_result_DBAkj <- t.test(DBAkj ~ island, data = data, var.equal = TRUE)

# Exibir o resultado do teste t
print(t_test_result_DBAkj)


#---A eficiência de forrageio em MS não segue uma distribuição normal, então vamos analisar usando o teste de Mann-Whitney (U)

# Teste de Mann-Whitney para foraging_efficiency_kJ
wilcox_test_efficiency <- wilcox.test(foraging_efficiency_kJ ~ island, data = data)
print(wilcox_test_efficiency)



#-------Representações gráficas dos testes estatísticos (T test)----

# Boxplot para vdba24
ggplot(data, aes(x = island, y = vdba24, fill = island)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Comparação de VDBA24 entre Ilhas",
       x = "Ilha",
       y = "VDBA24") +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot para DBAkJ
ggplot(data, aes(x = island, y = DBAkj, fill = island)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Comparação de DBAkJ entre Ilhas",
       x = "Ilha",
       y = "DBAkJ") +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot para Foraging efficiency kJ
ggplot(data, aes(x = island, y = foraging_efficiency_kJ, fill = island)) +
  geom_boxplot(alpha = 0.6, outlier.color = "red", outlier.shape = 1) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 3, 
               color = "black") +
  labs(title ="Food Load Energy Density (kJ)",
       x = "Archipelago",
       y = "Energy Density (kJ)") +
  theme_bw() +  # Mudando o tema
  theme(plot.title = element_text(hjust = 0.5))+  # Centralizando o título
  scale_fill_manual(values = c("MS" = "lightblue", "SP" = "lightgreen"))  # Definindo cores para cada ilha


