# Atualizando o dataframe com as informações fornecidas
dados <- data.frame(
  nome_cientifico = c('Handroanthus serratifolius*', 'Hymenaea courbaril', 'Manilkara huberi', 
                      'Couratari guianensis', 'Dinizia excelsa', 'Apuleia leiocarpa**', 'Dipteryx odorata*', 
                      'Cariniana micrantha', 'Astronium lecointei', 'Hymenolobium petraeum'),
  total_volume = c(853849.393, 385451.238, 394606.643, 449653.8, 356778.106, 341795.276, 241697.109,
                   199879.47, 154722.443, 151006.66),
  percentual = c(14, 6, 6, 7, 6, 6, 4, 3, 3, 2)
)

# Verificando os dados atualizados
dados


# Verificando os dados atualizados
dados



# Ordenando os dados pelo total_volume
dados <- dados[order(dados$total_volume),]



# Reordenar os níveis do fator nome_cientifico com base no total_volume
dados <- dados %>%
  mutate(nome_cientifico = factor(nome_cientifico, levels = reorder(nome_cientifico, -total_volume)))

library(ggplot2)

# Criando o gráfico com ggplot
ggplot(dados, aes(x = total_volume, y = nome_cientifico)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "10 Principais espécies exportadas",
       x = "Volume acumulado (2010-2023)",
       y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, hjust = 0, face = "italic"),  # Colocando os nomes em itálico
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5)) +
  scale_x_continuous(labels = function(x) {
    ifelse(x >= 1000000, paste0(x / 1000000, " Milhão"), paste0(x / 1000, " Mil"))
  }) +
  annotate("text", x = Inf, y = 1.70, hjust = 1, vjust = 0, label = "*Espécies presentes no Anexo II da CITES", size = 3) +
  annotate("text", x = Inf, y = 1.25, hjust = 1, vjust = 0, label = "**Espécie Vulnerável - Portaria MMA 148/22", size = 3)


