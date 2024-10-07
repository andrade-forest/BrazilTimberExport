##Verificando ssp
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Combine todos os dataframes em um único dataframe
#df_final <- do.call(rbind, mget(ls(pattern = "^exportacao_\\d{4}$")))

##filtrar
export_final <- df_final %>%
  filter((uf_origem %in% c("PA") & ano %in% c(2010:2020) & source == "Sema-PA") |
           (uf_origem %in% c("MT") & ano %in% c(2010:2023) & source == "Sema-MT") |
           (uf_origem != "PA" & uf_origem != "MT" & source == "DOF" & ano %in% c(2010:2023)) |
           (uf_origem %in% c("PA") & source == "DOF" & ano %in% c(2021:2023)))


export_final_tora <- export_final %>%
  filter((produto_group == "tora"))


# Supondo que export_final_tora seja o dataframe que contém os dados

# Lista das espécies desejadas em minúsculas
especies_desejadas <- c(
  'handroanthus barbatus', 'tabebuia aurea', 'dipteryx charapilla', 'cedrela fissilis',
  'handroanthus capitatus', 'tabebuia fluviatilis', 'dipteryx ferrea', 'cedrela odorata',
  'handroanthus impetiginosus', 'tabebuia insignis', 'dipteryx magnifica',
  'handroanthus incanus', 'tabebuia pilosa', 'dipteryx micrantha',
  'handroanthus obscurus', 'dipteryx odorata',
  'handroanthus serratifolius', 'dipteryx polyphylla',
  'handroanthus uleanus', 'dipteryx punctata',
  'handroanthus ochraceus', 'dipteryx rosea'
)


# Filtrando o dataframe
export_final_tora_filtrado <- export_final_tora[export_final_tora$nome_cientifico %in% especies_desejadas, ]

# Agora export_final_tora_filtrado contém apenas as linhas com as espécies desejadas em minúsculas

# Filtrando o dataframe
export_final_tora_filtrado <- export_final_tora[export_final_tora$nome_cientifico %in% especies_desejadas, ]

# Agora export_final_tora_filtrado contém apenas as linhas com as espécies desejadas


#pra ver se as bases estao boas
ssp_cites  <- export_final_tora_filtrado  %>%
  group_by(ano,nome_cientifico) %>%
  summarise(soma_volume = sum(volume))


# Definindo a paleta de cores manualmente
cores <- c("#8B4513", "#A0812D", "#CD853F", "#FF4500", "#FF6347", "#FF7F50", "#FFA07A", "#FF8C00", 
                    "#B22222", "#DC143C", "#FF0000", "#8B0000", "#FFFF00", "#FFD700", "#FFA500", "#DAA520")
                    
# Criando o gráfico de linhas com os ajustes solicitados
ggplot(ssp_cites, aes(x = ano, y = soma_volume, color = reorder(nome_cientifico, -soma_volume))) +
  geom_line() +
  labs(x = "Ano", y = "Volume (MIL)", title = "Volume ao longo dos anos para cada espécie") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Colocando a legenda embaixo
  scale_color_manual(values = cores, name = "Espécie") +  # Definindo as cores manualmente
  scale_y_continuous(labels = scales::comma, name = "Volume (Mil)") +  # Ajustando a legenda do volume
  scale_x_continuous(breaks = seq(ano_min, ano_max, by = 1))  # Ajustando o eixo x para mostrar ano a ano


# Criando o gráfico de barras empilhadas com os ajustes solicitados
ggplot(ssp_cites, aes(x = ano, y = soma_volume, fill = nome_cientifico)) +
  geom_bar(stat = "identity") +
  labs(x = "Ano", y = "Volume m (MIL)", title = "Volume de madeira em tora das espécies presentes no anexo II da CITES") +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Colocando a legenda embaixo
  scale_fill_manual(values = cores, name = "Espécie") +  # Definindo as cores manualmente
  scale_y_continuous(labels = scales::comma, name = "Volume m³ (Mil)") +  # Ajustando a legenda do volume
  scale_x_continuous(breaks = seq(ano_min, ano_max, by = 1))  # Ajustando o eixo x para mostrar ano a ano


