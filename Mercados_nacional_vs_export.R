
# Tirando todos os valores desnecessários, exceto aqueles em que exportacao_dof é "sim"
total_exp <- df_final %>%
  filter(!(produto_group %in% c("tora", "toretes, estacas, palanques e mouroes", "residuos industriais",
                                "carvao e lenha", "blocos e dormentes", "nao madeireiros florestais",
                                "residuos florestais", "cavacos, lascas, cascas e rachas") &
             exportacao_dof != "sim"))

##filtrar
export_final <- total_exp %>%
  filter((uf_origem %in% c("PA") & ano %in% c(2010:2020) & source == "Sema-PA") |
           (uf_origem %in% c("MT") & ano %in% c(2010:2023) & source == "Sema-MT") |
           (uf_origem != "PA" & uf_origem != "MT" & source == "DOF" & ano %in% c(2010:2023)) |
           (uf_origem %in% c("PA") & source == "DOF" & ano %in% c(2021:2023)))

#pra ver se as bases estao boas
export_final_verificando  <- export_final  %>%
  group_by(ano,source,uf_origem) %>%
  summarise(soma_volume = sum(volume))


# Classificar os dados em Exportação e Nacional
dados_classificados <- export_final %>%
  mutate(Mercado = if_else((exportacao_dof == "sim" & exportacao_imaflora == TRUE) |
                             (is.na(exportacao_dof) & exportacao_imaflora == TRUE) |
                             (exportacao_dof == "sim" & exportacao_imaflora == FALSE),
                           "Exportação", "Nacional", missing = "Nacional"))

# Atualizar o dataframe para substituir "apuleia molaris" por "apuleia leiocarpa"
dados_classificados <- dados_classificados%>%
  mutate(nome_cientifico = ifelse(nome_cientifico == "apuleia molaris", "apuleia leiocarpa", nome_cientifico))


# Calcular a soma dos valores onde o mercado é "Exportação"
soma_exportacao <- sum(dados_classificados$volume[dados_classificados$Mercado == "Exportação"], na.rm = TRUE)

# Agrupar os dados por ano e mercado, e calcular a soma do volume para cada grupo
dados_agrupados <- aggregate(volume ~  Mercado + ano, data = dados_classificados, sum)


# Criar o gráfico de barras
ggplot(dados_agrupados, aes(x = factor(ano), y = volume/1e6, fill = Mercado)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "Ano", y = "Volume m³ (milhões)",
       fill = "Mercado") +
  scale_x_discrete(labels = as.character(dados_agrupados$ano), name = "Ano") +
  scale_fill_manual(values = c("#D2691E", "#9B4513"), name = "Mercado") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_y_continuous(labels = scales::comma)  # Mostra os valores no eixo y em milhõe


# Calcular o volume total
volume_total <- sum(dados_agrupados$volume)

# Agrupar os dados por ano e mercado e calcular o volume total para cada ano
dados_agrupados_2 <- dados_agrupados %>%
  group_by(ano, Mercado, UF) %>%
  summarise(volume = sum(volume))

# Calcular o volume total para cada ano
verificacao <- dados_classificados %>%
  group_by(ano,produto_group) %>%
  summarise(Volume_Total_Ano = sum(volume))

# Mesclar os dados agrupados com o volume total por ano
dados_agrupados <- left_join(dados_agrupados, volume_total_por_ano, by = "ano")

# Calcular o percentual do mercado de exportação em relação ao volume total para cada ano
dados_agrupados <- dados_agrupados %>%
  mutate(Percentual_Exportacao = (volume[which(Mercado == "Exportação")] / Volume_Total_Ano) * 100)

# Calcular o percentual do mercado nacional em relação ao volume total para cada ano
dados_agrupados <- dados_agrupados %>%
  mutate(Percentual_Nacional = (volume[which(Mercado == "Nacional")] / Volume_Total_Ano) * 100)

# Selecionar apenas as colunas necessárias
tabela_percentual <- dados_agrupados %>%
  select(ano, Mercado, Percentual_Exportacao, Percentual_Nacional)

# Exibir a tabela
print(tabela_percentual)

# 3. Calcular o número de espécies e o volume de cada espécie
resumo_especie <- dados_classificados %>%
  group_by(Mercado) %>%
  summarise(total_volume = sum(volume))


############
# Calcular o volume total por ano
dados_agrupados <- dados_agrupados %>%
  group_by(ano) %>%
  mutate(Volume_Total_Ano = sum(volume)) %>%
  ungroup()

# Calcular o percentual do mercado de exportação em relação ao volume total para cada ano
dados_agrupados <- dados_agrupados %>%
  mutate(Percentual_Exportacao = if_else(Mercado == "Exportação",
                                         (volume / Volume_Total_Ano) * 100,
                                         NA_real_)) 

# Calcular o percentual do mercado nacional em relação ao volume total para cada ano
dados_agrupados <- dados_agrupados %>%
  mutate(Percentual_Nacional = if_else(Mercado == "Nacional",
                                       (volume / Volume_Total_Ano) * 100,
                                       NA_real_))

# Selecionar apenas as colunas necessárias
tabela_percentual <- dados_agrupados %>%
  select(ano, Mercado, Percentual_Exportacao, Percentual_Nacional)

# Exibir a tabela
print(tabela_percentual)
# Criar um dataframe com os dados agrupados para volume por ano da exportação e do mercado nacional
dados_tabela <- data.frame(
  Ano = unique(dados_agrupados$ano), # Obter os anos únicos do dataframe agrupado
  Exportacao_Madeira_Tropical = dados_agrupados$volume[dados_agrupados$Mercado == "Exportação"],
  Nacional_Madeira_Tropical = dados_agrupados$volume[dados_agrupados$Mercado == "Nacional"])

# Exibir a tabela
print(dados_tabela)

