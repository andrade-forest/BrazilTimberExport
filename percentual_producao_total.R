
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
  mutate(nome_cientifico = ifelse(nome_cientifico == "apuleia molaris", "apuleia leiocarpa", nome_cientifico))verificacao <- dados_classificados %>%
  group_by(nome_cientifico, Mercado,ano) %>%
  summarise(Volume_Total_Ano = sum(volume))

# Agrupando por nome científico e mercado e somando o volume total
total_por_especie_mercado <- verificacao %>%
  group_by(nome_cientifico, Mercado) %>%
  summarise(Volume_total = sum(Volume_Total_Ano)) %>%
  ungroup()

# Calculando o total por espécie
total_por_especie <- total_por_especie_mercado %>%
  group_by(nome_cientifico) %>%
  summarise(Total = sum(Volume_total))

# Mesclando os totais por espécie com os totais por espécie e mercado
total_por_especie_mercado <- left_join(total_por_especie_mercado, total_por_especie, by = "nome_cientifico")

# Calculando o percentual do total de cada mercado por espécie
total_por_especie_mercado <- total_por_especie_mercado %>%
  mutate(Percentual_do_total = (Volume_total / Total) * 100)

# Exibindo o dataframe com a nova coluna
print(total_por_especie_mercado)
