##PACOTES NECESSÁRIOS
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
#pra ver se as bases estao boas
export_final_verificando  <- export_final  %>%
  group_by(ano,source,uf_origem) %>%
  summarise(soma_volume = sum(volume))


# Filtrar os dados de acordo com os requisitos especificados para exportacao
dados_filtrados_exp <- export_final %>%
  filter((exportacao_dof == "sim" & exportacao_imaflora == TRUE) |
           (is.na(exportacao_dof) & exportacao_imaflora == TRUE) |
           (exportacao_dof == "sim" & exportacao_imaflora == FALSE))

dados_filtrados <- dados_filtrados_exp %>%
  filter(!(produto_group %in% c("tora", "toretes, estacas, palanques e mouroes", "residuos industriais",
                                "carvao e lenha", "blocos e dormentes", "nao madeireiros florestais",
                                "residuos florestais", "cavacos, lascas, cascas e rachas") &
             exportacao_dof != "sim"))

# Atualizar o dataframe para substituir "apuleia molaris" por "apuleia leiocarpa"
dados_filtrados <- dados_filtrados %>%
  mutate(nome_cientifico = ifelse(nome_cientifico == "apuleia molaris", "apuleia leiocarpa", nome_cientifico))

# 2. Calcular o número de produtos e o volume de cada produto
resumo_produto <- dados_filtrados %>%
  group_by(produto_original,ano, nome_cientifico) %>%
  summarise(total_volume = sum(volume))

# 2. Calcular o número de produtos e o volume de cada produto
resumo_produto_group <- dados_filtrados %>%
  group_by(produto_group) %>%
  summarise(total_volume = sum(volume))

# 3. Calcular o número de espécies e o volume de cada espécie
resumo_especie <- dados_filtrados %>%
  group_by(uf_origem, ano) %>%
  summarise(total_volume = sum(volume))

# Exibir o resumo das espécies
print(resumo_especie)

# Calcular o valor total da coluna "sum"
total_sum <- sum(resumo_especie$total_volume)

# Calcular a porcentagem de cada nome científico em relação ao valor total
resumo_especies <- transform(resumo_especie, percentual = total_volume / total_sum * 100)


# 4. Testar se os produtos e as espécies são os mesmos ao longo do tempo
# Agrupar por ano, produtos e espécies e contar o número de ocorrências
teste_mudanca <- dados_filtrados %>%
  group_by(produto_original, nome_cientifico) %>%
  summarise(ocorrencias = n())

# Exibir o resultado do teste de mudança
print(teste_mudanca)

# Exibir o resultado do teste de mudança
print(teste_mudanca)



# Calcular o valor total da coluna "sum"
total_produto <- sum(resumo_produto_group$total_volume)

# Calcular a porcentagem de cada nome científico em relação ao valor total
resumo_produto_group <- transform(resumo_produto_group, percentual = total_volume / total_produto * 100)

