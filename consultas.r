

# Exportar 10 maiores valores
head(arrange(select(soma_mun_item_contr,nome_do_municipio, soma_vl_item_contrato, valor_sobre_pop, valor_sobre_casos, lic_concluidas),desc(soma_vl_item_contrato)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Exportar 10 maiores contratantes
head(arrange(select(soma_mun_item_contr,nome_do_municipio, soma_vl_item_contrato, valor_sobre_pop, valor_sobre_casos, lic_concluidas),desc(lic_concluidas)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Exportar 10 maiores fornecedores geral
head(arrange(fornecedores,desc(lic_concluidas)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Exportar 10 maiores fornecedores por valor
head(arrange(fornecedores,desc(vl_liquidacao)), n = 20) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Exportar 10 maiores fornecedores por municipio
head(arrange(fornecedores,desc(municipios_contratantes)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Itens mais caros
item_contrato %>%
  filter(flag_servico == 0) %>%
  arrange(desc(vl_item_contrato)) %>%
  select(nome_municipio, vl_item_contrato, qt_itens_contrato, ds_3, ds_item) %>%
  head(n = 30) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Regressão valor_sobre_pop casos_sobre_hab
ggplot(soma_mun_item_contr, aes(valor_sobre_pop, produto_interno_bruto_per_cap)) +
  geom_point(aes(colour = log(casos_sobre_hab))) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  geom_smooth(method = "lm", se= FALSE)

# Regressão #2 valor casos
ggplot(soma_mun_item_contr, aes(log(soma_vl_item_contrato_objetos), log(produto_interno_bruto_a_prec))) +
  geom_point(aes(colour = log(casos_acumulado))) +
  scale_colour_gradientn(colours = terrain.colors(10)) +
  geom_smooth(method = "lm", se= FALSE)

# Municípios com mais casos
soma_mun_item_contr %>%
  mutate(casos_sobre_hab = casos_sobre_hab*1000) %>%
  arrange(desc(casos_sobre_hab)) %>%
  select(nome_do_municipio, casos_sobre_hab, casos_acumulado) %>%
  head(n = 20) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")  

# Municípios com mais óbitos
soma_mun_item_contr %>%
  mutate(obitos_sobre_hab = obitos_sobre_hab*1000) %>%
  unique() %>%
  arrange(desc(obitos_sobre_hab)) %>%
  select(nome_do_municipio, obitos_sobre_hab, obitos_acumulado) %>%
  head(n = 20) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center") 


head(arrange(select(soma_mun_item_contr,nome_do_municipio, soma_vl_item_contrato, valor_sobre_pop, valor_sobre_casos, lic_concluidas),desc(lic_concluidas)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Maiores gastos com remédios
head(arrange(soma_remedios,desc(soma_vl_tota_item_contrato_objetos)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")


# Summaries
n_distinct(item_contrato$cd_municipio_ibge)
n_distinct(item_contrato$id_licitacao)
sum(item_contrato$vl_item_contrato)
n_distinct(empenho$cnpj_cpf)


##
head(arrange(itens_mais_comprados,desc(aquisicoes)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Filtrar maiores discrepâncias
head(arrange(similares_filtrado,desc(diferenca_com_estado_porcentagem)), n = 150) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")


# Encontrar termos mais usados
mesclado <- itens_analise %>%
  mutate(juntador = 1) %>%
  dplyr::group_by(juntador) %>%
  dplyr::summarise(texto = paste(ds_item, collapse = " #! "))

palavras <- strsplit(mesclado$texto, " ")

termos <- as.data.frame(do.call(cbind, palavras)) %>%
  group_by(V1) %>%
  summarise(freq = n())

write.csv(termos, "termos.csv")


# Número de itens (ex.: termometro)
itens_analise %>% filter(categoria_item == "termometro") %>% nrow()
