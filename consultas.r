pal <- RColorBrewer::brewer.pal(name = "Set2", n = 7)

#Contabilizar

length(unique(item_contrato$cd_municipio_ibge))

length(unique(item_contrato$id_licitacao))

sum(item_contrato$vl_total_item_contrato)

sandbox <- info_contrato %>%
  right_join(item_contrato, by=c("id_licitacao")) %>%
  distinct(nr_documento_contratado)

length(unique(item_contrato$id_licitacao))

# tabela descritiva
# Summary
soma_mun_item_contr %>%
  filter(!is.na(cd_municipio_ibge)) %>%
  select(soma_vl_item_contrato, x2019, casos_acumulado, obitos_acumulado, lic_concluidas, valor_sobre_pib, valor_sobre_pop, casos_sobre_hab, obitos_sobre_hab) %>%
  as.data.frame() %>%
  stargazer::stargazer(type = "html")

# regressão casos dinheiro
summary(lm(soma_vl_item_contrato ~ casos_acumulado, data=soma_mun_item_contr))
summary(lm(soma_vl_item_contrato ~ casos_sobre_hab, data=soma_mun_item_contr))

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
fornecedores %>%
  filter(!is.na(razao_social)) %>%
  arrange(desc(vl_liquidacao)) %>%
  head(n = 20) %>%
  select(lic_concluidas,municipios_contratantes,vl_liquidacao,razao_social,data_inicio_atividade,nm_cnae) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# Exportar fornecedores mais recentes
fornecedores %>%
  filter(total_contratado > 5000) %>%
  arrange(desc(data_inicio_atividade)) %>%
  head(n = 20) %>%
  select(razao_social,municipios_contratantes,total_contratado,data_inicio_atividade,nm_cnae) %>%
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
  arrange(desc(casos_acumulado)) %>%
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



# Reme


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

# Exportar aventais
item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "aventa?|")) %>%
  mutate(flag_pacote = if_else(stringr::str_detect(ds_item, "pacote[s]?|kit[s]?|com \\d|c/ \\d|c \\d|\\d un|\\d unidades|\\d und|\\d uni"),1,0),
         tamanho_texto = stringr::str_length(ds_item),
         n_palavras = sapply(strsplit(ds_item, " "), length),
         bol_nao_unidade = if_else(sg_unidade_medida == "UN", 0, 1)) %>%
  filter(vl_item_contrato < 0.2) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         price_perc = vl_item_contrato/median_price,
         median_text = median(tamanho_texto),
         tamanho_perc = tamanho_texto/median_text) %>%
  write.csv("aventais.csv")


###
itens_analise2 %>% filter(categoria_item == "mascara" & sg_unidade_medida == "UN") %>%
  write.csv("mascaras.csv")

itens_analise2 %>% filter(estimativa_med_cor < & sg_unidade_medida == "UN") %>%
  write.csv("mascaras.csv")


##
itens_mensal <- itens_analise2 %>%
  filter(sg_unidade_medida == "UN" & flag_servico != 1 & esfera == "MUNICIPAL") %>%
  group_by(categoria_item, nivel_data2) %>%
  summarize(median_price = median(vl_item_contrato, na.rm=T), compras = n(), soma_gastos = sum(vl_item_contrato), soma_unidades = sum(qt_itens_contrato))

write.csv(itens_mensal, "itens_mensal.csv")

 

##
itens_analise2 %>%
  filter(categoria_item == "detergente") %>%
  View()


# Número de itens (ex.: termometro)
itens_analise %>% filter(categoria_item == "termometro") %>% nrow




# Contratos 2018 e 2019
fornecedores %>%
  left_join(contrs_1819, by=c("cnpj_trailed" = "NR_DOCUMENTO")) %>%
  mutate(contratos = tidyr::replace_na(contratos, 0), contratantes = tidyr::replace_na(contratantes, 0))%>%
  filter(contratos == 0) %>%
  select(cnpj_trailed,lic_concluidas,municipios_contratantes,vl_liquidacao,razao_social,data_inicio_atividade,nm_cnae,contratos,contratantes) %>%
  filter(is.na(razao_social)) %>%
  View()
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

  
info_contrato %>%
  filter(nr_documento_contratado == "34064557000108") %>%
  View()


# Itens_analise

reg_itens_analise2 %>%
  filter(!flag_servico == 1) %>%
  group_by(categoria_item) %>%
  mutate(total_cat = n(), total_valor_cat = sum(vl_total_item_contrato)) %>%
  filter(estimativa_med_cor < 0.4) %>%
  mutate(total_med_inc = n(),
         porc_inc = total_med_inc/total_cat,
         valor_inc = sum(vl_total_item_contrato),
         perc_valor_inc = valor_inc/total_valor_cat) %>%
  select(categoria_item, total_med_inc, porc_inc, valor_inc, perc_valor_inc) %>%
  unique() %>%
  arrange(desc(valor_inc)) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

  #total
reg_itens_analise2 %>%
  filter(!flag_servico == 1) %>%
  mutate(total_cat = n(), total_valor_cat = sum(vl_total_item_contrato)) %>%
  mutate(total_med_inc = n(),
         porc_inc = total_med_inc/total_cat,
         valor_inc = sum(vl_total_item_contrato),
         perc_valor_inc = valor_inc/total_valor_cat) %>%
  select(total_med_inc, porc_inc, valor_inc, perc_valor_inc) %>%
  unique() %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

  # Cidades
reg_itens_analise2 %>%
  filter(!categoria_item %in% c("macac","teste","jaleco","alcoo")) %>%
  filter(!flag_servico == 1) %>%
  group_by(nome_municipio) %>%
  mutate(total_mun = n(), total_valor_mun = sum(vl_total_item_contrato)) %>%
  filter(estimativa_med_cor < 0.4) %>%
  mutate(total_med_inc = n(),
         porc_inc = total_med_inc/total_mun,
         valor_inc = sum(vl_total_item_contrato),
         perc_valor_inc = valor_inc/total_valor_mun) %>%
  select(nome_municipio, total_med_inc, porc_inc, valor_inc, perc_valor_inc) %>%
  unique() %>%
  arrange(desc(total_med_inc)) %>%
  head(n=10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")


# Itens mensais
itens_analise2 %>%
  group_by(categoria_item,nivel_data2) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
          compras = n(),
         soma_gastos = sum(vl_item_contrato),
         soma_unidades = sum(qt_itens_contrato)) %>%
  select(categoria_item,nivel_data2,median_price,compras,soma_gastos,soma_unidades) %>%
  unique() %>%
  write.csv("itens_mensal.csv")

# Grafico compras mes
reg_itens_analise2 %>%
  group_by(categoria_item,nivel_data2) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         compras = n(),
         soma_gastos = sum(vl_item_contrato),
         soma_unidades = sum(qt_itens_contrato)) %>%
  select(categoria_item,nivel_data2,median_price,compras,soma_gastos,soma_unidades) %>%
  unique() %>%
  filter(categoria_item %in% c("alcoo", "mascara", "luva", "teste") & nivel_data2>as.Date("2020-01-02") & nivel_data2<as.Date("2020-10-01")) %>%
  ggplot(aes(x=nivel_data2, y=compras, group=categoria_item, color=categoria_item)) +
  scale_color_manual(labels = c("Álcool", "Luva", "Máscara", "Teste"), values=pal) +
  labs(colour = "Item") + xlab("Mês") + ylab("Número de contratos") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  geom_line()

# Grafico compras mes remedios
remedios %>%
  group_by(categoria_item,nivel_data2) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         compras = n(),
         soma_gastos = sum(vl_item_contrato),
         soma_unidades = sum(qt_itens_contrato)) %>%
  select(categoria_item,nivel_data2,median_price,compras,soma_gastos,soma_unidades) %>%
  unique() %>%
  filter(nivel_data2>as.Date("2020-01-02") & nivel_data2<as.Date("2020-10-01")) %>%
  ggplot(aes(x=nivel_data2, y=compras, group=categoria_item, color=categoria_item)) +
  labs(colour = "Item") + xlab("Mês") + ylab("Número de contratos") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  geom_line()



# Grafico valor medio mes
reg_itens_analise2 %>%
  group_by(categoria_item,nivel_data2) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         compras = n(),
         soma_gastos = sum(vl_item_contrato),
         soma_unidades = sum(qt_itens_contrato)) %>%
  select(categoria_item,nivel_data2,median_price,compras,soma_gastos,soma_unidades) %>%
  unique() %>%
  filter(categoria_item %in% c("alcoo", "mascara", "luva", "teste") & nivel_data2>as.Date("2020-01-02") & nivel_data2<as.Date("2020-10-01")) %>%
  ggplot(aes(x=nivel_data2, y=median_price, group=categoria_item, color=categoria_item)) +
  scale_color_manual(labels = c("Álcool", "Luva", "Máscara", "Teste"), values=pal) +
  labs(colour = "Item") + xlab("Mês") + ylab("Mediana do preço (escala logarítmica)") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  scale_y_continuous(trans='log10') +
  geom_line()



# Grafico total mês
item_contrato %>%
  mutate(nivel_data2 = lubridate::floor_date(as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d")),"month")) %>%
  group_by(nivel_data2, nome_municipio) %>%
  summarise(total_mun = sum(vl_total_item_contrato)) %>%
  group_by(nivel_data2) %>%
  summarise(media_mun = mean(total_mun),
            total_mensal = sum(total_mun)) %>%
  filter(nivel_data2>as.Date("2020-01-02") & nivel_data2<as.Date("2020-10-01")) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")
  
  tidyr::gather(tipo, valor, media_mun:total_mensal) %>%
  ggplot(aes(x=nivel_data2, y=valor, color=tipo)) +
  scale_color_manual(values=pal) +
  labs(colour = "Item") + xlab("Mês") + ylab("Mediana do preço") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  scale_y_continuous(labels=scales::dollar_format(prefix="R$")) +
  geom_line()

item_contrato %>%
  mutate(nivel_data2 = lubridate::floor_date(as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d")),"month")) %>%
  group_by(nivel_data2) %>%
  summarise(median_price = median(vl_total_item_contrato, na.rm=T),
            compras = n(),
            soma_gastos = sum(vl_total_item_contrato),
            n_municipio = n_distinct(cd_municipio_ibge)) %>%
  filter(nivel_data2>as.Date("2020-01-02") & nivel_data2<as.Date("2020-10-01")) %>%
  unique() %>%
  tidyr::gather(tipo, valor, median_price:n_municipio) %>%
  ggplot(aes(x=nivel_data2, y=valor, color=tipo)) +
  scale_color_manual(values=pal) +
  labs(colour = "Item") + xlab("Mês") + ylab("Mediana do preço") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  scale_y_continuous(labels=scales::dollar_format(prefix="R$"), trans='log10') +
  geom_line()

# Soma Municipios 
item_contrato %>%
  mutate(nivel_data2 = lubridate::floor_date(as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d")),"month")) %>%
  group_by(nivel_data2) %>%
  summarise(compras = n(),
            n_municipio = n_distinct(cd_municipio_ibge)) %>%
  filter(nivel_data2>as.Date("2020-01-02") & nivel_data2<as.Date("2020-10-01")) %>%
  unique() %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

# N municipios
item_contrato %>%
  mutate(nivel_data2 = lubridate::floor_date(as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d")),"month")) %>%
  group_by(nivel_data2) %>%
  summarise(compras = n(),
            n_municipio = n_distinct(cd_municipio_ibge)) %>%
  filter(nivel_data2>as.Date("2020-01-02") & nivel_data2<as.Date("2020-10-01")) %>%
  unique() %>%
  tidyr::gather(tipo, valor, compras:n_municipio) %>%
  View()
  ggplot(aes(x=nivel_data2, y=valor, color=tipo)) +
  scale_color_manual(values=pal) +
  labs(colour = "Item") + xlab("Mês") + ylab("Mediana do preço") +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") +
  scale_y_continuous(labels=scales::dollar_format(prefix="R$"), trans='log10') +
  geom_line()


# material 
reg_itens_analise2 %>%
  group_by(categoria_item) %>%
  mutate(total_cat = n(), total_valor_cat = sum(vl_total_item_contrato)) %>%
  filter(estimativa_med_cor < 0.4) %>%
  mutate(total_med_inc = n(),
         porc_inc = total_med_inc/total_cat,
         valor_inc = sum(vl_total_item_contrato),
         perc_valor_inc = valor_inc/total_valor_cat) %>%
  select(categoria_item, total_med_inc, porc_inc, valor_inc, perc_valor_inc) %>%
  unique() %>%
  arrange(desc(valor_inc)) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")


reg_itens_analise2 %>%
  filter(categoria_item %in% c("mascara", "luva", "aventa", "jaleco", "touca")) %>%
  group_by(categoria_item) %>%
  mutate(total_cat = n(),
         total_valor_cat = sum(vl_total_item_contrato),
         sem_mat = total_cat - sum(flag_material),
         porc_sem_mat = (sem_mat/total_cat),
         valor_sem_mat = (total_valor_cat - sum(flag_material*vl_total_item_contrato)),
         porc_valor_sem_mat = (valor_sem_mat/total_valor_cat)) %>%
  select(categoria_item,sem_mat,porc_sem_mat,valor_sem_mat,porc_valor_sem_mat,total_valor_cat) %>%
  unique() %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")

reg_itens_analise2 %>%
  filter(categoria_item %in% c("mascara", "luva", "aventa", "jaleco", "touca")) %>%
  View()

#totais materiais
itens_analise2 %>%
  filter(categoria_item %in% c("mascara", "luva", "aventa", "jaleco", "touca")) %>%
  mutate(total_cat = n(),
         total_valor_cat = sum(vl_total_item_contrato)) %>%
  select(total_cat,total_valor_cat) %>%
  unique %>%
  View()
  

# Modalidades
licitacoes %>%
  filter(tipo_modalidade_licitacao == 'Pregão Presencial' | tipo_modalidade_licitacao == 'Pregão Eletrônico') %>%
  select(id_licitacao) %>%
  left_join(info_contrato) %>%
  View()


# Totais itens analisados estimativa
reg_itens_analise2 %>%
  select(id_item_contrato) %>%
  unique() %>%
  nrow()

item_contrato %>%
  select(id_item_contrato) %>%
  unique() %>%
  nrow()



soma_mun_item_contr %>%
  filter(!is.na(cd_municipio_ibge)) %>%
  select(cd_municipio_ibge,soma_vl_item_contrato, x2019, casos_acumulado, obitos_acumulado, lic_concluidas, valor_sobre_pib, valor_sobre_pop, casos_sobre_hab, obitos_sobre_hab) %>%
  as.data.frame() %>%
  View()