trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)


library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)


# Importar arquivos
item_contrato <- data.table::fread("10_09/info_item_contrato.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
info_contrato <- data.table::fread("10_09/info_contrato.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
item_licitacao <- data.table::fread("10_09/info_item_licitacao.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
orgaos <- data.table::fread("d8ag0132l0g7r7_public_orgao.csv", encoding="UTF-8", colClasses=c("id_orgao"="character", "cd_municipio_ibge" = "character"))
casos <- data.table::fread("HIST_PAINEL_COVIDBR_13set2020.csv", encoding="UTF-8", colClasses = c("codmun" = "character"))
ibge <- data.table::fread("IBGE.csv", encoding="UTF-8", colClasses=c("Código do Município"="character"))
pop <- data.table::fread("populacao.csv", encoding="UTF-8", colClasses=c("Cód."="character"))
empenho <- data.table::fread("10_09/info_empenhos.csv", encoding="UTF-8", colClasses=c("id_orgao"="character","cnpj_cpf"="character"))
similares <- data.table::fread("10_09/itens_similares_covid.csv", encoding="UTF-8")
cnpjs <- data.table::fread("output.csv", encoding="UTF-8", sep="#", colClasses=c("cnpj"="character", "cnae_fiscal"="character"))
licitacoes <- data.table::fread("10_09/info_licitacao.csv", encoding="UTF-8")
aventais <- data.table::fread( "Avental.csv", encoding = "UTF-8", colClasses=c("id_orgao"="character","Medida correta"="numeric"))

# Limpar variáveis
ibge <- janitor::clean_names(ibge)
casos <- janitor::clean_names(casos)
pop <- janitor::clean_names(pop)
casos <- janitor::clean_names(casos)
cnpjs <- janitor::clean_names(cnpjs)
aventais <- janitor::clean_names(aventais)

# Filtrar apenas valores máximos de casos e óbitos
casos <- casos %>% mutate(data = as.Date(data, format="%d/%m/%Y"))
max_casos <- casos %>% group_by(codmun) %>% top_n(1, data)

# Filtrar apenas dados de 2017 do IBGE
ibge <- filter(ibge, ano == 2017)
ibge <- mutate(ibge, cod_sem_ver = substr(codigo_do_municipio, 1,6))


# Código Manoel para identificar serviços
servicos <- item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item , from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "(contratacao de empresa)|(prestacao de servico[s]?)|^servico[s]?|(a prestacao de)|(contratacao de prestacao)|(contratacao de servico)|(aluguel/loca)|servia|(contratacao contratacao de)|(contratacao da empresa)|(contratacao de)|(repasse hospital)|(empenho global)|(locacao de infraestrutura)|(fornecimento de alimentacao)|(fornecimento de gestao)|(contratacao emergencial gerenciamento)|(gerenciamento, operacionalizacao)|(reforma do predio)|(execucao de obras)|(contratacao, de servicos)")) %>%
  mutate(flag_servico = 1)


# Somar licitações efetivadas
cont_licit_efetivadas <- item_contrato %>%
  left_join(select(orgaos,!(home_page) ), by="id_orgao") %>%
  group_by(cd_municipio_ibge, id_licitacao) %>%
  summarize(lic_concluidas=n()) %>%
  group_by(cd_municipio_ibge) %>%
  summarize(lic_concluidas=n())

# Somar dados por fornecedor
fornecedores_mun <- empenho %>%
  mutate(vl_liquidacao = tidyr::replace_na(vl_liquidacao, 0)) %>%
  left_join(select(orgaos,!(home_page) ), by="id_orgao") %>%
  group_by(id_licitacao, cnpj_cpf, cd_municipio_ibge) %>%
  summarize(vl_liquidacao = sum(vl_liquidacao)) %>%
  group_by(cnpj_cpf, cd_municipio_ibge) %>%
  summarize(vl_liquidacao = sum(vl_liquidacao), lic_concluidas=n_distinct(id_licitacao)) %>% 
  group_by(cnpj_cpf) %>%
  summarize(lic_concluidas=sum(lic_concluidas), vl_liquidacao = sum(vl_liquidacao), municipios_contratantes= n_distinct(cd_municipio_ibge)) 


fornecedores <- select(empenho, cnpj_cpf, nm_credor) %>%
  group_by(cnpj_cpf) %>%
  slice(1) %>%
  ungroup() %>%
  right_join(fornecedores_mun) %>%
  group_by(cnpj_cpf) %>%
  summarize(lic_concluidas=sum(lic_concluidas), municipios_contratantes=sum(municipios_contratantes), vl_liquidacao = sum(vl_liquidacao))


fornecedores <- fornecedores %>%
  mutate(cnpj_trailed = stringr::str_pad(cnpj_cpf, 14, pad = "0"))

fornecedores <- fornecedores %>%
  left_join(cnpjs, by=c("cnpj_trailed" = "cnpj")) %>%
  left_join(select(tab_cnae,cod_cnae,nm_cnae), by=c("cnae_fiscal" = "cod_cnae"))

select(tab_cnae,cod_cnae,nm_cnae) %>%
  head()

# Associar itens a cod_mun
item_contrato <- left_join(item_contrato, select(orgaos,!(home_page) ), by="id_orgao") %>%
  left_join(select(servicos,id_licitacao,flag_servico), by = c("id_licitacao")) %>%
  mutate(flag_servico = tidyr::replace_na(flag_servico, 0))

# Remédios
remedios <- item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item , from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "cloroquina|ivermectina|azitromicina")) %>%
  mutate(flag_remedio = 1)

# Somar valores não-serviços
soma_mun_item_contr_filtrados <- item_contrato %>%
  filter(flag_servico == 0) %>%
  group_by(cd_municipio_ibge) %>% 
  summarise(soma_vl_item_contrato_objetos = sum(vl_item_contrato), soma_vl_item_contrato_objetos = sum(vl_item_contrato), soma_qt_itens_contrato_objetos = sum(qt_itens_contrato)) %>%
  unique()

# Somar valores remédios
soma_remedios <- remedios %>%
  group_by(cd_municipio_ibge, nome_municipio) %>% 
  summarise(soma_vl_tota_item_contrato_objetos = sum(vl_total_item_contrato), soma_qt_itens_contrato_objetos = sum(qt_itens_contrato)) %>%
  unique()

# Somar valores contratados todos
soma_mun_item_contr <- item_contrato %>%
  group_by(cd_municipio_ibge) %>% 
  summarise(soma_vl_item_contrato = sum(vl_item_contrato), soma_qt_itens_contrato = sum(qt_itens_contrato)) %>%
  left_join(select(ibge,"codigo_do_municipio","nome_do_municipio","impostos_liquidos_de_subsidi","produto_interno_bruto_a_prec","produto_interno_bruto_per_cap","cod_sem_ver"), by= c("cd_municipio_ibge" = "codigo_do_municipio")) %>%
  left_join(select(pop, "cod", "x2019"), by= c("cd_municipio_ibge" = "cod")) %>%
  left_join(select(max_casos, codmun, casos_acumulado, obitos_acumulado), by= c("cod_sem_ver" = "codmun")) %>%
  left_join(cont_licit_efetivadas, by = c("cd_municipio_ibge")) %>%
  left_join(soma_mun_item_contr_filtrados, by = c("cd_municipio_ibge")) %>%
  unique()

# Calculcar variáveis proporcionais
soma_mun_item_contr <- soma_mun_item_contr %>%
  mutate(valor_sobre_pib = soma_vl_item_contrato/produto_interno_bruto_a_prec) %>%
  mutate(valor_sobre_pop = soma_vl_item_contrato/x2019) %>%
  mutate(valor_sobre_casos = soma_vl_item_contrato/casos_acumulado) %>%
  mutate(valor_sobre_obitos = soma_vl_item_contrato/obitos_acumulado) %>%
  mutate(casos_sobre_hab = casos_acumulado/x2019) %>%
  mutate(obitos_sobre_hab = obitos_acumulado/x2019)

# Filtrar os 24 municípios com valores zerados pagos
soma_mun_item_contr <- filter(soma_mun_item_contr, soma_vl_item_contrato != 0)

# Filtrar similares
similares_filtrado <- similares %>%
  filter(similaridade >= 0.6 & vl_item_pesq != 0)

similares_filtrado <- similares_filtrado %>%
  left_join(select(servicos,id_licitacao,flag_servico), by = c("id_licitacao_item_pesq" = "id_licitacao"))

similares_filtrado <- similares_filtrado %>%
  mutate(ds_1_item_pesq = tolower(iconv(ds_1_item_pesq, from="UTF-8", to="ASCII//TRANSLIT")), ds_2_item_pesq = tolower(iconv(ds_2_item_pesq, from="UTF-8", to="ASCII//TRANSLIT")), ds_3_item_pesq = tolower(iconv(ds_3_item_pesq, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  group_by(id_item_pesquisado) %>%
  top_n(1, id_item_similar) %>%
  mutate(flag_servico = tidyr::replace_na(flag_servico,0)) %>%
  filter(!(flag_servico==1)) %>%
  filter(stringr::str_detect(ds_3_item_pesq, "mascara[s]?|teste[s]?|avental?|alcool?|luva[s]?|termometro?|macacao?|touca?|protetor?|oculos?|sabonete?|sabao?|oximetro?|jaleco?|detergente?|kit?"))


# Listar itens mais comprados
itens_epis <- item_contrato %>%
  filter(!(flag_servico==1)) %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "mascara[s]?|teste[s]?|avental?|alcool?|luva[s]?|termometro?|macacao?|touca?|protetor?|oculos?|sabonete?|sabao?|oximetro?|jaleco?|detergente?")) %>%
  group_by(ds_1) %>%
  summarize(aquisicoes = n_distinct(id_licitacao), total_comprado = sum(qt_itens_contrato), total_valor_comprado = sum(vl_total_item_contrato))

medianas <- similares_filtrado %>%
  filter(stringr::str_detect(ds_1_item_pesq, "mascara[s]?|teste[s]?|avental?|alcool?|luva[s]?|termometro?|macacao?|touca?|protetor?|oculos?|sabonete?|sabao?|oximetro?|jaleco?|detergente?") & unidade_medida_item_pesq == "UN") %>%
  group_by(ds_1_item_pesq) %>%
  top_n(1, mediana_no_estado) %>%
  select(ds_1_item_pesq, mediana_no_estado) %>%
  unique()
  

itens_mais_comprados <- item_contrato %>%
  filter(!(flag_servico==1)) %>%
  mutate(ds_1 = tolower(iconv(ds_1, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  group_by(ds_1) %>%
  summarize(aquisicoes = n_distinct(id_licitacao), total_comprado = sum(qt_itens_contrato), total_valor_comprado = sum(vl_total_item_contrato)) %>%
  left_join(medianas, by=c("ds_1" = "ds_1_item_pesq"))

#####

# Exportar planilha
write.csv(soma_mun_item_contr, "compras_emergenciais.csv")

write.csv(remedios, "compras_emergenciais.csv")


fornecedores %>%
  select(cnpj_cpf,vl_liquidacao,cnpj_trailed,municipios_contratantes) %>%
  write.csv("cnpjs.csv")

exportar <- soma_mun_item_contr %>%
  select(!c("impostos_liquidos_de_subsidi",cod_sem_ver,valor_sobre_pop,soma_vl_item_contrato_objetos,soma_qt_itens_contrato_objetos,valor_sobre_pop,obitos_sobre_hab)) %>%
  left_join(select(soma_remedios,!c(nome_municipio)), by = c("cd_municipio_ibge"))

write.csv(similares_filtrado,"similares.csv")

soma_mun_item_contr %>%
summarise(total = sum(soma_vl_item_contrato))

stargazer::stargazer(as.data.frame(soma_mun_item_contr), type="html")

write.csv(select(fornecedores, -c(cnpj_cpf,tipo_de_registro,indicador,tipo_atualizacao,identificador_matriz_filial,situacao_cadastral,data_situacao_cadastral,motivo_situacao_cadastral,nm_cidade_exterior,cod_pais,nm_pais,qualificacao_responsavel,data_opcao_pelo_simples,data_exclusao_simples,situacao_especial,data_situacao_especial,filler,fim_registro)), "fornecedores.csv")

licitacoes <- licitacoes %>%
  mutate(data_homologacao = substr(data_homologacao,1,10)) %>%
  mutate(data_homologacao = as.Date(data_homologacao, format="%Y-%m-%d"))

total_mes_mun <- licitacoes %>%
  mutate(id_orgao = as.character(id_orgao)) %>%
  left_join(select(orgaos,!(home_page) ), by="id_orgao") %>%
  group_by(mes=lubridate::floor_date(data_homologacao, "month"),cd_municipio_ibge) %>%
  summarize(valor_total_estimado = sum(vl_estimado_licitacao), total_licitacoes=n_distinct(id_licitacao))

item_licitacao %>%
  filter(vl_unitario_estimado == 0) %>%
  summarize(n())


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



aventais2 <- aventais %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "aventa")) %>%
  mutate(flag_pacote = if_else(stringr::str_detect(ds_item, "pacote|kit|com \\d|c/ \\d|c \\d|\\d un|unidades|und|\\d uni|c/\\d|kg| un |cx|pacote|pct|kg|und|quilo| unid "),1,0),
         flag_material = if_else(stringr::str_detect(ds_item, "tecido|latex|material|tnt|plastic|plastic|pvc|pff|acrilic|algodao|polipropileno|nitrilica|borracha|polietileno|n95|galvanizado|poliester|inox|vinil|mdf|aluminio|nylon|ferro|policarbonato|nao-tecido|nitrilica|prata|metalica|microfibra|\\(tnt\\)|acrilica"),1,0),
         flag_detalhe_ad = if_else(stringr::str_detect(ds_item, "tamanho|cor|elastico|cm|100%|branco|ambidestra|esteril|impermeavel|gramatura|mg|atoxica|preto|natural|azul|medindo|mm|branca|lubrificada|capacidade|bioabsorvivel|certificado|peso|comprimento|lei|nbr|normas|caracteristicas|costura|dimensoes|bolsos|abnt|flexivel|infantil|anvisa|sanfonada|densidade|certificacao|gramas|inmetro|antiderrapante"),1,0),
         tamanho_texto = stringr::str_length(ds_item),
         n_palavras = sapply(strsplit(ds_item, " "), length),
         bol_nao_unidade = if_else(sg_unidade_medida == "UN", 0, 1)) %>%
  mutate(vl_item_contrato = gsub("R\\$ ", "" ,vl_item_contrato),
         vl_item_contrato = as.numeric(gsub(",", "", vl_item_contrato))) %>%
  filter(vl_item_contrato > 0.1) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         price_perc = vl_item_contrato/median_price,
         median_text = median(tamanho_texto),
         tamanho_perc = tamanho_texto/median_text,
         median_palavras = median(n_palavras),
         palavras_perc = n_palavras/median_palavras,
         bol_1_uni = if_else(qt_itens_contrato == 1, 1, 0),
         flag_pre_e_uni = if_else(bol_1_uni == 1 & price_perc > 1.5 & bol_nao_unidade == 0, 1, 0))

reg_med_cor <- lm(medida_correta ~ log(palavras_perc) + log(price_perc) + bol_nao_unidade + flag_pacote + flag_pre_e_uni, data=aventais2)
summary(reg_med_cor)

reg_material <- lm(material_do_objeto ~ log(palavras_perc) + flag_material, data=aventais2)
summary(reg_material)

reg_det_ad <- lm(detalhes_adicionais ~ log(palavras_perc) + flag_detalhe_ad, data=aventais2)
summary(reg_det_ad)


aventais2 <- aventais2 %>%
  mutate(estimativa = predict(reg_med_cor, aventais2)) %>%
  

#! Criar listas
selecao <- c("mascara","aventa","luva","macac","touca","protetor","oculos","jaleco","sapatilha","teste","alcoo","termometro","sabonete","oximetro","sabao","detergente","desinfentante","uniforme","tapete","detergente","hipoclorito","sapato","viseira","blusa","jaqueta")

# Obs.: Removi macacão, protetor, lençol, toalha


itens_analise <- item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT")))


# Criar indicadores de mais informação
itens_analise <- itens_analise %>%
  mutate(flag_pacote = if_else(stringr::str_detect(ds_item, "pacote|kit|com \\d|c/ \\d|c \\d|\\d un|unidades|und|\\d uni|c/\\d|kg| un |cx|pacote|pct|kg|und|quilo| unid "),1,0),
         flag_material = if_else(stringr::str_detect(ds_item, "tecido|latex|material|tnt|plastic|plastic|pvc|pff|acrilic|algodao|polipropileno|nitrilica|borracha|polietileno|n95|galvanizado|poliester|inox|vinil|mdf|aluminio|nylon|ferro|policarbonato|nao-tecido|nitrilica|prata|metalica|microfibra|\\(tnt\\)|acrilica"),1,0),
         flag_detalhe_ad = if_else(stringr::str_detect(ds_item, "tamanho|cor|elastico|cm|100%|branco|ambidestra|esteril|impermeavel|gramatura|mg|atoxica|preto|natural|azul|medindo|mm|branca|lubrificada|capacidade|bioabsorvivel|certificado|peso|comprimento|lei|nbr|normas|caracteristicas|costura|dimensoes|bolsos|abnt|flexivel|infantil|anvisa|sanfonada|densidade|certificacao|gramas|inmetro|antiderrapante"),1,0),
         nivel_data2 = as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d"))
                              )

  

# Filtrar pela seleção de itens
itens_analise <- selecao %>%
  purrr::map(function(x) {
    itens_analise %>%
      mutate(selec = stringr::str_detect(ds_item, x), categoria_item = x) %>%
      filter(selec == TRUE)
  }) %>%
  bind_rows() %>%
  distinct(id_item_contrato, .keep_all= TRUE)

# Criar variaveis por item
itens_analise2 <- itens_analise %>%
  filter(vl_item_contrato > 0.1) %>%
  group_by(categoria_item) %>%
  mutate(tamanho_texto = stringr::str_length(ds_item),
         n_palavras = sapply(strsplit(ds_item, " "), length),
         bol_nao_unidade = if_else(sg_unidade_medida == "UN", 0, 1),
         median_price = median(vl_item_contrato, na.rm=T),
         price_perc = vl_item_contrato/median_price,
         median_palavras = median(n_palavras),
         palavras_perc = n_palavras/median_palavras,
         bol_1_uni = if_else(qt_itens_contrato == 1, 1, 0),
         flag_pre_e_uni = if_else(bol_1_uni == 1 & price_perc > 1.5 & bol_nao_unidade == 0, 1, 0),
         nivel_data2 = lubridate::floor_date(as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d")),"month")) %>%
  ungroup()

itens_mensal <- itens_analise2 %>%
  filter(sg_unidade_medida == "UN") %>%
  group_by(categoria_item, nivel_data2) %>%
  summarize(median_price = median(vl_item_contrato, na.rm=T), compras = n(), soma_gastos = sum(vl_item_contrato))

write.csv(itens_mensal, "itens_mensal.csv")

# Preencher com valores estimado
itens_analise2 <- itens_analise2 %>%
  mutate(estimativa_med_cor = predict(reg_med_cor, itens_analise2),
         estimativa_material = predict(reg_material, itens_analise2),
         estimativa_det_ad = predict(reg_det_ad, itens_analise2))





itens_analise2 %>% filter(categoria_item == "mascara" & sg_unidade_medida == "UN") %>%
  write.csv("mascaras.csv")

itens_analise2 %>% filter(estimativa_med_cor < & sg_unidade_medida == "UN") %>%
  write.csv("mascaras.csv")


























aventais2 <- aventais %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "aventa?|")) %>%
  mutate(flag_pacote = if_else(stringr::str_detect(ds_item, "pacote[s]?|kit[s]?|com \\d|c/ \\d|c \\d|\\d un|\\d unidades|\\d und|\\d uni"),1,0),
         tamanho_texto = stringr::str_length(ds_item),
         n_palavras = sapply(strsplit(ds_item, " "), length),
         bol_nao_unidade = if_else(sg_unidade_medida == "UN", 0, 1)) %>%
  mutate(vl_item_contrato = gsub("R\\$ ", "" ,vl_item_contrato),
         vl_item_contrato = as.numeric(gsub(",", "", vl_item_contrato))) %>%
  filter(vl_item_contrato > 0.1) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         price_perc = vl_item_contrato/median_price,
         median_text = median(tamanho_texto),
         tamanho_perc = tamanho_texto/median_text,
         median_palavras = median(n_palavras),
         palavras_perc = n_palavras/median_palavras)