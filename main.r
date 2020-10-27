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
item_contrato <- data.table::fread("Dados/bd/info_item_contrato.csv", encoding="UTF-8", colClasses=c("id_orgao"="character","nr_contrato"="character"))
info_contrato <- data.table::fread("Dados/bd/info_contrato.csv", encoding="UTF-8", colClasses=c("id_orgao"="character","nr_documento_contratado"="character"))
item_licitacao <- data.table::fread("Dados/bd/info_item_licitacao.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
orgaos <- data.table::fread("Dados/bd/info_orgaos.csv", encoding="UTF-8", colClasses=c("id_orgao"="character", "cd_municipio_ibge" = "character"))
casos <- data.table::fread("Dados/HIST_PAINEL_COVIDBR_19out2020.csv", encoding="UTF-8", colClasses = c("codmun" = "character"))
ibge <- data.table::fread("Dados/IBGE.csv", encoding="UTF-8", colClasses=c("Código do Município"="character"))
pop <- data.table::fread("Dados/populacao.csv", encoding="UTF-8", colClasses=c("Cód."="character"))
empenho <- data.table::fread("Dados/bd/info_empenhos.csv", encoding="UTF-8", colClasses=c("id_orgao"="character","cnpj_cpf"="character"))
similares <- data.table::fread("Dados/itens_similares_covid.csv", encoding="UTF-8")
cnpjs <- data.table::fread("Dados/output_152_09.csv", encoding="UTF-8", sep="#", colClasses=c("cnpj"="character", "cnae_fiscal"="character"))
licitacoes <- data.table::fread("Dados/bd/info_licitacao.csv", encoding="UTF-8")
aventais <- data.table::fread( "Dados/Avental.csv", encoding = "UTF-8", colClasses=c("id_orgao"="character","Medida correta"="numeric"))
load("Dados/tab_cnae.rda")
cnpjs_2019 <- data.table::fread("Dados/filtro2019.txt", encoding="UTF-8", colClasses =c("cnpj" = "character"))

# Corrigir tabela cnpjs novos
cnpjs_2019 <- cnpjs_2019[, c(1:40)]

##### Contratos 2018 e 2019 (não é necessário para demais análises)
# contr_2018 <- data.table::fread("contrato_2018.csv", encoding="UTF-8", colClasses=c("NR_DOCUMENTO"="character"))
# contr_2019 <- data.table::fread("contrato_2019.csv", encoding="UTF-8", colClasses=c("NR_DOCUMENTO"="character"))
# 
# contrs_1819 <- rbind(contr_2018, contr_2019) %>%
#   group_by(NR_DOCUMENTO,CD_ORGAO) %>%
#   summarize(contratos = n()) %>%
#   group_by(NR_DOCUMENTO) %>%
#   summarize(contratos = sum(contratos), contratantes = n())
# 
#   
# rm(contr_2018)
# rm(contr_2019)

# Limpar variáveis
ibge <- janitor::clean_names(ibge)
casos <- janitor::clean_names(casos)
pop <- janitor::clean_names(pop)
casos <- janitor::clean_names(casos)
cnpjs <- janitor::clean_names(cnpjs)
aventais <- janitor::clean_names(aventais)

# Filtrar apenas valores máximos de casos e óbitos
casos <- casos %>% mutate(data = as.Date(data, format="%Y-%m-%d"))
max_casos <- casos %>% group_by(codmun) %>% top_n(1, data)

# Filtrar apenas dados de 2017 do IBGE
ibge <- filter(ibge, ano == 2017)
ibge <- mutate(ibge, cod_sem_ver = substr(codigo_do_municipio, 1,6))


##
# Filtrar apenas dados municipais
item_contrato <- item_contrato %>%
  left_join(select(orgaos,!(home_page) ), by="id_orgao") %>%
  filter(esfera == "MUNICIPAL")

# Código Manoel para identificar serviços
servicos <- item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item , from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "(contratacao de empresa)|(prestacao de servico[s]?)|^servico[s]?|(a prestacao de)|(contratacao de prestacao)|(contratacao de servico)|(aluguel/loca)|servia|(contratacao contratacao de)|(contratacao da empresa)|(contratacao de)|(repasse hospital)|(empenho global)|(locacao de infraestrutura)|(fornecimento de alimentacao)|(fornecimento de gestao)|(contratacao emergencial gerenciamento)|(gerenciamento, operacionalizacao)|(reforma do predio)|(execucao de obras)|(contratacao, de servicos)|convenio")) %>%
  mutate(flag_servico = 1)


# Somar licitações efetivadas
cont_licit_efetivadas <- item_contrato %>%
  group_by(cd_municipio_ibge, id_licitacao) %>%
  summarize(lic_concluidas=n()) %>%
  group_by(cd_municipio_ibge) %>%
  summarize(lic_concluidas=n())

# Somar dados por fornecedor
cnpjs_2019 <- cnpjs_2019 %>%
  mutate(data_inicio_atividade = lubridate::ymd(data_inicio_atividade))

cnpjs <- cnpjs %>%
  mutate(data_inicio_atividade = lubridate::ymd(data_inicio_atividade))

cnpjs_2019 <- cnpjs_2019[!(cnpjs_2019$cnpj %in% cnpjs$cnpj),]

cnpjs2 <- cnpjs %>%
  rbind(cnpjs_2019)

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

#fornecedores %>%
#  select(cnpj_cpf,vl_liquidacao,cnpj_trailed,municipios_contratantes) %>%
#  pgirmess::write.delim("cnpjs.csv", sep = "#")

soma_contratos_fornecedor <- info_contrato %>%
  group_by(nr_documento_contratado) %>%
  summarise(total_contratado = sum(vl_contrato))

fornecedores <- fornecedores %>%
  left_join(cnpjs2, by=c("cnpj_trailed" = "cnpj")) %>%
  left_join(select(tab_cnae,cod_cnae,nm_cnae), by=c("cnae_fiscal" = "cod_cnae")) %>%
  left_join(soma_contratos_fornecedor, by=c("cnpj_trailed" = "nr_documento_contratado"))
  

# Associar itens a cod_mun
item_contrato <- item_contrato %>%
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




##############################################################

# Listar itens mais comprados
itens_epis <- item_contrato %>%
  filter(!(flag_servico==1)) %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
  filter(stringr::str_detect(ds_item, "mascara|aventa|luva|macac|touca|oculos|jaleco|sapatilha|teste|alcoo|termometro|sabonete|oximetro|sabao|detergente|desinfentante|uniforme|tapete|detergente|hipoclorito|sapato|viseira")) %>%
  group_by(ds_1) %>%
  summarize(aquisicoes = n_distinct(id_licitacao), total_comprado = sum(qt_itens_contrato), total_valor_comprado = sum(vl_total_item_contrato))

medianas <- similares_filtrado %>%
  filter(stringr::str_detect(ds_1_item_pesq, "mascara[s]?|teste[s]?|avental?|alcool?|luva[s]?|termometro?|macacao?|touca?|protetor?|oculos?|sabonete?|sabao?|oximetro?|jaleco?|detergente?") & unidade_medida_item_pesq == "UN") %>%
  group_by(ds_1_item_pesq) %>%
  top_n(1, mediana_no_estado) %>%
  select(ds_1_item_pesq, mediana_no_estado) %>%
  unique()
  

#####itens_mais_comprados <-  %>%
#  filter(!(flag_servico==1)) %>%
#  mutate(ds_1 = tolower(iconv(ds_1, from="UTF-8", to="ASCII//TRANSLIT"))) %>%
#  group_by(ds_1) %>%
#  summarize(aquisicoes = n_distinct(id_licitacao), total_comprado = sum(qt_itens_contrato), total_valor_comprado = sum(vl_total_item_contrato)) %>%
#  left_join(medianas, by=c("ds_1" = "ds_1_item_pesq"))


# Exportar planilha
#write.csv(soma_mun_item_contr, "compras_emergenciais.csv")

#write.csv(remedios, "compras_emergenciais.csv")


#fornecedores %>%
#  select(cnpj_cpf,vl_liquidacao,cnpj_trailed,municipios_contratantes) %>%
#  write.csv("cnpjs.csv")

# exportar <- soma_mun_item_contr %>%
#   select(!c("impostos_liquidos_de_subsidi",cod_sem_ver,valor_sobre_pop,soma_vl_item_contrato_objetos,soma_qt_itens_contrato_objetos,valor_sobre_pop,obitos_sobre_hab)) %>%
#   left_join(select(soma_remedios,!c(nome_municipio)), by = c("cd_municipio_ibge"))
# 
# #write.csv(similares_filtrado,"similares.csv")
# 
# soma_mun_item_contr %>%
# summarise(total = sum(soma_vl_item_contrato))

#write.csv(select(fornecedores, -c(cnpj_cpf,tipo_de_registro,indicador,tipo_atualizacao,identificador_matriz_filial,situacao_cadastral,data_situacao_cadastral,motivo_situacao_cadastral,nm_cidade_exterior,cod_pais,nm_pais,qualificacao_responsavel,data_opcao_pelo_simples,data_exclusao_simples,situacao_especial,data_situacao_especial,filler,fim_registro)), "fornecedores.csv")

licitacoes <- licitacoes %>%
  mutate(data_homologacao = substr(data_homologacao,1,10)) %>%
  mutate(data_homologacao = as.Date(data_homologacao, format="%Y-%m-%d"))

total_mes_mun <- licitacoes %>%
  mutate(id_orgao = as.character(id_orgao)) %>%
  left_join(select(orgaos,!(home_page) ), by="id_orgao") %>%
  group_by(mes=lubridate::floor_date(data_homologacao, "month"),cd_municipio_ibge) %>%
  summarize(valor_total_estimado = sum(vl_estimado_licitacao), total_licitacoes=n_distinct(id_licitacao))


##############################################################

#################
# R program to implement 
# Leave one out cross validation 

# defining training control 
# as Leave One Out Cross Validation 
train_control <- caret::trainControl(method = "LOOCV") 



# Importar avental
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


# training the model by assigning sales column 
# as target variable and rest other column 
# as independent varaible 
reg_med_cor <- caret::train(medida_correta ~ log(palavras_perc) + log(price_perc) + bol_nao_unidade + flag_pacote + flag_pre_e_uni, data=aventais2,  
                      method = "glm",
                      family = "quasibinomial",
                      trControl = train_control) 

reg_material <- caret::train(material_do_objeto ~ log(palavras_perc) + flag_material, data=aventais2,  
                             method = "glm",
                             family = "binomial",
                             trControl = train_control) 

reg_det_at <- caret::train(detalhes_adicionais ~ log(palavras_perc) + flag_detalhe_ad, data=aventais2,  
                           method = "glm",
                           family = "binomial",
                           trControl = train_control) 
  
reg_desc_alem <- caret::train(descricao_alem_do_objeto ~ log(palavras_perc), data=aventais2,  
                              method = "glm",
                              family = "binomial",
                              trControl = train_control) 


#! Criar listas
selecao <- c("mascara","aventa","luva","macac","touca","oculos","jaleco","sapatilha","teste","alcoo","sabonete","sabao","detergente","desinfetante","uniforme","detergente","hipoclorito","sapato","viseira")

#OBS.: Reincluir termometro e oxímetro

# Obs.: Removi macacão, protetor, lençol, toalha, jaqueta, blusa, sapato porque havia muita variação no preço e itens muito pouco relacionados

reg_itens_analise <- item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT")))


# Criar indicadores de mais informação
reg_itens_analise <- reg_itens_analise %>%
  mutate(flag_pacote = if_else((stringr::str_detect(ds_item, "pacote|com \\d|c/ \\d|c \\d|\\d un|unidades|und|\\d uni|c/\\d|kg| un |cx|pacote|pct|kg|und|quilo| unid ") & !stringr::str_detect(ds_item, "\\dl|\\d l|\\d ml|\\dml" )),1,0),
         flag_material = if_else(stringr::str_detect(ds_item, "tecido|latex|material|tnt|plastic|plastic|pvc|pff|acrilic|algodao|polipropileno|nitrilica|borracha|polietileno|n95|galvanizado|poliester|inox|vinil|mdf|aluminio|nylon|ferro|policarbonato|nao-tecido|nitrilica|prata|metalica|microfibra|\\(tnt\\)|acrilica"),1,0),
         flag_detalhe_ad = if_else(stringr::str_detect(ds_item, "tamanho|cor|elastico|cm|100%|branco|ambidestra|esteril|impermeavel|gramatura|mg|atoxica|preto|natural|azul|medindo|mm|branca|lubrificada|capacidade|bioabsorvivel|certificado|peso|comprimento|lei|nbr|normas|caracteristicas|costura|dimensoes|bolsos|abnt|flexivel|infantil|anvisa|sanfonada|densidade|certificacao|gramas|inmetro|antiderrapante"),1,0),
         nivel_data2 = as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d"))
  )

# Filtrar pela seleção de itens
reg_itens_analise <- selecao %>%
  purrr::map(function(x) {
    reg_itens_analise %>%
      mutate(selec = stringr::str_detect(ds_item, x), categoria_item = x) %>%
      filter(selec == TRUE)
  }) %>%
  bind_rows() %>%
  distinct(id_item_contrato, .keep_all= TRUE)

# Criar variaveis por item
reg_itens_analise2 <- reg_itens_analise %>%
  filter(vl_item_contrato > 0.1) %>%
  filter(!stringr::str_detect(ds_item, "lavadora de pressao|totem|totens|toten|dispenser")) %>%
  group_by(categoria_item) %>%
  mutate(tamanho_texto = stringr::str_length(ds_item),
         n_palavras = sapply(strsplit(ds_item, " "), length),
         bol_nao_unidade = if_else(sg_unidade_medida == "UN", 0, 1),
         median_palavras = median(n_palavras),
         palavras_perc = n_palavras/median_palavras,
         bol_1_uni = if_else(qt_itens_contrato == 1, 1, 0),
         nivel_data2 = lubridate::floor_date(as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d")),"month")) %>%
  ungroup()

reg_itens_analise2 <- reg_itens_analise2 %>%
  group_by(categoria_item,nivel_data2) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         price_perc = vl_item_contrato/median_price,
         flag_pre_e_uni = if_else(bol_1_uni == 1 & price_perc > 1.5 & bol_nao_unidade == 0, 1, 0)) %>%
  ungroup


# Preencher com valores estimado
reg_itens_analise2 <- reg_itens_analise2 %>%
  mutate(estimativa_med_cor = predict(reg_med_cor, reg_itens_analise2),
         estimativa_material = predict(reg_material, reg_itens_analise2),
         estimativa_det_ad = predict(reg_det_ad, reg_itens_analise2))

reg_itens_analise2 %>%
  select(id_item_contrato,estimativa_med_cor) %>%
  left_join(select(itens_analise2,id_item_contrato,estimativa_med_cor), by = 'id_item_contrato') %>%
  View()

####

