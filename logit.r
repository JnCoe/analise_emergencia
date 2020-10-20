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

reg_med_cor <- glm(medida_correta ~ log(palavras_perc) + log(price_perc) + bol_nao_unidade + flag_pacote + flag_pre_e_uni, data=aventais2, family=binomial)
summary(reg_med_cor)

reg_material <- glm(material_do_objeto ~ log(palavras_perc) + flag_material, data=aventais2, family=binomial)
summary(reg_material)

reg_det_ad <- glm(detalhes_adicionais ~ log(palavras_perc) + flag_detalhe_ad, data=aventais2, family=binomial)
summary(reg_det_ad)

reg_desc_alem <- glm(descricao_alem_do_objeto ~ log(palavras_perc) , data=aventais2, family=binomial)
summary(reg_det_ad)


aventais2 <- aventais2 %>%
  mutate(estimativa = predict.glm(reg_med_cor, aventais2)) 
  
  
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
  mutate(estimativa_med_cor = predict.glm(reg_med_cor, reg_itens_analise2, type = "response"),
         estimativa_material = predict.glm(reg_material, reg_itens_analise2),
         estimativa_det_ad = predict.glm(reg_det_ad, reg_itens_analise2))

reg_itens_analise2 %>%
  select(id_item_contrato,estimativa_med_cor) %>%
  left_join(select(itens_analise2,id_item_contrato,estimativa_med_cor), by = 'id_item_contrato') %>%
  View()