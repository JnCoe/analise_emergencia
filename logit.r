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

reg2_med_cor <- lm(medida_correta ~ log(palavras_perc) + log(price_perc) + bol_nao_unidade + flag_pacote + flag_pre_e_uni, data=aventais2)
summary(reg2_med_cor)

summary(reg_med_cor)

reg2_material <- lm(material_do_objeto ~ log(palavras_perc) + flag_material, data=aventais2)
summary(reg2_material)

reg2_det_ad <- lm(detalhes_adicionais ~ log(palavras_perc) + flag_detalhe_ad, data=aventais2)
summary(reg2_det_ad)

reg2_desc_alem <- lm(descricao_alem_do_objeto ~ log(palavras_perc) , data=aventais2)
summary(reg2_det_ad)


aventais2 <- aventais2 %>%
  mutate(estimativa = predict(reg2_med_cor, aventais2)) 
  
  
  #! Criar listas
  selecao <- c("mascara","aventa","luva","macac","touca","oculos","jaleco","sapatilha","teste","alcoo","sabonete","sabao","detergente","desinfetante","uniforme","detergente","hipoclorito","sapato","viseira")

#OBS.: Reincluir termometro e oxímetro

# Obs.: Removi macacão, protetor, lençol, toalha, jaqueta, blusa, sapato porque havia muita variação no preço e itens muito pouco relacionados

reg3_itens_analise <- item_contrato %>%
  mutate(ds_item = tolower(iconv(ds_item, from="UTF-8", to="ASCII//TRANSLIT")))


# Criar indicadores de mais informação
reg3_itens_analise <- reg2_itens_analise %>%
  mutate(flag_pacote = if_else((stringr::str_detect(ds_item, "pacote|com \\d|c/ \\d|c \\d|\\d un|unidades|und|\\d uni|c/\\d|kg| un |cx|pacote|pct|kg|und|quilo| unid ") & !stringr::str_detect(ds_item, "\\dl|\\d l|\\d ml|\\dml" )),1,0),
         flag_material = if_else(stringr::str_detect(ds_item, "tecido|latex|material|tnt|plastic|plastic|pvc|pff|acrilic|algodao|polipropileno|nitrilica|borracha|polietileno|n95|galvanizado|poliester|inox|vinil|mdf|aluminio|nylon|ferro|policarbonato|nao-tecido|nitrilica|prata|metalica|microfibra|\\(tnt\\)|acrilica"),1,0),
         flag_detalhe_ad = if_else(stringr::str_detect(ds_item, "tamanho|cor|elastico|cm|100%|branco|ambidestra|esteril|impermeavel|gramatura|mg|atoxica|preto|natural|azul|medindo|mm|branca|lubrificada|capacidade|bioabsorvivel|certificado|peso|comprimento|lei|nbr|normas|caracteristicas|costura|dimensoes|bolsos|abnt|flexivel|infantil|anvisa|sanfonada|densidade|certificacao|gramas|inmetro|antiderrapante"),1,0),
         nivel_data2 = as.Date(stringr::str_sub(dt_inicio_vigencia, start = 1L, end = 10), tryFormats = c("%Y-%m-%d"))
  )

# Filtrar pela seleção de itens
reg3_itens_analise <- selecao %>%
  purrr::map(function(x) {
    reg3_itens_analise %>%
      mutate(selec = stringr::str_detect(ds_item, x), categoria_item = x) %>%
      filter(selec == TRUE)
  }) %>%
  bind_rows() %>%
  distinct(id_item_contrato, .keep_all= TRUE)

# Criar variaveis por item
reg3_itens_analise2 <- reg3_itens_analise %>%
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

reg3_itens_analise2 <- reg3_itens_analise2 %>%
  group_by(categoria_item,nivel_data2) %>%
  mutate(median_price = median(vl_item_contrato, na.rm=T),
         price_perc = vl_item_contrato/median_price,
         flag_pre_e_uni = if_else(bol_1_uni == 1 & price_perc > 1.5 & bol_nao_unidade == 0, 1, 0)) %>%
  ungroup


# Preencher com valores estimado
reg3_itens_analise2 <- reg3_itens_analise2 %>%
  mutate(estimativa_med_cor = predict(model, reg2_itens_analise2)),
         estimativa_material = predict(reg2_material, reg2_itens_analise2),
         estimativa_det_ad = predict(reg2_det_ad, reg2_itens_analise2))

reg_itens_analise2 %>% filter(estimativa_med_cor > 0.4) %>%
  nrow()


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

reg2_itens_analise2 %>%
  select(id_item_contrato,id_contrato, ds_item, estimativa_med_cor, estimativa_material)

reg2_itens_analise2 %>%
  select(id_item_contrato,estimativa_med_cor) %>%
  left_join(select(itens_analise2,id_item_contrato,estimativa_med_cor), by = 'id_item_contrato') %>%
  View()



####### Cross

# Split the data into training and test set
set.seed(123)
training.samples <- aventais2 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- swiss[training.samples, ]
test.data <- swiss[-training.samples, ]
# Build the model
model <- lm(Fertility ~., data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))





#################
# R program to implement 
# Leave one out cross validation 

# defining training control 
# as Leave One Out Cross Validation 
train_control <- caret::trainControl(method = "LOOCV") 

# training the model by assigning sales column 
# as target variable and rest other column 
# as independent varaible 
model <- caret::train(medida_correta ~ log(palavras_perc) + log(price_perc) + bol_nao_unidade + flag_pacote + flag_pre_e_uni, data=aventais2,  
               method = "glm",
               family = "binomial",
               trControl = train_control) 


