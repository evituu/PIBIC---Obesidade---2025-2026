#------------------------------------------------------------------------------#
###                  CÓDIGO OBESIDADE INTERGERACIONAL                        ###
#------------------------------------------------------------------------------#
# Aluno: Victor Eduardo
# Doscente: Adriano Firmino V. Araújo

# ---------------------------------------------------------------------------
# EXTRAÇÃO POR readr::read_fwf + fwf_cols (POF 2017-2018 | MORADOR.txt)
# ---------------------------------------------------------------------------

getwd()

setwd("C:/Users/vitor/OneDrive/Área de Trabalho/UFPB/PIBIC_2025/Base de Dados/POFF_Dados_2002_2003")
getwd()

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(quantreg)
  library(tibble)
  library(tidyr)
  library(haven)
  library(anthro)
  library(childsds)
  library(survey)
})

morador <- "T_MORADOR.txt"
dir()

# ------------------------ Leitura com fwf_cols ------------------------------

dados_obesidade <- read_fwf(
  file = morador,
  col_positions = fwf_cols(
    tipo_registro     = c(1, 2),
    UF                = c(3, 4),
    num_seq           = c(5, 7),        # Número Sequencial
    dv_seq            = c(8),           # DV do Sequencial
    #ESTRATO_POF       = c(3, 4),        # 3-4 
    #COD_UPA           = c(8, 252),     # 9 posições (8..16)
    NUM_DOM           = c(9, 10),
    NUM_UC            = c(11),
    fator_expan2      = c(25, 35),
    NUM_INFORMANTE    = c(36, 37),      # Número do informante    
    estrato_geo       = c(12, 13),       # Estrato geográfico
    #NUM_FAMILIA       = c(46, 47),
    COD_INFORMANTE    = c(38),          # Código do informante
    V0403             = c(49, 51),      # idade
    idade_meses       = c(59, 64),      # idade em meses
    V0404             = c(40),          # sexo
    V0405             = c(72),          # cor/raça
    #V0414             = c(78, 79),     # sabe ler e escrever
    V0415             = c(65),          # frequenta escola
    ANOS_ESTUDO       = c(70, 71), 
    NIVEL_INSTRUCAO   = c(66, 67),
    RENDA_TOTAL       = c(103, 114),
    #RENDA_MONET_PC    = c(112, 127),
    #RENDA_NAO_MONET_PC = c(128, 143),
    #PESO              = c(16, 29),
    #PESO_FINAL        = c(30, 43),
    #DEDUCAO_PC         = c(177, 196),
    ALTURA_REFERIDA   = c(86, 95),
    PESO_REFERIDO     = c(76, 85)
  ),
  # Tipagem explícita (ajuste se necessário)
  col_types = cols(
    UF                = col_character(),
    num_seq           = col_character(),
    dv_seq           = col_character(),
    #ESTRATO_POF       = col_character(),
    #COD_UPA           = col_character(),
    NUM_DOM           = col_character(),
    NUM_UC            = col_character(),
    fator_expan2      = col_character(),
    NUM_INFORMANTE    = col_character(),
    #NUM_FAMILIA       = col_integer(),
    COD_INFORMANTE    = col_character(),
    V0403             = col_integer(),
    idade_meses       = col_integer(),
    V0404             = col_integer(),
    V0405             = col_integer(),
    #V0414             = col_integer(),
    V0415             = col_integer(),
    ANOS_ESTUDO       = col_integer(),
    NIVEL_INSTRUCAO   = col_integer(),
    RENDA_TOTAL       = col_double(),
    #RENDA_MONET_PC    = col_double(),
    #RENDA_NAO_MONET_PC = col_double(),
    #PESO              = col_double(),
    #PESO_FINAL        = col_double(),
    ALTURA_REFERIDA   = col_double(),
    PESO_REFERIDO     = col_double()
  ),
  locale = locale(encoding = "Latin1"),
  na = c("", " ", "NA"),
  show_col_types = FALSE
)

###
### ------------------------ ESCALA (decimais implícitos) ----------------------
###

scale_if_needed <- function(x, dec){
  if (all(is.na(x))) return(x)
  med <- suppressWarnings(stats::median(x, na.rm = TRUE))
  if (!is.finite(med)) return(x)
  if (med > 1e6) x <- x / (10^dec)
  x
}

dados_obesidade <- dados_obesidade |>
  mutate(
    #PESO               = scale_if_needed(PESO,               8),
    #PESO_FINAL         = scale_if_needed(PESO_FINAL,         8),
    RENDA_TOTAL        = scale_if_needed(RENDA_TOTAL,        2)
    #RENDA_MONET_PC     = scale_if_needed(RENDA_MONET_PC,     10),
    #RENDA_NAO_MONET_PC = scale_if_needed(RENDA_NAO_MONET_PC, 10)
  )

summary(dados_obesidade$RENDA_TOTAL)

# Contar quantos valores são extremamente grandes ou pequenos
dados_obesidade |>
  summarise(
    max_peso = max(PESO_FINAL, na.rm = TRUE),
    min_peso = min(PESO_FINAL, na.rm = TRUE),
    max_renda = max(RENDA_TOTAL, na.rm = TRUE),
    min_renda = min(RENDA_TOTAL, na.rm = TRUE)
  )

# Tamanho do banco e verificação de NAs
dim(dados_obesidade)
colSums(is.na(dados_obesidade))

# 1) Renomeiar
dados_obesidade <- dados_obesidade |>
  rename(
    uf                = UF,
    #estrato_pof       = ESTRATO_POF,
    #zona              = TIPO_SITUACAO_REG,
    #cod_upa           = COD_UPA,
    num_dom           = NUM_DOM,
    num_uc            = NUM_UC,
    #num_familia       = NUM_FAMILIA,
    grau_parentesco    = COD_INFORMANTE,
    idade             = V0403,
    sexo              = V0404,
    cor               = V0405,
    #sabe_ler_escrever = V0414,
    freq_escola       = V0415,
    instrucao         = NIVEL_INSTRUCAO,
    anos_estudo       = ANOS_ESTUDO,
    #peso              = PESO,
    #peso_final        = PESO_FINAL,
    renda_total       = RENDA_TOTAL,
    #renda_monet_pc    = RENDA_MONET_PC,
    #renda_nao_monet_pc = RENDA_NAO_MONET_PC,
    altura            = ALTURA_REFERIDA,
    massa              = PESO_REFERIDO
  )

# quantidade de moradores
# Dummies de região
# biparental, mono H, mono M
# Pesos

# Criar chave única para unidade de consumo e para pessoa
dados_obesidade <- dados_obesidade |>
  mutate(
    chave_uc = paste0(
      str_pad(uf, 2, pad = "0"),            # 1–2: UF
      str_pad(num_seq, 1, pad = "0"),
      str_pad(dv_seq, 1, pad = "0"),
      str_pad(num_dom, 2 , pad = "0"),
      str_pad(num_uc, 1, pad = "0"),        # 19: unidade de consumo
      str_pad(fator_expan2, 11, pad = "0")  # Fator de expansão do domicílio
      #str_pad(estrato_pof, 4, pad = "0"),   # 3–6: Estrato
      #str_pad(cod_upa, 9, pad = "0"),       # 8–16: UPA
      #str_pad(num_familia, 2, pad = "0")    # 46-47: unidade familiar 
    )
  ) |>
  mutate(
    chave_pessoa = paste0(
      str_pad(uf, 2, pad = "0"),            # 1–2: UF
      str_pad(num_seq, 1, pad = "0"),
      str_pad(dv_seq, 1, pad = "0"),
      str_pad(num_dom, 2 , pad = "0"),
      str_pad(num_uc, 1, pad = "0"), 
      str_pad(grau_parentesco, 1, pad = "0"),
      str_pad(NUM_INFORMANTE, 2, pad = "0")
    )
  )

# Verificar formação da chave:
chave_verificacao <- dados_obesidade |>
  select(
    uf, 
    #estrato_pof, 
    zona, 
    #cod_upa, 
    num_dom, 
    #num_uc, 
    cod_informante, 
    chave_pessoa)

# Conferir se é única:
dados_obesidade |> 
  count(chave_uc) |>
  filter(n > 1)

# chave_uc: várias linhas por família (normal no MORADOR)
dados_obesidade |> count(num_uc) |> arrange(desc(n)) |> head()
dados_obesidade |> count(num_dom) |> arrange(desc(n)) |> head()

dados_obesidade |> count(chave_uc) |> arrange(desc(n)) |> head()
sum(duplicated(dados_obesidade$chave_uc))

# chave_pessoa: deve ser única por morador
dados_obesidade |> count(chave_pessoa) |> filter(n > 1)  # idealmente, 0 linhas
sum(duplicated(dados_obesidade$chave_pessoa))

# Nenhuma chave está repetida ou duplicada

# Nenhum NA em chaves
colSums(is.na(dados_obesidade[, c("chave_uc", "chave_pessoa")]))

# ------------------------ Checagens rápidas ---------------------------------
glimpse(dados_obesidade)
head(dados_obesidade)
# table(dados_obesidade$num_uc, useNA = "ifany")
# table(dados_obesidade$grau_parentesco, useNA = "ifany")


### ------------------------ OVERVIEW E ANÁLISE ---------------------------- ###

# Verificação dos dados da base
str(dados_obesidade)
unique(dados_obesidade$uf)
unique(dados_obesidade$num_dom)
unique(dados_obesidade$grau_parentesco)
unique(dados_obesidade$instrucao)
unique(dados_obesidade$idade)
unique(dados_obesidade$sexo)
unique(dados_obesidade$cor)
#unique(dados_obesidade$num_uc)
unique(dados_obesidade$freq_escola)
#unique(dados_obesidade$num_familia)

# Verificar estatística descritiva da altura e massa
summary(dados_obesidade$massa)
summary(dados_obesidade$altura)

# Verificar quantidade de NA
sum(is.na(dados_obesidade$massa))
sum(is.na(dados_obesidade$altura))

unique(dados_obesidade$num_dom)


# Verificar estatística descritiva da altura e massa
summary(dados_obesidade$massa)
summary(dados_obesidade$altura)

# Verificar quantidade de NA
sum(is.na(dados_obesidade$massa))
sum(is.na(dados_obesidade$altura))

dados_obesidade <- dados_obesidade |> 
  filter(altura > 0)

dados_obesidade |> 
  filter(altura == 0) |> 
  select(massa, altura)

dados_obesidade <- dados_obesidade |> 
  filter(altura > 0)

dados_obesidade <- dados_obesidade |> 
  filter(massa > 0)

summary(dados_obesidade$altura)
dados_obesidade <- dados_obesidade |>
  dplyr::filter(altura <= 250)

ggplot(dados_obesidade, aes(y = altura)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.color = "red") +
  labs(
    title = "Boxplot da Altura (cm) - POF",
    y = "Altura (cm)"
  ) +
  theme_minimal(base_size = 12)

summary(dados_obesidade$massa)
summary(dados_obesidade$altura)


# Calcular IMC das observações
dados_obesidade <- dados_obesidade |>
  mutate(
    altura_m = altura / 100,
    imc = massa / (altura_m^2)
  )

summary(dados_obesidade$imc)
sum(is.na(dados_obesidade$imc))
count(dados_obesidade)

# Visualizar graficamente o IMC geral
ggplot(dados_obesidade, aes(x = imc)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "blue", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1.1) +
  labs(
    title = "Distribuição do IMC da POF 2002-2003)",
    x = "IMC (kg/m²)",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12)

sum(duplicated(dados_obesidade$chave_pessoa))
sum(duplicated(dados_obesidade$chave_pessoa))

# Organizar ordem das variáveis
dados_obesidade <- dados_obesidade |>
  select(
    uf, num_dom, chave_uc, num_uc,
    chave_uc, chave_pessoa, grau_parentesco, idade, sexo, cor,
    freq_escola, anos_estudo, instrucao, renda_total,
    massa, altura, imc
  )

table(dados_obesidade$num_familia)

quant_crianca1 <- dados_obesidade |>
  filter(grau_parentesco == 3) |>
  filter(idade < 20 & idade > 2)

table(quant_crianca1$num_uc)
table(quant_crianca1$idade)
table(dados_obesidade$idade)


quant_crianca2 <- dados_obesidade |>
  filter(grau_parentesco == 3) |>
  filter(idade <= 20 & idade >= 2)

colSums(is.na(dados_obesidade))
table(dados_obesidade$idade)

#------------------------------------------------------------------------------#
### ----------------------- TRATAMENTO DOS DADOS --------------------------- ###
#------------------------------------------------------------------------------#

## Remover mais de uma unidade familiar

unique(dados_obesidade$num_uc)

# Transformar em numérica
dados_obesidade$num_uc <- as.numeric(dados_obesidade$num_uc)
unique(dados_obesidade$num_uc)

dados_obesidade_trat_uma_familia_uc <- dados_obesidade |>
  filter(num_uc == 1)

dados_obesidade_trat_uma_familia_uc <- dados_obesidade_trat_uma_familia_uc

table(dados_obesidade_trat_uma_familia_uc$num_uc)
unique(dados_obesidade_trat_uma_familia_uc$num_uc)

# Manter os graus de parentescos relevantes para pesquisa
sort(unique(dados_obesidade_trat_uma_familia_uc$grau_parentesco))
table(dados_obesidade_trat_uma_familia_uc$grau_parentesco)

dados_obesidade_trat <- dados_obesidade_trat_uma_familia_uc |>
  filter(grau_parentesco %in% c(1,2,3))

sort(unique(dados_obesidade_trat$grau_parentesco)) # Verificar
table(dados_obesidade_trat$grau_parentesco)        # Verificar

# Criar variável de branco e não branco
## Examinar situação:
sort(unique(dados_obesidade_trat$cor))
table(dados_obesidade_trat$cor)

dados_obesidade_trat <- dados_obesidade_trat |>
  filter(cor != 9)

dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    cor = case_when(
      cor == 1 ~ 1L,
      cor %in% c(2, 3, 4, 5) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

sort(unique(dados_obesidade_trat$cor))
table(dados_obesidade_trat$cor)

# Contar quantos responsáveis (grau_parentesco == 1) há em cada UC
domicilios_mult_resp <- dados_obesidade_trat |>
  filter(grau_parentesco == 1) |>         
  count(chave_uc, name = "n_responsaveis") |>   # contar por família
  filter(n_responsaveis > 1)                    # mantém famílias com > 1 responsável

# Ver quantas famílias estão nessa situação
nrow(domicilios_mult_resp)
head(domicilios_mult_resp, 10)
# Os resultados apontam que existe apenas um responsável por UC

# Construir a estrutura familiar biparental e monoparental
fam_flags <- dados_obesidade_trat |>
  summarise(
    n_pais = sum(sexo == 1 & grau_parentesco %in% c(1,2)),
    n_maes = sum(sexo == 2 & grau_parentesco %in% c(1,2)),
    .by = chave_uc
  ) |>
  mutate(
    estrutura_fam = case_when(
      (n_pais + n_maes) >= 2 ~ "biparental",
      n_maes == 1 & n_pais == 0 ~ "mono_mulher",
      n_pais == 1 & n_maes == 0 ~ "mono_homem",
      TRUE ~ NA_character_
    )
  )

table(fam_flags$estrutura_fam)
table(fam_flags$n_pais)
table(fam_flags$n_maes)

# Criar variável de região e criação da dummy regional
dados_obesidade_trat <- dados_obesidade_trat |>
  dplyr::mutate(
    regiao = case_when(
      uf %in% c(11:17) ~ "norte",
      uf %in% c(21:29) ~ "nordeste",
      uf %in% c(31:35) ~ "sudeste",
      uf %in% c(41:43) ~ "sul",
      uf %in% c(50:53) ~ "centro_oeste"
    )
  )

unique(dados_obesidade_trat$regiao)
table(dados_obesidade_trat$regiao)

# Criar dummy regional
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    dummy_norte        = if_else(regiao == "norte",         1L, 0L),
    dummy_nordeste     = if_else(regiao == "nordeste",      1L, 0L),
    dummy_sul          = if_else(regiao == "sul",           1L, 0L),
    dummy_centro_oeste = if_else(regiao == "centro_oeste",  1L, 0L)
    # repare: NÃO crio dummy para "sudeste" → ele é a base
  )

unique(dados_obesidade_trat$dummy_norte)
unique(dados_obesidade_trat$dummy_nordeste)
unique(dados_obesidade_trat$dummy_sul)
unique(dados_obesidade_trat$dummy_centro_oeste)

# Contar número de moradores por Unidade de Consumo - Universo de moradores
# Usar base não trata para considerar todo mundo
moradores_uc <- dados_obesidade_trat_uma_familia_uc |>
  count(chave_uc, name = "n_moradores")

table(moradores_uc$n_moradores)

# Dummies de frequência escolar
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    freq_escola = as.integer(freq_escola == 1)
  )

# Verificar a dummy escolar
freq_escola_verificacao <- dados_obesidade_trat |>
  mutate(
    freq_escolar = as.integer(freq_escola == 1)
  ) |>
  select(
    freq_escola, freq_escolar
  )

# Verificar dados de frequêcnia escolar
unique(dados_obesidade_trat$freq_escola)
table(dados_obesidade_trat$freq_escola)

####
#### CRIAR BASE DE DADOS
####

## 2.1 Base do responsável (V0306 == 1)
# Se houver mais de 1 “responsável” na mesma UC, fica o mais velho.
base_resp <- dados_obesidade_trat |>
  filter(grau_parentesco == 1) |>
  filter(idade >= 18) |>
  # arrange(chave_uc, desc(idade)) |>
  # distinct(chave_uc, .keep_all = TRUE) |>
  transmute(
    chave_uc,
    chave_pessoa_resp = chave_pessoa,
    idade_resp = idade,
    idade_resp2 = idade^2,              # <- idade^2
    sexo_resp  = sexo,
    cor_resp   = cor,
    instrucao_resp = instrucao,
    anos_estudo_resp = anos_estudo,
    chave_pessoa_resp = chave_pessoa,
    freq_escola_resp = freq_escola,

    altura_resp = altura,
    massa_resp = massa,
    imc_resp = imc
    
  )

# Verificar se tem mais de um cônjuge
base_conj <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(2)) |>
  count(chave_uc, name = "n_conjuges") |>
  filter(n_conjuges > 1)

nrow(base_conj); head(base_conj)

# 2.2 Base do cônjuge
base_conj <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(2)) |>
  filter(idade >= 18) |>
  # arrange(chave_uc, desc(idade)) %>%
  distinct(chave_uc, .keep_all = TRUE) %>%
  transmute(
    chave_uc,
    chave_pessoa_conj = chave_pessoa,
    idade_conj = idade,
    idade_conj2 = idade^2,              # <- idade^2
    sexo_conj  = sexo,
    cor_conj   = cor,
    instrucao_conj = instrucao,
    anos_estudo_conj = anos_estudo,
    chave_pessoa_conj = chave_pessoa,
    freq_escola_conj = freq_escola,
    
    altura_conj = altura,
    massa_conj = massa,
    imc_conj = imc
    
  )
nrow(base_conj); head(base_conj)

# 2.3 Base dos filhos
base_filho <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(3), idade >=2, idade <=20) |>
  arrange(chave_uc) |>
  mutate(id_filho_uc = row_number(), .by = chave_uc) |>
  transmute(
    chave_uc,
    id_filho_uc,
    idade_filho = idade,
    idade_filho2 = idade^2,
    idade_meses_filho = as.integer(idade * 12L),  # <- para tabela OMS/CDC
    sexo_filho  = sexo,
    cor_filho   = cor,
    instrucao_filho = instrucao,
    chave_pessoa_filho = chave_pessoa,
    freq_escola_filho = freq_escola,

    altura_filho = altura,
    massa_filho = massa,
    imc_filho = imc
  )

uc_info <- dados_obesidade_trat |>
  distinct(chave_uc, regiao, uf, dummy_norte, dummy_nordeste, dummy_sul, dummy_centro_oeste,
           renda_total)

# Criar base consolidada “filho + pais + controles”
# Fazer "inner_join" para se ter pelo menos um par de "filho - responsável"
base_unida_consolidada <- base_filho |>
  inner_join(base_resp, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  inner_join(base_conj, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  left_join(fam_flags, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  left_join(moradores_uc, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  left_join(uc_info, by = "chave_uc")

str(base_unida_consolidada)



# Criar IMC de Pai e de Mãe
base_unida_consolidada <- base_unida_consolidada |>
  mutate(
    # Pai
    imc_pai = case_when(
      estrutura_fam == "biparental" & sexo_resp == 1 ~ imc_resp,
      estrutura_fam == "biparental" & sexo_conj == 1 ~ imc_conj,
      estrutura_fam == "mono_homem"                  ~ imc_resp,
      TRUE                                           ~ NA_real_
    ),
    # Mãe
    imc_mae = case_when(
      estrutura_fam == "biparental" & sexo_resp == 2 ~ imc_resp,
      estrutura_fam == "biparental" & sexo_conj == 2 ~ imc_conj,
      estrutura_fam == "mono_mulher"                 ~ imc_resp,
      TRUE                                           ~ NA_real_
    )
  )


# Função para classificar o IMC
classifica_imc_adulto <- function(imc) {
  case_when(
    is.na(imc)           ~ NA_character_,
    imc < 18.5           ~ "desnutrido",
    imc < 25             ~ "saudavel",
    imc < 30             ~ "sobrepeso",
    TRUE                 ~ "obeso"
  )
}

# 4 categorias empíricas para crianças
classifica_empirico4 <- function(imc, p5, p85, p95) {
  dplyr::case_when(
    is.na(imc)      ~ NA_character_,
    imc < p5        ~ "desnutrido",
    imc < p85       ~ "saudavel",
    imc < p95       ~ "sobrepeso",
    TRUE            ~ "obeso"
  )
  
}

base_unida_consolidada <- base_unida_consolidada %>%
  group_by(sexo_filho) %>%  # 1 = menino, 2 = menina
  mutate(
    p5  = quantile(imc_filho, 0.05, na.rm = TRUE),
    p85 = quantile(imc_filho, 0.85, na.rm = TRUE),
    p95 = quantile(imc_filho, 0.95, na.rm = TRUE),
    
    # classificação comum (antes de separar em filho/filha)
    cat_imc_filho_filha = classifica_empirico4(imc_filho, p5, p85, p95)
  ) %>%
  ungroup() %>%
  mutate(
    cat_imc_pai  = classifica_imc_adulto(imc_pai),
    cat_imc_mae  = classifica_imc_adulto(imc_mae),
    
    # divide explicitamente para variáveis separadas
    cat_imc_filho = if_else(sexo_filho == 1, cat_imc_filho_filha, NA_character_),
    cat_imc_filha = if_else(sexo_filho == 2, cat_imc_filho_filha, NA_character_),
    
    obeso_pai = as.integer(cat_imc_pai == "obeso"),
    obeso_mae = as.integer(cat_imc_mae == "obeso")
  ) %>%
  select(-p5, -p85, -p95)

table(base_unida_consolidada$cat_imc_filha, useNA = "ifany")
table(base_unida_consolidada$cat_imc_filho, useNA = "ifany")


table(base_unida_consolidada$cat_imc_filha)
table(base_unida_consolidada$cat_imc_filho)
table(base_unida_consolidada$cat_imc_pai)
table(base_unida_consolidada$cat_imc_mae)

unique(base_unida_consolidada$cat_imc_filho)

unique(base_unida_consolidada$idade_meses_filho)
table(base_unida_consolidada$idade_meses_filho)
sum(is.na(base_unida_consolidada$idade_meses_filho))

table(base_unida_consolidada$cat_imc_filho, useNA = "ifany")
sum(is.na(base_unida_consolidada$imc_filho))

table(base_unida_consolidada$cat_imc_mae)
table(base_unida_consolidada$cat_imc_pai)

table(base_unida_consolidada$obeso_mae)
table(base_unida_consolidada$obeso_pai)

base_unida_consolidada <- base_unida_consolidada |>
  mutate(
    # dummy branco
    branco_filho = as.integer(cor_filho == 0),  # se 0 for branco mesmo
    
    # log renda
    ln_renda_total = log(renda_total + 1)
  )

#base_unida_consolidadaa <- base_unida_consolidada |>
#  filter(estrutura_fam == "biparental")

# Baixar dados em .dta
#write_dta(base_unida_consolidada, "base_unida_poff2.dta")

colSums(is.na(base_unida_consolidada))
any(is.na(base_unida_consolidada))
str(base_unida_consolidada)

table(base_unida_consolidada$cat_imc_pai)
table(base_unida_consolidada$cat_imc_mae)
tale(base_unida_consolidada$cat_f)

table(base_unida_consolidada)

## -------------------------------------------------------------------------- ##
##                           MATRIZ DE TRANSIÇÃO                              ##
## -------------------------------------------------------------------------- ##
colSums(is.na(base_unida_consolidada))
any(is.na(base_unida_consolidada))
str(base_unida_consolidada)


# Remove todas as linhas que tenham pelo menos um NA em qualquer coluna
base_unida_consolidada_sem_na <- na.omit(base_unida_consolidada)
# equivalente:
base_unida_consolidada_sem_na <- base_unida_consolidada[complete.cases(base_unida_consolidada), ]

library(survey)

# 1) Filtrar faixa etária e observações válidas
base_analise <- base_unida_consolidada |>
  # faixa etária usada no artigo (ajuste se quiser)
  filter(
    idade_filho >= 2,
    idade_filho <= 20,
    !is.na(cat_imc_filho_filha),
    !is.na(cat_imc_pai),
    !is.na(estrutura_fam),
    !is.na(sexo_filho),
    !is.na(peso_final_filho)
  ) |>
  # pelo menos pai com categoria de IMC e filho com categoria de IMC
  filter(
    !is.na(cat_imc_filho_filha),
    !is.na(cat_imc_pai)
  ) |>
  # garantir a mesma ordem de estados (desnutrido -> saudável -> sobrepeso -> obeso)
  mutate(
    cat_imc_filho_filha = factor(
      cat_imc_filho_filha,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    cat_imc_pai = factor(
      cat_imc_pai,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    # codificação numérica para o índice beta
    imc_pai_num = as.numeric(cat_imc_pai) - 1,
    imc_filho_num = as.numeric(cat_imc_filho_filha) - 1,
    # "controle" = combinação de estrutura familiar e sexo do filho
    grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
    controle = dense_rank(grupo)
  )

colSums(is.na(base_analise))


{
  # Matriz de transição (todas as matrizes empilhadas)
  Matriz_Transicao <- tibble(
    Controle        = integer(),
    origem_imc      = character(),
    IMC_desnutrido  = double(),
    IMC_saudavel    = double(),
    IMC_sobrepeso   = double(),
    IMC_obeso       = double()
  )
  
  Mobility_Index <- NULL
  estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
  
  for (g in sort(unique(base_analise$controle))) {
    
    dados <- base_analise |>
      filter(controle == g)
    
    # Se grupo tiver poucas observações, pula
    if (nrow(dados) < 2) next
    
    # Desenho amostral
    design <- tryCatch(
      svydesign(
        id      = ~cod_upa_resp,
        strata  = ~estrato_pof_resp,
        weights = ~peso_final_filho,
        data    = dados,
        nest    = TRUE
      ),
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
        return(NULL)
      }
    )
    if (is.null(design)) next
    
    observacao <- nrow(dados)
    
    populacao_estimada <- tryCatch(
      round(sum(weights(design)), 0),
      error = function(e) {
        message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
        return(NA_real_)
      }
    )
    
    # ---------- Matriz de transição pai -> filho(a) ---------- #
    tab_IMC <- tryCatch(
      svytable(~cat_imc_pai + cat_imc_filho_filha, design),
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svytable): ", e$message)
        return(NULL)
      }
    )
    if (is.null(tab_IMC)) next
    
    MTransicao <- data.frame(
      prop.table(tab_IMC, margin = 1)
    ) |>
      complete(
        cat_imc_pai         = estados_imc,
        cat_imc_filho_filha = estados_imc,
        fill = list(Freq = 0)
      ) |>
      arrange(cat_imc_pai) |>
      pivot_wider(
        names_from  = cat_imc_filho_filha,
        values_from = Freq
      ) |>
      select(-cat_imc_pai) |>
      as.matrix()
    
    rownames(MTransicao) <- estados_imc
    
    # ----------------- Índices de mobilidade ----------------- #
    eigenvalues <- eigen(MTransicao)$values
    
    Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
    Ml  <- 1 - abs(eigenvalues[2])
    Md  <- 1 - abs(det(MTransicao))
    Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
    
    Ib <- 0
    for (i in 1:nrow(MTransicao)) {
      for (j in 1:nrow(MTransicao)) {
        Ib <- Ib + (MTransicao[i, j] * abs(i - j))
      }
    }
    Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
    
    IMA <- 0
    for (i in 1:(nrow(MTransicao) - 1)) {
      for (j in (i + 1):nrow(MTransicao)) {
        IMA <- IMA + MTransicao[i, j]
      }
    }
    IMD <- 0
    for (i in 2:nrow(MTransicao)) {
      for (j in 1:(i - 1)) {
        IMD <- IMD + MTransicao[i, j]
      }
    }
    IP <- sum(diag(MTransicao))
    
    soma <- IMA + IMD + IP
    IMA <- IMA / soma
    IMD <- IMD / soma
    IP  <- IP  / soma
    
    # Persistências
    P00 <- MTransicao[1, 1]
    P11 <- MTransicao[2, 2]
    P22 <- MTransicao[3, 3]
    P33 <- MTransicao[4, 4]
    
    # ---------- Índice beta (regressão) ---------- #
    regre <- tryCatch(
      svyglm(imc_filho_num ~ imc_pai_num, design = design),
      error = function(e) {
        message("Erro em svyglm no grupo ", g, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(regre) || is.na(coef(regre)[2])) {
      beta <- NA_real_
      beta_compl <- NA_real_
    } else {
      beta <- coef(regre)[2]
      beta_compl <- 1 - beta
    }
    
    Mobility_Index_coluna <- tibble(
      Indicador = c(
        "Observacoes",
        "Populacao Estimada",
        "Indice de Prais",
        "Indice do 2o Autovalor",
        "Indice do Determinante",
        "Indice do Determinante Alternativo",
        "Indice de Bartholomew",
        "Indice de Mobilidade Ascendente",
        "Indice de Mobilidade Descendente",
        "Indice de Persistencia",
        "Persistencia em desnutrido",
        "Persistencia em saudavel",
        "Persistencia em sobrepeso",
        "Persistencia em obeso",
        "Indice Beta (Parametrico)",
        "Indice 1-Beta (Parametrico)"
      ),
      !!paste0("grupo_", g) := c(
        observacao,
        populacao_estimada,
        Mt, Ml, Md, Mda,
        Ib,
        IMA, IMD, IP,
        P00, P11, P22, P33,
        beta, beta_compl
      )
    )
    
    Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
      mutate(
        Controle   = g,
        origem_imc = estados_imc
      ) |>
      relocate(Controle, origem_imc) |>
      rename(
        IMC_desnutrido = desnutrido,
        IMC_saudavel   = saudavel,
        IMC_sobrepeso  = sobrepeso,
        IMC_obeso      = obeso
      )
    
    Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
    
    if (is.null(Mobility_Index)) {
      Mobility_Index <- Mobility_Index_coluna
    } else {
      Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
    }
  }
}

Legenda_Micrormicro <- base_analise |>
  distinct(controle, .keep_all = TRUE) |> # MantÃ©m a primeira ocorrÃƒÂªncia de cada 'controle
  select(controle, regiao) |>
  arrange(controle)

names(Mobility_Index)
Matriz_Transicao |> dplyr::count(Controle)

Mobility_Index
# ou, se quiser só olhar:
View(Mobility_Index)

Mobility_Index_long <- Mobility_Index |>
  tidyr::pivot_longer(
    cols = starts_with("grupo_"),
    names_to = "grupo",
    values_to = "valor"
  )

Mobility_Index_long

mapa_grupos <- base_analise |>
  distinct(controle, estrutura_fam, sexo_filho) |>
  arrange(controle) |>
  mutate(
    grupo = paste0("grupo_", controle),
    sexo_filho_lab = dplyr::recode(
      as.character(sexo_filho),
      "1" = "menino",
      "2" = "menina"
    ),
    label_grupo = paste(estrutura_fam, sexo_filho_lab, sep = " - ")
  )

mapa_grupos

Mobility_Index_rotulado <- Mobility_Index_long |>
  left_join(mapa_grupos[, c("grupo", "label_grupo")], by = "grupo") |>
  select(Indicador, label_grupo, valor) |>
  arrange(Indicador, label_grupo) |>
  filter(
    Indicador %in% c(
      "Indice 1-Beta (Parametrico)",
      "Indice de Bartholomew",
      "Indice de Mobilidade Ascendente",
      "Indice de Prais",
      "Persistencia em obeso",
      "Persistencia em desnutrido"
    )
  )

Mobility_Index_rotulado
Matriz_Transicao

## -------------------------------------------------------------------------- ##
##                           MODELOS ECONOMÉTRICOS                            ##
## -------------------------------------------------------------------------- ##

## -------------------------------------------------------------------------- ##
###        Regressão MQO e Quantílica: elasticidades intergeracionais        ###
## -------------------------------------------------------------------------- ##

# Amostra biparental, sem missing
amostra_reg <- base_unida_consolidada |>
  filter(
    !is.na(imc_filho),
    !is.na(imc_pai), !is.na(imc_mae),
    imc_filho > 0, imc_pai > 0, imc_mae > 0
  ) |>
  mutate(
    ln_imc_filho = log(imc_filho),
    ln_imc_pai   = log(imc_pai),
    ln_imc_mae   = log(imc_mae)
  )

# Fórmula
form <- ln_imc_filho ~ ln_imc_pai + ln_imc_mae +
  idade_filho + I(idade_filho^2) +
  branco_filho + freq_escola_filho +
  n_moradores + ln_renda_total +
  dummy_norte + dummy_nordeste + dummy_sul + dummy_centro_oeste
# + dummies de instrucao_resp / instrucao_conj, se quiser

# MQO
mqo <- lm(form, data = amostra_reg)
summary(mqo)

# Regressão quantílica: mediana, q90 e q95
rq50 <- rq(form, tau = 0.5, data = amostra_reg)
rq90 <- rq(form, tau = 0.9, data = amostra_reg)
rq95 <- rq(form, tau = 0.95, data = amostra_reg)

summary(rq95, se = "boot", R = 200)
summary(rq90, se = "boot", R = 200)
summary(rq50, se = "boot", R = 200)

# Filtrar por sexo
amostra_meninOS  <- amostra_reg |> filter(sexo_filho == 1)
amostra_meninAS  <- amostra_reg |> filter(sexo_filho == 2)

# MQO para meninos
mqo_meninos <- lm(form, data = amostra_meninOS)
summary(mqo_meninos)

# MQO para meninas
mqo_meninas <- lm(form, data = amostra_meninAS)
summary(mqo_meninas)

# ---------------------------------------------------------------------------#
#          Regressões quantílicas por sexo do filho (τ = 0.5, 0.9, 0.95)     #
# ---------------------------------------------------------------------------#

# Meninos
rq50_meninos <- rq(form, tau = 0.5, data = amostra_meninOS)
rq90_meninos <- rq(form, tau = 0.9, data = amostra_meninOS)
rq95_meninos <- rq(form, tau = 0.95, data = amostra_meninOS)

sum_rq50_meninos <- summary(rq50_meninos, se = "boot", R = 200)
sum_rq90_meninos <- summary(rq90_meninos, se = "boot", R = 200)
sum_rq95_meninos <- summary(rq95_meninos, se = "boot", R = 200)

sum_rq50_meninos
sum_rq90_meninos
sum_rq95_meninos

# Meninas
rq50_meninas <- rq(form, tau = 0.5, data = amostra_meninAS)
rq90_meninas <- rq(form, tau = 0.9, data = amostra_meninAS)
rq95_meninas <- rq(form, tau = 0.95, data = amostra_meninAS)

sum_rq50_meninas <- summary(rq50_meninas, se = "boot", R = 200)
sum_rq90_meninas <- summary(rq90_meninas, se = "boot", R = 200)
sum_rq95_meninas <- summary(rq95_meninas, se = "boot", R = 200)

sum_rq50_meninas
sum_rq90_meninas
sum_rq95_meninas

# Função helper para pegar coeficientes de pai/mãe de um modelo
extrai_betas <- function(modelo, nome_modelo, grupo) {
  coefs <- coef(modelo)
  tibble(
    grupo   = grupo,
    modelo  = nome_modelo,
    beta_pai = unname(coefs["ln_imc_pai"]),
    beta_mae = unname(coefs["ln_imc_mae"])
  )
}

# Tabelas de elasticidades
resultados_total <- bind_rows(
  extrai_betas(mqo,  "MQO_total",  "Total"),
  extrai_betas(rq50, "RQ50_total", "Total"),
  extrai_betas(rq90, "RQ90_total", "Total"),
  extrai_betas(rq95, "RQ95_total", "Total")
)

# Tabelas de elasticidades para MENINOS
resultados_meninos <- bind_rows(
  extrai_betas(mqo_meninos,  "MQO_meninos",  "Meninos"),
  extrai_betas(rq50_meninos, "RQ50_meninos", "Meninos"),
  extrai_betas(rq90_meninos, "RQ90_meninos", "Meninos"),
  extrai_betas(rq95_meninos, "RQ95_meninos", "Meninos")
)

# Tabelas de elasticidades para MENINAS
resultados_meninas <- bind_rows(
  extrai_betas(mqo_meninas,  "MQO_meninas",  "Meninas"),
  extrai_betas(rq50_meninas, "RQ50_meninas", "Meninas"),
  extrai_betas(rq90_meninas, "RQ90_meninas", "Meninas"),
  extrai_betas(rq95_meninas, "RQ95_meninas", "Meninas")
)

# Juntar tudo numa tabela só
tabela_elasticidades <- bind_rows(
  resultados_total,
  resultados_meninos,
  resultados_meninas
)

tabela_elasticidades

tabela_organizada <- tabela_elasticidades |>
  mutate(
    modelo = factor(modelo, levels = c(
      "MQO_total","RQ50_total","RQ90_total","RQ95_total",
      "MQO_meninos","RQ50_meninos","RQ90_meninos","RQ95_meninos",
      "MQO_meninas","RQ50_meninas","RQ90_meninas","RQ95_meninas"
    ))
  ) |>
  arrange(grupo, modelo)

tabela_organizada

tabela_formatada <- tabela_elasticidades |>
  mutate(
    beta_pai = round(beta_pai, 3),
    beta_mae = round(beta_mae, 3),
    modelo = recode(modelo,
                    "MQO_total" = "MQO",
                    "RQ50_total" = "RQ(50)",
                    "RQ90_total" = "RQ(90)",
                    "RQ95_total" = "RQ(95)",
                    "MQO_meninos" = "MQO",
                    "RQ50_meninos" = "RQ(50)",
                    "RQ90_meninos" = "RQ(90)",
                    "RQ95_meninos" = "RQ(95)",
                    "MQO_meninas" = "MQO",
                    "RQ50_meninas" = "RQ(50)",
                    "RQ90_meninas" = "RQ(90)",
                    "RQ95_meninas" = "RQ(95)"
    )
  ) |>
  arrange(grupo, modelo)

tabela_formatada

summary(base_unida_consolidada$idade_resp)
summary(base_unida_consolidada$idade_filho)
summary(base_unida_consolidada$idade_conj)
count(base_unida_consolidada)

# Salvar todo o resultado:
resultados_2002 <- list(
  dados_obesidade = dados_obesidade,
  dados_obesidade_trat = dados_obesidade_trat,
  base_unida_consolidada = base_unida_consolidada,
  #Matriz_Transicao = Matriz_Transicao,
  #Mobility_Index = Mobility_Index,
  #Legenda_Microrregiao = Legenda_Microrregiao,
  mqo = mqo,
  rq50 = rq50,
  rq90 = rq90,
  rq95 = rq95,
  mqo_meninos = mqo_meninos,
  mqo_meninas = mqo_meninas,
  rq50_meninos = rq50_meninos,
  rq90_meninos = rq90_meninos,
  rq95_meninos = rq95_meninos,
  rq50_meninas = rq50_meninas,
  rq90_meninas = rq90_meninas,
  rq95_meninas = rq95_meninas,
  tabela_formatada = tabela_formatada
)

# Salvar resultados:
saveRDS(resultados, "resultados_obesidade_intergeracional_2002.rds")

# Rodar novamente (carregar resultados)
resultados <- readRDS("resultados_obesidade_intergeracional_2002.rds")

# Bases
dados_obesidade               <- resultados$dados_obesidade
dados_obesidade_trat          <- resultados$dados_obesidade_trat
base_unida_consolidada        <- resultados$base_unida_consolidada

# Matrizes e índices
#Matriz_Transicao              <- resultados$Matriz_Transicao
#Mobility_Index                <- resultados$Mobility_Index
#Legenda_Microrregiao          <- resultados$Legenda_Microrregiao

# Modelos total
mqo                           <- resultados$mqo
rq50                          <- resultados$rq50
rq90                          <- resultados$rq90
rq95                          <- resultados$rq95

# Modelos por sexo
mqo_meninos                   <- resultados$mqo_meninos
mqo_meninas                   <- resultados$mqo_meninas

rq50_meninos                  <- resultados$rq50_meninos
rq90_meninos                  <- resultados$rq90_meninos
rq95_meninos                  <- resultados$rq95_meninos

rq50_meninas                  <- resultados$rq50_meninas
rq90_meninas                  <- resultados$rq90_meninas
rq95_meninas                  <- resultados$rq95_meninas

# Tabelas finais
tabela_formatada              <- resultados$tabela_formatada

# Conferência rápida
ls()  # deve mostrar tudo no ambiente







domicilio <- "T_DOMICILIO.txt"

dados_obesidade <- read_fwf(
  file = domicilio,
  col_positions = fwf_cols(
    TIPO_REGISTRO     = c(1, 2),
    UF                = c(3, 4),
    NUM_SEQ           = c(5, 7),
    DEV_SEQ           = c(8)
  ),
  # Tipagem explícita (ajuste se necessário)
  col_types = cols(
    TIPO_REGISTRO     = col_character(),
    UF                = col_character(),
    NUM_SEQ           = col_character(),
    DEV_SEQ           = col_character()
  ),
  locale = locale(encoding = "Latin1"),
  na = c("", " ", "NA"),
  show_col_types = FALSE
)

unique(dados_obesidade$TIPO_REGISTRO)
unique(dados_obesidade$UF)
unique(dados_obesidade$NUM_SEQ)
unique(dados_obesidade$DEV_SEQ)

dados_obesidade <- dados_obesidade |>
  mutate(
    chave_uc = paste0(
      str_pad(TIPO_REGISTRO, 2, pad = "0"),            # 1–2: UF
      str_pad(UF, 2, pad = "0"),   # 3–6: Estrato
      str_pad(NUM_SEQ, 3, pad = "0"),          # 7: Tipo de situação (urb/rural)
      #str_pad(cod_upa, 9, pad = "0"),       # 8–16: UPA
      str_pad(DEV_SEQ, 1, pad = "0")       # 17–18: domicílio
      #str_pad(num_uc, 1, pad = "0"),        # 19: unidade de consumo
      #str_pad(num_familia, 2, pad = "0")    # 46-47: unidade familiar 
    ))

# Conferir se é única:
dados_obesidade |> 
  count(chave_uc) |>
  filter(n > 1)

# chave_uc: várias linhas por família (normal no MORADOR)
dados_obesidade |> count(chave_uc) |> arrange(desc(n)) |> head()
sum(duplicated(dados_obesidade$chave_uc))

# chave_pessoa: deve ser única por morador
dados_obesidade |> count(chave_pessoa) |> filter(n > 1)  # idealmente, 0 linhas
sum(duplicated(dados_obesidade$chave_pessoa))






