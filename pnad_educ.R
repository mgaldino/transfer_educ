library(lodown)
library(survey)
library(dplyr)
library(janitor) # instalar versão do github
library(data.table)
library(readr)

setwd("C:\\Users\\mgaldino\\2018\\Ta de Pe\\transfer_educ\\pnad")

# importando discionário de variáveis do suplemento de educação
load(file="input.RData")

# importando PNAD
setwd("C:\\Users\\mgaldino\\2018\\Ta de Pe\\transfer_educ\\pnad\\PNADC_022017_educacao")

pnad2017 <- read_fwf("PNADC_022017_educacao.txt", input,
                       col_types = cols(RM_RIDE= "i",
                                        V1008 = "i",
                                        V1014 = "i",
                                        V1029 = "i",
                                        V2001 = "i",
                                        V2003 = "i",
                                        V3003 = "l",
                                        V3003A = "i",
                                        V3009 = "l",
                                        V3009A = "i")) %>%
  clean_names() %>% # colocando nomes em caixa baixa
  mutate(one = 1,
         v1027 = as.numeric(v1027))
  

setwd("C:\\Users\\mgaldino\\2018\\Ta de Pe\\transfer_educ")

# carrega nomes das UFS
load(file="uf_df_pnad.RData") # está na pasta "C:/Users/mgaldino/2018/Ta de Pe/transfer_educ"

# adiciona nome das UFs na base da pnad
pnad2017 <- pnad2017 %>%
  left_join(uf, by="uf") %>%
  clean_names()

options( survey.lonely.psu = "adjust" )


pre_stratified <-  svydesign(
    ids = ~ upa , 
    strata = ~ estrato , 
    weights = ~ v1027 , 
    data = pnad2017 ,
    nest = TRUE
  )

# post-stratification targets
df_pos <- data.frame( posest = unique( pnad2017$posest ) , Freq = unique( pnad2017$v1029 ) )


# final survey design object
pnadc_design <- postStratify( pre_stratified , ~ posest , df_pos )

# remove the `pnad2017` data.frame object
# and the `pre_stratified` design before stratification
rm( pnad2017 , pre_stratified )

# recode
pnadc_design <- 
  update( 
    pnadc_design , 
    age_categories = factor( 1 + findInterval( v2009 , seq( 5 , 60 , 5 ) ) ) ,
    male = as.numeric( v2007 == 1 ) ,
    pia = as.numeric( v2009 >= 14 ) ,
    region = substr( uf , 1 , 1 )
  )

pnadc_design <- 
  update( 
    pnadc_design , 
    age = as.numeric(v2009),
    nursery_age = as.numeric( age %in% c(0:3) ),
    enrolled_nursery = as.numeric(ifelse(is.na(v3003a), 0,
                                         ifelse(v3003a == 1, 1, 0))),
    public_school = as.numeric(v3002a == 2),
    private_school = as.numeric(v3003a == 1),
    ocup_c = ifelse( pia == 1 , as.numeric( vd4002 %in% 1 ) , NA ) ,
    desocup30 = ifelse( pia == 1 , as.numeric( vd4002 %in% 2 ) , NA ) ,
    # calculate usual income from main job
    # (rendimento habitual do trabalho principal)
    vd4016n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4016 , NA ) ,
    # calculate effective income from main job
    # (rendimento efetivo do trabalho principal) 
    vd4017n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4017 , NA ) ,
    # calculate usual income from all jobs
    # (variavel rendimento habitual de todos os trabalhos)
    vd4019n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4019 , NA ) ,
    # calculate effective income from all jobs
    # (rendimento efetivo do todos os trabalhos) 
    vd4020n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4020 , NA ) ,
    # determine individuals who are either working or not working
    # (that is, the potential labor force)
    pea_c = as.numeric( ocup_c == 1 | desocup30 == 1 )
  )

# crianças de 0 a 3 anos matriculadas em creche
matricula_creche <- svyby( ~ nursery_age , ~ enrolled_nursery , pnadc_design , svytotal )

matricula_creche %>%
  mutate(total_criancas_0_3 =sum(nursery_age),
         perc_criancas = nursery_age/total_criancas_0_3)
# 0.2714878