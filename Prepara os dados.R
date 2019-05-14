#Instala e carrega pacotes necessários

pacotes <- c("survey", "srvyr", "tidyverse", "devtools")

lapply(pacotes, install.packages)

lapply(pacotes, library, character.only = TRUE)

install_github("ajdamico/lodown", dependencies = TRUE)

library(lodown)

#Realiza download do catalogo da PNS 2013 e desta

pns_cat <-
  get_catalog("pns" ,
              output_dir = file.path(path.expand("~") , "PNS"))

pns_cat <- lodown("pns" , pns_cat)


#Carrega a PNS 2013

options(survey.lonely.psu = "adjust")

pns_design1 <-
  readRDS(file.path(
    path.expand("~") ,
    "PNS" ,
    "2013 long questionnaire survey design.rds"
  ))


pns_design <- as_survey_design(pns_design1)

#Adequação de variáveis

pns_design$variables$q002 <- as.factor(pns_design$variables$q002)

pns_design$variables$q030 <- as.factor(pns_design$variables$q030)

pns_design$variables$q074 <- as.factor(pns_design$variables$q074)

pns_design$variables$q063 <- as.factor(pns_design$variables$q079)

pns_design$variables$q068 <- as.factor(pns_design$variables$q063)

attach(pns_design$variables)

comorb_var <- data.frame(q002, q030, q074, q079, q063)

summary(comorb_var)

detach(pns_design$variables)

pns_design$variables$q002 <- na_if(pns_design$variables$q002, "")

pns_design$variables$q030 <- na_if(pns_design$variables$q030, "")

pns_design$variables$q002 <-
  droplevels.factor(pns_design$variables$q002)

pns_design$variables$q030 <-
  droplevels.factor(pns_design$variables$q030)

#Cria variável derivada: Comorbidade

pns_design <- mutate(
  pns_design,
  q002 = recode_factor(
    q002,
    '1' = "1",
    '2' = "2",
    '3' = "2"
  ),
  q030 = recode_factor(
    q030,
    '1' = "1",
    '2' = "2",
    '3' = "2"
  )
)

pns_design <-
  update(pns_design ,
         
         one = 1 ,
         
         V1 = as.factor(
           case_when(
             q002 == "2" &
               q030 == "2" &
               q074 == "2" &
               q079 == "2" &
               q063 == "2"  ~ "D0",
             q002 == "1" &
               q030 == "2" &
               q074 == "2" &
               q079 == "2" &
               q063 == "2"  ~ "D1",
             q002 == "2" &
               q030 == "1" &
               q074 == "2" &
               q079 == "2" &
               q063 == "2" ~ "D1",
             q002 == "2" &
               q030 == "2" &
               q074 == "1" &
               q079 == "2" &
               q063 == "2" ~ "D1",
             q002 == "2" &
               q030 == "2" &
               q074 == "2" &
               q079 == "1" &
               q063 == "2" ~ "D1",
             q002 == "2" &
               q030 == "2" &
               q074 == "2" &
               q079 == "2" &
               q063 == "1"  ~ "D1",
             TRUE ~ "D2"
           )
         ))

pns_design$variables$V1

summary(pns_design$variables$V1)

#Cria e modifica variáveis

pns_design <- mutate(pns_design,
                     V2 = recode_factor(
                       p050,
                       '1' = "T1",
                       '2' = "T1",
                       "3" = "T2"
                     ))


pns_design$variables$c009 <-
  droplevels.factor(pns_design$variables$c009, exclude = 9)

pns_design <-
  update(pns_design ,
         
         one = 1 ,
         
         V3 = as.factor(case_when(p035 < 3 ~ "S1",
                                  p035 >= 3 ~ "S2")))


#Prepara dataframe para exportação

attach(pns_design$variables)

DADOS <-
  data.frame(V1,
             V2,
             V3)


DADOS <- na.omit(DADOS)

detach(pns_design$variables)

#Exporta dados

save(DADOS, file = "DADOS.Rda")
