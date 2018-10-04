lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis", "survey") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

install.packages("survyr")

"survey"juizes_drogas_CL <- decisoes %>%
  select(juiz,municipio,txt_decisao,data_registro,data_decisao) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[ií]na"),
         tempo = dmy(data_registro) - dmy(data_decisao)) %>%
  filter(droga ==TRUE, municipio %in% c("Campinas","Limeira")) %>%
  group_by(juiz) %>%
  summarise(tempo_medio = mean(tempo,na.rm=T))

write_rds(juizes_drogas_CL, "C:/Users/ML/Documents/Aulas_ENAP/juizes_drogas_CL.rds")


processo <- read_rds("C:/Users/ML/Documents/Aulas_ENAP/Dados/processos_nested.rds")
deciso

dec_gather <- decisoes %>%
  filter(!is.na(id_decisao)) %>%
  select(id_decisao:data_registro) %>%
  gather(key="variavel", value="valor",-id_decisao) %>%
  arrange(id_decisao)


levels(factor(dec_gather$variavel))

decisoes_spread <- decisoes%>%
  filter(!is.na(id_decisao))%>%
  select(id_decisao:data_registro)%>% 
  gather(key, value,-id_decisao) %>%
  spread(key, value)


juizes_drogasL <- decisoes %>%
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[ií]na"),
         droga=case_when(
           droga==TRUE ~ "droga",
           droga==FALSE ~"n_droga" )) %>%
         group_by(juiz,droga) %>%
summarise(n=n()) %>%
spread(droga,n,fill = 0) %>%
mutate(total=droga+n_droga,
       proporcao=droga/total) %>%
  arrange(-proporcao)
  
juizes_mes <- decisoes %>%
  mutate(mes = month(dmy(data_decisao))) %>%
  filter(!is.na(mes)) %>% 
  group_by(juiz,mes) %>% 
  summarise(n=n()) %>% 
  spread(mes,n,fill = 0)
  

juizes_mes <- decisoes %>%
  mutate(data_decisao_corrigida=dmy(data_decisao),
    mes = month(data_decisao_corrigida)) %>%
  filter(!is.na(mes)) %>% 
  group_by(juiz,mes) %>% 
  summarise(n=n()) %>% 
  spread(mes,n,fill = 0)
    
decisoes_sep <- decisoes %>% 
select(n_processo, classe_assunto) %>%
  separate(classe_assunto, 
           c('classe', 'assunto'), 
           sep = '/',
           extra = 'merge', 
           fill = 'right') %>% 
  count(assunto, sort = TRUE)

d_partes <- processo %>%
  select(n_processo, partes) %>%
  unnest(partes)


decisoes %>%
  distinct(juiz, municipio)
