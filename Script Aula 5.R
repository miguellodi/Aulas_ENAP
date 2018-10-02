lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

juizes_drogas_CL <- decisoes %>%
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
      