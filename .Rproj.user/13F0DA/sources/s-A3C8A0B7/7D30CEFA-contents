library(stringr)
library(dplyr)
library(rtweet)
library(purrr)
library(abjutils)

robotox<- rtweet::get_timeline("orobotox",n=3200)

robotox%>% filter(text,str_extract("|"))




textos_robotox<- robotox%>% select(text)




textos_robotox$text<- str_replace_all(textos_robotox$text, "[\r\n]" , " ")

pos_agrotoxico<- str_which(textos_robotox$text, "[|]")


agro_tweet <- robotox[pos_agrotoxico,]%>%select(created_at, status_id)

agro_tweet$id <- as.numeric(row.names(agro_tweet))


agrotoxicos<- textos_robotox$text[pos_agrotoxico]




write(agrotoxicos,file="agrotoxico.csv")

library(readr)
agrotoxico <- read_delim("agrotoxico.csv", 
                         "|", escape_double = FALSE, col_names = FALSE, 
                         locale = locale(encoding = "LATIN1"), 
                         trim_ws = TRUE)

agrotoxico$id <- as.numeric(row.names(agrotoxico))

names(agrotoxico)[1:4]<- c("Produtor","Marca","Classificacao_toxicologica","Aplicacao")

agrotoxico<- agrotoxico %>% inner_join(agro_tweet)


agrotoxico$Aplicacao[pos_alvo_biologico] <- "Alvos Biológicos"


continuacao<- str_which(robotox$text,"2/2")

tweet_continuacao<-robotox[continuacao,] %>% 
  select(text, reply_to_status_id)

names(tweet_continuacao)[2]<- "status_id"




id_cont<- tweet_continuacao$reply_to_status_id


texto_completo<- agrotoxico%>%
  filter(status_id %in% id_cont  ) %>%
  select(status_id, Aplicacao)


novo_texto<- texto_completo %>% 
  inner_join(tweet_continuacao) %>%
  mutate(novo_texto= str_c(Aplicacao,text))





novo_texto$novo_texto<- str_replace_all(string = novo_texto$novo_texto, 
                pattern = "[[:punct:]][1-2][[:punct:]][1-2][[:punct:]]", 
                replacement = "")

agrotoxico<-agrotoxico %>%
  left_join(novo_texto[1:11,])%>%
  mutate(Aplicacao=ifelse(!is.na(novo_texto),novo_texto,Aplicacao)) %>%
  filter(!is.na(Classificacao_toxicologica)) %>%
  select(1:7)

classificacao_toxicologica<- agrotoxico[169:266, 4]$Aplicacao
aplicacao<- agrotoxico[169:266, 3]$Classificacao_toxicologica

agrotoxico[169:266, 3]<- classificacao_toxicologica
agrotoxico[169:266, 4]<- aplicacao


agrotoxico[agrotoxico$id==187,3]<- "Classificação toxicológica: Classe III - Medianamente Tóxico."
agrotoxico[agrotoxico$id%in%c(188, 190, 211, 214,257,265),3]<- "Classificação toxicológica: Classe IV - Pouco Tóxico."


agrotoxico[agrotoxico$id==199,3]<- "Classificação toxicológica: Classe I - Extremamente Tóxico."  

agrotoxico[agrotoxico$id==206,3]<- "Classificação toxicológica: Classe III - Medianamente Tóxico."  
agrotoxico[agrotoxico$id==220,3]<- "Classificação toxicológica: Classe III - Medianamente Tóxico."  
agrotoxico[agrotoxico$id==233,3]<- "Classificação toxicológica: Classe I - Extremamente Tóxico."  
agrotoxico[agrotoxico$id==271,3]<- "Classificação toxicológica: Classe III - Medianamente Tóxico."  


agrotoxico[agrotoxico$id==206,2]<- NA  
agrotoxico[agrotoxico$id==220,2]<- NA
agrotoxico[agrotoxico$id==233,2]<- NA
agrotoxico[agrotoxico$id==271,2]<- NA


agrotoxico[agrotoxico$id==199,4]<- "Indicado para as culturas de Batata (dessecante), Café, Citros, Feijão, Feijão (dessecante), Soja e Soja (dessecante)."  

agrotoxico[agrotoxico$id==206,4]<- "Trata-se de produto técnico."  
agrotoxico[agrotoxico$id==220,4]<- "Indicado para as culturas de Begônia, Gérbera, Kalanchoe e Rosa."  
agrotoxico[agrotoxico$id==233,4]<- "Indicado para as culturas de Milho e Soja."  
agrotoxico[agrotoxico$id==271,4]<- "Trata-se de produto técnico."  

agrotoxico[agrotoxico$id %in% c(38,43,49, 166, 167), 4]<- "	Alvos Biológicos"  



agrotoxico$Marca<- str_replace_all(string = agrotoxico$Marca, 
                pattern = "Marca comercial: ", 
                replacement = "")

agrotoxico$Aplicacao<- str_replace_all(string = agrotoxico$Aplicacao, 
                pattern = "Indicado para as culturas de ", 
                replacement = "")

agrotoxico$Aplicacao<- str_replace_all(string = agrotoxico$Aplicacao, 
                                       pattern = "Indicado para culturas de ", 
                                       replacement = "")


agrotoxico$Aplicacao<- str_replace_all(string = agrotoxico$Aplicacao, 
                                       pattern = "Indicado para a cultura da ", 
                                       replacement = "")

agrotoxico$Aplicacao<- str_replace_all(string = agrotoxico$Aplicacao, 
                                       pattern = "Indicado para a cultura de ", 
                                       replacement = "")

agrotoxico$Aplicacao<- str_replace_all(string = agrotoxico$Aplicacao, 
                                       pattern = "Indicado para a cultura do ", 
                                       replacement = "")


agrotoxico$Aplicacao<- str_replace_all(string = agrotoxico$Aplicacao, 
                                       pattern = " e ", 
                                       replacement = ", ")




agrotoxico<- agrotoxico %>% 
  mutate(Is_produto_tecnico = ifelse(Aplicacao=="Trata-se de produto técnico.",1,0))
  



#search_2019_08_03_19_11<- search_tweets(q="STF 'Lava Jato'", n=10000, type="mixed")

#rtweet::write_as_csv(search_2019_08_03_19_11, file_name = "search_2019_08_03_19_11")


culturas<- agrotoxico%>%select(id, Aplicacao)

culturas$Aplicacao<- str_replace_all(string = culturas$Aplicacao, 
                                       pattern = "[.]+$", 
                                       replacement = "")

culturas$Aplicacao<- str_replace_all(string = culturas$Aplicacao, 
                                     pattern = "[.]", 
                                     replacement = ",")

culturas$Aplicacao<- str_replace_all(string = culturas$Aplicacao, 
                                     pattern = "[.]", 
                                     replacement = ",")



df_culturas<-map_dfr(1:NROW(culturas),function(k){
  vector<- unlist(strsplit(culturas$Aplicacao[k], split=", "))
  vector<- matrix(vector)
  data.frame(id=rep(culturas$id[k],NROW(vector)),cultura= vector)
  
})

df_culturas$cultura <- tolower(df_culturas$cultura)


pos_cana<- str_which(df_culturas$cultura,
          pattern = "^cana")

df_culturas$cultura[pos_cana]<- "cana-de-açúcar"

df_culturas$cultura[pos_cana]<- "cana-de-açúcar"


df_culturas$cultura<-str_replace_all(df_culturas$cultura,
                     pattern = "batata,",
                replacement = "batata")


df_culturas$cultura<-str_replace_all(df_culturas$cultura,
                                     pattern = "côco",
                                     replacement = "coco")


df_culturas$cultura<-str_replace_all(df_culturas$cultura,
                                     pattern = "indicado para pastagens",
                                     replacement = "pastagem")


df_culturas$cultura<-str_replace_all(df_culturas$cultura,
                                     pattern = "pastagens",
                                     replacement = "pastagem")

sort(unique(df_culturas$cultura))

df_join_agro_cultura<- agrotoxico %>% inner_join(df_culturas)

save(list=c("df_culturas", "agrotoxico", "df_join_agro_cultura"), file = "data/robotox.RData")



load("data/robotox.RData")

df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classe I - Extremamente Tóxico"] <- "Classe I - Extremamente Tóxico"                                  
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe I - Extremamente Tóxico."] <- "Classe I - Extremamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II - Altamente tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II - Altamente tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II - Altamente Tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II- Produto Altamente Tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação: Classe II - Altamente Tóxico"] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III"] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Extremamente Tóxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Medianamente Tóxico"] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Medianamente Toxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Medianamente Tóxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Mediante Tóxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe IV - Pouco Tóxico."] <-  "Classe IV - Pouco Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe IV - Produto Pouco Tóxico."] <-  "Classe IV - Pouco Tóxico"   


save(list=c("df_culturas", "agrotoxico", "df_join_agro_cultura"), file = "data/robotox.RData")
