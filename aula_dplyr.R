
library(data.table)
library(dplyr)





















library(data.table) # esta library é pra ler os dados
library(dplyr)

dados <- fread("menu.csv")

glimpse(dados)

summary(dados)

hist(dados$Cholesterol)


boxplot(dados$Cholesterol)























dados <- fread("menu.csv")

glimpse(dados)

dados_reduzidos <- select(dados, Category, Item) # este comando seleciona as variáveis

dados_unhealthy <-  filter(dados, Calories > 400, Cholesterol > 100, Sodium > 800)


dados_reduzidos <- dados %>%
                   filter(Calories > 400, Cholesterol > 100, Sodium > 800) %>%
                   select(Category, Item)

dados_sodium <- dados %>% 
                select(Category, Item, Sodium) %>% 
                mutate(Sodium_por_mil = Sodium / 1000)

dados_sodium <- dados %>% 
                transmute(Category, Item, Sodium_por_mil = Sodium / 1000)



dados_sodium_media <- dados %>% 
                      group_by(Category) %>%
                      summarise(media_sodium = mean(Sodium),
                                media_cholesterol = mean(Cholesterol)) %>% 
                      arrange(-media_sodium)

 #por categoria
  #   media de colesterol nos casos onde a comida é zero açucar
   #  media em ordem decrescente
     
dados_cholesterol <- dados %>% 
                     select(Category, Cholesterol, Sugars) %>%  # não precisa, so se forem muitos dados
                     filter(Sugars == 0) %>%
                     group_by(Category) %>%
                     summarise(media_cholesterol = mean(Cholesterol)) %>%
                     arrange(-media_cholesterol)