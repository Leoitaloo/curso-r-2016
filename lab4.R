library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

#Diamonds
#1
#Se não for especificado no nível superior da plotagem, são mapping e data

#2
diamonds %>% 
  ggplot(aes(x = price, y = carat)) + geom_point() + xlab("Preço") +  ylab("Quilates")

#3
diamonds %>% 
  ggplot(aes(x = price, y = carat)) + geom_point() + xlab("Preço") +  ylab("Quilates") + facet_grid(.~clarity)

#4
#Se não for especificado no nível superior da plotagem, são mapping e data

#5
diamonds %>% 
ggplot(aes(x = price)) + geom_histogram(col = "red", aes(fill=..count..))

#6
diamonds %>% 
  ggplot(aes(x = price)) + geom_density()

#7
#Se não for especificado no nível superior da plotagem, são mapping e data

#8
diamonds %>% 
  ggplot(aes(x = cut, y = price, fill = as.factor(cut))) + geom_boxplot()

#9
diamonds %>% 
  ggplot(aes(x = cut, y = price)) + geom_boxplot() + scale_colour_manual(values=cbPalette )

#10
#Se não for especificado no nível superior da plotagem, são mapping e data

#11
diamonds %>% 
  ggplot(aes(x = as.factor(color))) + geom_bar()

#Economics

#12
economics %>% 
  ggplot(aes(x = uempmed, y = unemploy)) + geom_point() + xlab("Semanas") +  ylab("Número de Desempregados")

#13
economics %>% 
  mutate(Ano = year(date)) %>% 
  ggplot(aes(x = uempmed, y = unemploy), colours = Ano) + geom_point() + xlab("Ano") +  ylab("Número de Desempregados")

#14
economics %>% 
  ggplot(aes(x = date, y = unemploy)) + geom_line() + xlab("Ano") +  ylab("Número de Desempregados")

#15
economics %>% 
  mutate(Ano = year(date)) %>% 
  group_by(Ano) %>% 
  summarise(med = mean(unemploy)) %>% 
  ggplot(aes(x = Ano, y = med)) + geom_line() + xlab("Ano") +  ylab("Número de Desempregados")

#16
economics %>% 
  mutate(Ano = year(date)) %>% 
  group_by(Ano) %>% 
  summarise(med = median(unemploy), 
            qmin = quantile(unemploy, 0.01), 
            qmax = quantile(unemploy, 0.99)) %>% 
  ggplot(aes(x = Ano, y = med)) + geom_line() + xlab("Ano") +  ylab("Número de Desempregados") + geom_ribbon(aes(ymin = qmin , ymax = qmax), alpha = 0.2)

#17
economics %>%
  mutate(txa_desemprego = unemploy/pop) %>% 
  select(date,pce,psavert,txa_desemprego) %>% 
  gather(key = variavel, value = valor, pce, psavert, txa_desemprego) %>% 
  ggplot(aes(x = date, y = valor)) + geom_line() + facet_grid(variavel~., scales = "free")
  


