install.packages("tree")
library(ggplot2)
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)
library(jpeg)
library(tree)

arq1 <- "C:\\Users\\LeonardoItalo\\Desktop\\purple_wave.jpg"
arq2 <- "C:\\Users\\LeonardoItalo\\Desktop\\xadrez_colorido.jpg"

img_purple <- readJPEG(arq1)
img_xadrez <- readJPEG(arq2)

img_dim <- dim(img_purple)

img_df <- data.frame(
  x = rep(1:img_dim[2], each = img_dim[1]),
  y = rep(img_dim[1]:1, img_dim[2]),
  r = as.vector(img_purple[,,1]),
  g = as.vector(img_purple[,,2]),
  b = as.vector(img_purple[,,3])
) %>%
  mutate(cor = rgb(r, g, b),
         id = 1:n())

# para reprodução
set.seed(1) 

# Parte 1) x, y, r, g
img_df_parte1 <- img_df %>% 
  sample_frac(3/5) %>% # separando 3/5 do banco
  mutate(b_backup = b, # backup do azul original
         b = 0, # retirando o azul da imagem
         cor = rgb(r, g, b)) # cor da imagem sem o azul

# Parte 2) x, y, r, g, b
img_df_parte2 <- img_df %>% filter(!id%in%img_df_parte1$id) # filtra as linhas que estão na Parte 1

# Imagem sem o azul
ggplot(data = img_df_parte1, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte1$cor) +
  labs(x = "x", y = "y", title = "Imagem sem B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()



# Apenas o azul da imagem
ggplot(data = img_df_parte2, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte2$cor) +
  labs(x = "x", y = "y", title = "Imagem sem B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()

#Exercício1

# uma amostra de 500 pontos para a análise descritiva (usar o banco inteiro é desnecessário e demorado)
img_df_amostra <- img_df %>% 
  sample_n(500,replace = FALSE)

cor(img_df_amostra %>% select(-cor, -id)) %>%  round(2)

pairs(img_df_amostra %>% select(-cor, -id))

#Exercício2

regb <- lm(b ~ r, data = img_df_parte2)
summary(regb)
#plot(regb)

#Exercício3

treeb <- tree(b ~ r, data = img_df_parte2)
summary(treeb)
plot(treeb)
text(treeb, pretty = 0)

#Exercício4

predict_lm <- predict(regb, img_df_parte1)
predict_tree <- predict(treeb, img_df_parte1)

img_df_parte1 %>% 
  mutate(predito_lm = predict_lm, predito_tree = predict_tree ) %>% tbl_df
