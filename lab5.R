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

##Imagem Roxa

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

# para reprodu√ß√£o
set.seed(1) 

# Parte 1) x, y, r, g
img_df_parte1 <- img_df %>% 
  sample_frac(3/5) %>% # separando 3/5 do banco
  mutate(b_backup = b, # backup do azul original
         b = 0, # retirando o azul da imagem
         cor = rgb(r, g, b)) # cor da imagem sem o azul

# Parte 2) x, y, r, g, b
img_df_parte2 <- img_df %>% filter(!id%in%img_df_parte1$id) # filtra as linhas que est√£o na Parte 1

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

#Exerc√?cio1

# uma amostra de 500 pontos para a an√°lise descritiva (usar o banco inteiro √© desnecess√°rio e demorado)
img_df_amostra <- img_df %>% 
  sample_n(500,replace = FALSE)

cor(img_df_amostra %>% select(-cor, -id)) %>%  round(2)

pairs(img_df_amostra %>% select(-cor, -id))

#Exerc√?cio2

regb1 <- lm(b ~ r, data = img_df_parte2)
summary(regb1)
#plot(regb)

#Exerc√?cio3

treeb1 <- tree(b ~ r, data = img_df_parte2)
summary(treeb1)
plot(treeb1)
text(treeb1, pretty = 0)

#Exerc√?cio4
#Modelo linear
predict_lm <- predict(regb1, img_df_parte1)

img_df_parte1$new.col <- predict_lm #È preciso criar um novo "cor"
img_df_parte1$new.cor <- rgb(img_df_parte1$r, img_df_parte1$g, abs(img_df_parte1$new.col)) #transformando os n˙meros em positivos
wave_predito_lm <- ggplot(data = img_df_parte1, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte1$new.cor) +
  labs(x = "x", y = "y", title = "Previs„o LM") +
  coord_fixed(ratio = 1) +
  theme_bw()
wave_predito_lm

#Modelo Tree
predict_tree <- predict(treeb1, img_df_parte1)

img_df_parte1$new.col <- predict_tree #È preciso criar um novo "cor"
img_df_parte1$new.cor <- rgb(img_df_parte1$r, img_df_parte1$g, abs(img_df_parte1$new.col)) #transformando os n˙meros em positivos
wave_predito_tree <- ggplot(data = img_df_parte1, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte1$new.cor) +
  labs(x = "x", y = "y", title = "Previs„o Tree") +
  coord_fixed(ratio = 1) +
  theme_bw()
wave_predito_tree

#Erro
img_df_parte1 <- img_df_parte1 %>% 
  mutate(erro_lm = (b - predict_lm)^2, erro_tree = (b - predict_tree)^2)


erro_total_lm <- sum(img_df_parte1$erro_lm1)
erro_total_lm

erro_total_tree <- sum(img_df_parte1$erro_arv1)


#O modelo de regress„o linear apresenta um resultado melhor

##Imagem Xadrez

img_dim2 <- dim(img_xadrez)

img_df2 <- data.frame(
  x = rep(1:img_dim2[2], each = img_dim2[1]),
  y = rep(img_dim2[1]:1, img_dim2[2]),
  r = as.vector(img_xadrez[,,1]),
  g = as.vector(img_xadrez[,,2]),
  b = as.vector(img_xadrez[,,3])
) %>%
  mutate(cor = rgb(r, g, b),
         id = 1:n())

# para reprodu√ß√£o
set.seed(1) 

# Parte 1) x, y, r, g
img_df_parte12 <- img_df2 %>% 
  sample_frac(3/5) %>% # separando 3/5 do banco
  mutate(b_backup = b, # backup do azul original
         b = 0, # retirando o azul da imagem
         cor = rgb(r, g, b)) # cor da imagem sem o azul

# Parte 2) x, y, r, g, b
img_df_parte22 <- img_df2 %>% filter(!id%in%img_df_parte12$id) # filtra as linhas que est√£o na Parte 1

# Imagem sem o azul
ggplot(data = img_df_parte12, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte12$cor) +
  labs(x = "x", y = "y", title = "Imagem sem B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()



# Apenas o azul da imagem
ggplot(data = img_df_parte22, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte22$cor) +
  labs(x = "x", y = "y", title = "Imagem sem B (azul)") +
  coord_fixed(ratio = 1) +
  theme_bw()

#Exerc√?cio1

# uma amostra de 500 pontos para a an√°lise descritiva (usar o banco inteiro √© desnecess√°rio e demorado)
img_df_amostra1 <- img_df2 %>% 
  sample_n(500,replace = FALSE)

cor(img_df_amostra1 %>% select(-cor, -id)) %>%  round(2)

pairs(img_df_amostra1 %>% select(-cor, -id))

#Exerc√?cio2

regb2 <- lm(b ~ r, data = img_df_parte22)
summary(regb2)
#plot(regb)

#Exerc√?cio3

treeb2 <- tree(b ~ r, data = img_df_parte22)
summary(treeb2)
plot(treeb2)
text(treeb2, pretty = 0)

#Exerc√?cio4

predict_lm2 <- predict(regb2, img_df_parte12)

img_df_parte11$new.col <- predict_lm2 #È preciso criar um novo "cor"
img_df_parte11$new.cor <- rgb(img_df_parte12$r, img_df_parte12$g, abs(img_df_parte12$new.col)) #transformando os n˙meros em positivos
xadrez_predito_lm <- ggplot(data = img_df_parte12, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte12$new.cor) +
  labs(x = "x", y = "y", title = "Previs„o LM") +
  coord_fixed(ratio = 1) +
  theme_bw()
xadrez_predito_lm

predict_tree2 <- predict(treeb2, img_df_parte12)

img_df_parte11$new.col <- predict_tree2 #È preciso criar um novo "cor"
img_df_parte11$new.cor <- rgb(img_df_parte12$r, img_df_parte12$g, abs(img_df_parte12$new.col)) #transformando os n˙meros em positivos
xadrez_predito_tree <- ggplot(data = img_df_parte12, aes(x = x, y = y)) + 
  geom_point(colour = img_df_parte12$new.cor) +
  labs(x = "x", y = "y", title = "Previs„o Tree") +
  coord_fixed(ratio = 1) +
  theme_bw()
xadrez_predito_tree

#Erro
img_df_parte12 <- img_df_parte12 %>% 
  mutate(erro_lm1 = (b - predict_lm1)^2, erro_tree1 = (b - predict_tree)^2)


erro_total_lm <- sum(img_df_parte12$erro_lm1)
erro_total_lm

erro_total_tree <- sum(img_df_parte12$erro_arv1)
erro_total_tree

#O modelo pela ·rvore de decis„o apresenta um resultado melhor

