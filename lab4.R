library(ggplot2)
install.packages("lubridate")
library(lubridate)

diamonds %>% 
  ggplot(aes(x = price, y = carat)) + geom_point() + xlab("Preço") +  ylab("Quilates")


diamonds %>% 
  ggplot(aes(x = price, y = carat)) + geom_point() + xlab("Preço") +  ylab("Quilates") + facet_grid(.~clarity)

diamonds %>% 
ggplot(aes(x = price)) + geom_histogram(col = "red", aes(fill=..count..))


diamonds %>% 
  ggplot(aes(x = price)) + geom_density()

diamonds %>% 
  ggplot(aes(x = cut, y = price, fill = as.factor(cut))) + geom_boxplot()

diamonds %>% 
  ggplot(aes(x = cut, y = price)) + geom_boxplot() + scale_colour_manual(values=cbPalette )

diamonds %>% 
  ggplot(aes(x = as.factor(color))) + geom_bar

