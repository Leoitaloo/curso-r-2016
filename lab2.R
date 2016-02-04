install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("RSQLite")
library(RSQLite)
install.packages("openxlsx")
library(openxlsx)
install.packages("readxl")
library(readxl)

#Parte1

 pathname <- "C:\\Users\\LeonardoItalo\\Desktop\\CursoR\\"
 arq <- paste0(pathname,"pnud.rda")
 arq2 <- paste0(pathname,"pnud.rds")
 arq3 <- paste0(pathname,"pnud.sqlite")
 arq4 <- paste0(pathname,"pnud_linux.csv")
 arq5 <- paste0(pathname,"pnud_linux.txt")
 arq6 <- paste0(pathname,"pnud_linux.xlsx")
 arq7 <- paste0(pathname,"pnud_win.csv")
 arq8 <- paste0(pathname,"pnud_win.txt")
 arq9 <- paste0(pathname,"pnud_win.xlsx")
 arq10 <- paste0(pathname,"pnud2_linux.csv")
 arq11 <- paste0(pathname,"pnud2_linux.csv")
 
 #Arquivo .rda
 load(arq)
 pnud_rda <- d
 summary(pnud_rda[,1:10])
 
 #Arquivo .rds
 pnud_rds <- readRDS(arq2)
 summary(pnud_rds[,1:10])
 
 #Arquivo .sqlite
 pnud_sqlite <- tbl(src_sqlite(path = arq3, create = TRUE), "pnud")
 pnud_sqlite
 
 #Arquivo linux.csv
 pnud_linux_csv <- read.table(arq4, sep = ",", header = T, dec = ".")
 pnud_linux_csv %>% tbl_df %>% summary()
 pnud_linux_csv$mulh0a4[6514]
 
 #Arquivo linux.txt com problema na linha 6514
 pnud_linux_txt <- read.table(arq5)
 pnud_linux_txt %>% tbl_df %>% summary()
 pnud_linux_txt$mulh0a4[6514]
 
 #Arquivo linux.xlsx
 pnud_linux_xlsx <- read.xlsx(arq6)
 pnud_linux_xlsx %>% tbl_df %>% summary()
 pnud_linux_xlsx$mulh0a4[6514]
 
 #Arquivo win.csv
 pnud_win_csv <- read.table(arq7, sep = ",", header = T, dec = ".")
 pnud_win_csv %>% tbl_df %>% summary()
 pnud_win_csv$mulh0a4[6514]
 
 #Arquivo win.txt com problema na linha 6514
 pnud_win_txt <- read_delim(arq8, " ")
 pnud_win_txt %>% tbl_df %>% summary()
 pnud_win_txt$mulh0a4[6514]
 
 #Arquivo win.xlsx
 pnud_win_xlsx <- read_excel(arq9)
 pnud_win_xlsx %>% tbl_df %>% summary()
 pnud_win_xlsx$mulh0a4[6514]
 
 #Arquivo linux2.csv com problema na linha 6514
 pnud2_linux_csv <- read_csv2(arq10)
 pnud2_linux_csv %>% tbl_df %>% summary()
 pnud2_linux_csv$mulh0a4[6514]
 
 #Arquivo win2.csv com problema na linha 6514
 pnud2_win_csv <- read_delim(arq11, ";" )
 pnud2_win_csv %>% tbl_df %>% summary()
 pnud2_win_csv$mulh0a4[6514]
 
#Parte2
 
 
