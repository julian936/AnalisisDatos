
library(haven)
library(dplyr)
library(readstata13)
library(FactoMineR)

#install.packages("readstata13")
#Cargamos el data set en la variable df
data_set_original<-read.dta13("C:/Users/julia/Downloads/Prueba estadistico.dta",fromEncoding="macroman")
#View(df)

#El primer paso a realizar es limpiear la informacion.


summary(df)

#variable id_u
#Esta variable es la llave de la tabla por lo cual solo debemos validar que no
#existen valores repetidos para ello hacemos uso de la libreria dplyr el metodo
#distinct el cual segmenta filas duplicadas por la columna id_u
df <- data_set_original %>% distinct(id_u, .keep_all = TRUE)


#variable sexo
#Esta variable es cualitativa y la pasamos a cuantitativa

count(df, sexo)

df_sexo_F <- df %>% 
  mutate(sexo = ifelse(sexo == "F", 1, sexo))

df_sexo_M <- df_sexo_F %>% 
  mutate(sexo = ifelse(sexo == "M", 0, sexo))

df00<-transform(df_sexo_M, sexo = as.numeric(sexo))


count(df00, sexo)


count(df00, tipodedeficienciadiagnosticada)


#Variable hemartrosisdurantelosultimos12me
#esta variable tiene registros como 5555, 9999 y NA los cuales son registros no necesarios
count(df00, hemartrosisdurantelosultimos12me)


df01 = filter(df00, hemartrosisdurantelosultimos12me<5555)

count(df01, hemartrosisdurantelosultimos12me)



#Variable v39
#esta variable tiene registros en NA los cuales son registros no necesarios

count(df01, v39)

df02 = filter(df01, v39>=0)

count(df02, v39)


#Variable v40
#esta variable tiene registros en NA los cuales son registros no necesarios

count(df02, v40)

df03 = filter(df02, v40>=0)

count(df03, v40)


#Variable v47
#esta variable tiene registros en NA los cuales son registros no necesarios
count(df03, v47)
df04 = filter(df03, v47>=0)
count(df04, v47)



#Variable v48
#esta variable tiene registros en NA los cuales son registros no necesarios
count(df04, v48)
df05 = filter(df04, v48>=0)
count(df05, v48)


#Variable v49
#esta variable tiene registros en NA los cuales son registros no necesarios
count(df05, v49)
df06 = filter(df05, v49>=0)
count(df06, v49)

#Variable presenciadeinhibidoralafechadeco
#esta variable tiene registros como 9999 y NA los cuales son registros no necesarios
count(df06, presenciadeinhibidoralafechadeco)
df07 = filter(df06, presenciadeinhibidoralafechadeco<9999)
count(df07, presenciadeinhibidoralafechadeco)
  
#Variable v54
#esta variable tiene 4157 de valores que no aplican segun la documentacion, al ser tantos valores que no aplican se elimina la columna
count(df07, v54)
df08 <- select(df07,id_u,sexo,tipodedeficienciadiagnosticada, hemartrosisdurantelosultimos12me,v39,v40,v47,v48,v49,presenciadeinhibidoralafechadeco,diagnosticodelpaciente,edad_simple,grupo_etario,Tasa1  )
#count(df08, v54)


count(df08, diagnosticodelpaciente)


count(df08, edad_simple)


count(df08, grupo_etario)
#Variable v54
#esta variable la volvemos cuantitativa


dfge17 <- df08 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 75 A 79 A-OS", 17, grupo_etario))

dfge16 <- dfge17 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 70 A 74 A-OS", 16, grupo_etario))

dfge15 <- dfge16 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 65 A 69 A-OS", 15, grupo_etario))

dfge14 <- dfge15 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 60 A 64 A-OS", 14, grupo_etario))

dfge13 <- dfge14 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 55 A 59 A-OS", 13, grupo_etario))

dfge12 <- dfge13 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 50 A 54 A-OS", 12, grupo_etario))

dfge11 <- dfge12 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 5 A 9 A-OS", 11, grupo_etario))

dfge10 <- dfge11 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 45 A 49 A-OS", 10, grupo_etario))

dfge09 <- dfge10 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 40 A 44 A-OS", 09, grupo_etario))

dfge08 <- dfge09 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 35 A 39 A-OS", 08, grupo_etario))

dfge07 <- dfge08 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 30 A 34 A-OS", 07, grupo_etario))

dfge06 <- dfge07 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 25 A 29 A-OS", 06, grupo_etario))

dfge05 <- dfge06 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 20 A 24 A-OS", 05, grupo_etario))

dfge04 <- dfge05 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 15 A 19 A-OS", 04, grupo_etario))

dfge03 <- dfge04 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 10 A 14 A-OS", 03, grupo_etario))

dfge02 <- dfge03 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "DE 0 A 4 A-OS", 02, grupo_etario))

dfge01 <- dfge02 %>% 
  mutate(grupo_etario = ifelse(grupo_etario == "80 A-OS Y MAS", 01, grupo_etario))


df09<-transform(dfge01, grupo_etario = as.numeric(grupo_etario))

count(df09, grupo_etario)

#aaaa <- df00 %>% filter( !str_detect( grupo_etario, "#A\xd1OS"))
#aaaaa<-gsub("45","",df01$grupo_etario)



count(df09, Tasa1)

#VAlidamos que las variables sean positivas
df10 = filter(df09, id_u>=1)
df11 = filter(df10, tipodedeficienciadiagnosticada>=0)
df12 = filter(df11, hemartrosisdurantelosultimos12me>=0)
df13 = filter(df12, v39>=0)
df14 = filter(df13, v40>=0)
df15 = filter(df14, v47>=0)
df16 = filter(df15, v48>=0)
df17 = filter(df16, v49>=0)
df18 = filter(df17, presenciadeinhibidoralafechadeco>=0)
df19 = filter(df18, diagnosticodelpaciente>=0)
df20 = filter(df19, edad_simple>=0)
df21 = filter(df20, Tasa1>=0)


#Finalmente eliminamos el id_u
data_set_limpio <- select(df21,sexo,tipodedeficienciadiagnosticada, hemartrosisdurantelosultimos12me,v39,v40,v47,v48,v49,presenciadeinhibidoralafechadeco,diagnosticodelpaciente,edad_simple,grupo_etario,Tasa1  )

summary(data_set_limpio)



#Analisis de Correspondencia simples usando CA

AC<-CA(data_set_limpio, ncp=13)

ACP = PCA(data_set_limpio)

AC$eig

windows()
plot(AC,axes = c(1,3))

windows()
plot(AC,axes = c(4,6))

windows()
plot(AC,axes = c(2,6))



#Regresion lineal multiple usando lm

modeloInicial = lm(tipodedeficienciadiagnosticada ~ sexo+hemartrosisdurantelosultimos12me+v39+v40+v47+v48+v49+presenciadeinhibidoralafechadeco+diagnosticodelpaciente+edad_simple+grupo_etario+Tasa1,
                   data = data_set_limpio)

summary(modeloInicial)




step(modeloInicial, direction = "backward")

step(modeloInicial, direction = "both")


mdlCir0<-lm(tipodedeficienciadiagnosticada~1,data=data_set_limpio)

step(mdlCir0,direction ="forward", ~ sexo+hemartrosisdurantelosultimos12me+v39+v40+v47+v48+v49+presenciadeinhibidoralafechadeco+diagnosticodelpaciente+edad_simple+grupo_etario+Tasa1)


summary(mdlCir0)


#cris
pairs(x = data_set_limpio, lower.panel = NULL)

cor(x = data_set_limpio, method = "pearson")
