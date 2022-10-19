library(readxl)
base <- read_excel("ENFT_abril2021_Mincer_sheepskin.xlsx")

#Limpiar base sacando a individuos sin información educativa.
library(dplyr)
base1 <- filter(base,!is.na(base$EFT_ULT_NIVEL_ALCANZADO))

#Crear variable educ, años educativos, a fin de comparar modelos.
base1$educ <- NA
base1$educ[base1$EFT_ULT_NIVEL_ALCANZADO==2] <- base1[base1$EFT_ULT_NIVEL_ALCANZADO==2,]$EFT_ULT_ANO_APROBADO #primaria
base1$educ[base1$EFT_ULT_NIVEL_ALCANZADO==3] <- base1[base1$EFT_ULT_NIVEL_ALCANZADO==3,]$EFT_ULT_ANO_APROBADO + 8 #secundaria
base1$educ[base1$EFT_ULT_NIVEL_ALCANZADO==4] <- base1[base1$EFT_ULT_NIVEL_ALCANZADO==4,]$EFT_ULT_ANO_APROBADO + 8 #vocaional
base1$educ[base1$EFT_ULT_NIVEL_ALCANZADO==5] <- base1[base1$EFT_ULT_NIVEL_ALCANZADO==5,]$EFT_ULT_ANO_APROBADO + 12 #unversitario
base1$educ[base1$EFT_ULT_NIVEL_ALCANZADO==6] <- base1[base1$EFT_ULT_NIVEL_ALCANZADO==6,]$EFT_ULT_ANO_APROBADO + 16 #postuniversitario
base1$educ[base1$EFT_ULT_NIVEL_ALCANZADO==7] <- 0 #ninguno
base1$educ[base1$EFT_ULT_NIVEL_ALCANZADO==1] <- 0

#Crear variable binarias a fines de comparación.

base1$casado <- 0
base1$casado[base1$EFT_ESTADO_CIVIL==2] <- 1
base1$casado[base1$EFT_ESTADO_CIVIL==6] <- 0

base1$mujer <- 0
base1$mujer[base1$EFT_SEXO==1] <- 0
base1$mujer[base1$EFT_SEXO==2] <- 1

#Crear Variables categoricas educativas

# primaria
table(base1$COMPLETO_EDUCACION_PRIMARIA)
table(is.na(base1$COMPLETO_EDUCACION_PRIMARIA)) #7492 vacia
base1$dip_primaria <- NA
base1$dip_primaria[base1$COMPLETO_EDUCACION_PRIMARIA==2] <- 0
base1$dip_primaria[base1$COMPLETO_EDUCACION_PRIMARIA==1] <- 1 #9114
table(base1$dip_primaria)
sum(table(base1$dip_primaria)) #17369

# secundaria
table(base1$COMPLETO_EDUCACION_SECUNDARIA)
table(is.na(base1$COMPLETO_EDUCACION_SECUNDARIA)) #15748 vacias
base1$dip_secundaria <- NA
base1$dip_secundaria[base1$COMPLETO_EDUCACION_SECUNDARIA==2 | !is.na(base1$dip_primaria)] <- 0
base1$dip_secundaria[base1$COMPLETO_EDUCACION_SECUNDARIA==1] <- 1 #5284
table(base1$dip_secundaria)
sum(table(base1$dip_secundaria)) #17371

# tecnico superior
table(base1$ESTA_INSCRITO_EN,base1$COMPLETO_PROGRAMA_INSCRITO)
table(is.na(base1$ESTA_INSCRITO_EN),is.na(base1$COMPLETO_PROGRAMA_INSCRITO)) #19559 vacias
base1$dip_tec_sup <- NA
base1$dip_tec_sup[!is.na(base1$dip_primaria)] <- 0
base1$dip_tec_sup[base1$ESTA_INSCRITO_EN==2 & base1$COMPLETO_PROGRAMA_INSCRITO==1] <- 1
base1$dip_tec_sup[base1$ESTA_INSCRITO_EN==3 & base1$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #100
table(base1$dip_tec_sup)
sum(table(base1$dip_tec_sup)) #17369

# carrera universitaria
base1$dip_licenciatura <- NA
base1$dip_licenciatura[!is.na(base1$dip_primaria)] <- 0
base1$dip_licenciatura[base1$ESTA_INSCRITO_EN==1 & base1$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #1293
table(base1$dip_licenciatura)
sum(table(base1$dip_licenciatura)) #17369

#Creación variable capital. 
base1$capital <- 0
base1$capital[base1$EFT_MUNICIPIO_RESIDE>101] <- 0
base1$capital[base1$EFT_MUNICIPIO_RESIDE==101] <- 1

#Creación variable DesEmp
base1$DesEmp <- 0
base1$DesEmp[base1$DESOCUPADO==0] <- 0
base1$DesEmp[base1$DESOCUPADO==1] <- 1

#Desempleo incluyendo personas inactivas.
base1$DesEmpInac <- 0
base1$DesEmpInac[base1$DESOCUPADO==0 | base1$INACTIVO==0] <- 0
base1$DesEmpInac[base1$DESOCUPADO==1 | base1$INACTIVO==1] <- 1

#Histograma
hist(base1$DesEmp)

#Estimación Modelos
Modelo2 <- glm(DesEmp~mujer+casado+capital+educ, data = base1, family = binomial(link = "logit"))
library(stargazer)
stargazer(Modelo2, type="text")

Modelo1 <- glm(DesEmp~mujer+casado+capital+dip_primaria+dip_secundaria+dip_licenciatura+dip_tec_sup, data = base1, family = binomial(link = "logit"))
stargazer(Modelo1, type="text")

Modelo3 <- glm(DesEmp~mujer+casado+capital+dip_primaria+dip_secundaria+dip_licenciatura+dip_tec_sup, data = base1, family = binomial(link = "probit"))
stargazer(Modelo2, type="text")

stargazer(Modelo1, Modelo2, type = "text")

library(margins)
margins(Modelo1)
margins(Modelo2)
margins(Modelo3)


mean(base1$DesEmp)
sd(base1$DesEmp)
mean(base1$mujer)
sd(base1$mujer)
mean(base1$casado)
sd(base1$casado)
mean(base1$capital)
sd(base1$capital)
library(psych)
describe(base1$dip_secundaria)
describe(base1$dip_primaria)
describe(base1$dip_tec_sup)
describe(base1$dip_licenciatura)
describe(base1$educ)

table(base1$capital)
table(base1$DesEmp)
descr <- data.frame(Variable=rep(c("DesEmp", "Mujer", "Casado", "Capital", "Dip_Primaria", "Dip_Secundaria", "Dip_Licenciatura", "Dip_Tec_Sup", "Educ")), Media=rep(c("0.065", "0.494", "0.122", "0.038", "0.525","0.304", "0.074","0.006", "6.830")), Des.Std.=rep(c("0.24", "0.49", "0.33", "0.19", "0.50", "0.46", "0.26", "0.08", "4.87")))
head(descr, n=9)
Desem <- data.frame(Estado=rep(c("Empleado", "Desempleado")), Número=rep(c("23226", "1638")))
head(Desem)
gen <- data.frame(Género=rep(c("Hombre", "Mujer")), Número=rep(c("12563", "12301")))
head(gen)
Cas <- data.frame(Estado=rep(c("Casado", "No casado")), Número=rep(c("3050", "21814")))
head(Cas)
cap <- data.frame(Lugar=rep(c("Distrito Nacional", "Fuera del Distrito Nacional")), Número=rep(c("965", "23899")))
head(cap)
dippri <- data.frame(Estado=rep(c("Graduado de primaria", "No graduado")), Número=rep(c("8255", "9114")))
head(dippri)
dipsec <- data.frame(Estado=rep(c("Graduado de secundaria", "No graduado")), Número=rep(c("5284", "12087")))
head(dipsec)
diplic <- data.frame(Estado=rep(c("Graduado de licenciatura", "No graduado")), Número=rep(c("1293", "16076")))
head(diplic)
diptec <- data.frame(Estado=rep(c("Graduado de técnico superior", "No graduado")), Número=rep(c("100", "17269")))
head(diptec)

hist(base1$educ, main = "Histograma años educativos", xlab = "Años educativos", ylab = "Frecuencia")

mod1 <- data.frame(Variable=rep(c("Mujer", "Casado", "Capital", "Dip_Primaria", "Dip_Secundaria", "Dip_Licenciatura", "Dip_Tec_Sup")), Efecto=rep(c("0.061", "-0.065", "0.019", "0.061", "0.017", "-0.047", "-0.118")))
head(mod1, n=7)
mod2 <- data.frame(Variable=rep(c("Mujer", "Casado", "Capital", "Educ")), Efecto=rep(c("0.040", "-0.050", "0.002", "0.007")))  
head(mod2)
mod3 <- data.frame(Variable=rep(c("Mujer", "Casado", "Capital", "Dip_Primaria", "Dip_Secundaria", "Dip_Licenciatura", "Dip_Tec_Sup")), Efecto=rep(c("0.060", "-0.062", "0.021", "0.059", "0.018", "-0.048", "-0.114")))
head(mod1, n=7)
