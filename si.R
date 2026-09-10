library(readxl)
library(seasonal)
library(forecast)
library(TSA)
library(portes)


setwd("~/marco/man/emim/series/nuevo")

mes<-substr(seq(as.Date('2018-01-01'),as.Date('2026-06-01'),by='month'),1,7)
length(mes)

inpc<-as.matrix(read_excel('inpc.xlsx'))
meses2<-substr(seq(as.Date('1969-01-01'),as.Date('2026-07-01'),by='month'),1,7)
row.names(inpc)<-meses2
tail(inpc,10)

defl<-(inpc)/(mean(inpc[589:(589+11),]))*(100)
dfl<-defl[589:(dim(defl)-1)[1],]
length(dfl)


abss <- as.matrix(read_excel("absolutos.xlsx",col_names = TRUE))[,01:32]
abss <- (as.matrix(read_excel("absolutos.xlsx",col_names = TRUE))[,33:((33+32)-1)])/(dfl)

#(as.matrix(read_excel("absolutos.xlsx",col_names = TRUE))[,33:((33+32)-1)])[1,]/(dfl[1])
#abss[1,]
#36470.18
#27980.4
#1546.285 

dom <- as.matrix(read_excel("dom.xlsx",col_names = TRUE))[c(145:159,180:196),]
W   <- as.matrix(read_excel("W.xlsx",col_names = TRUE))[c(145:159,180:196),]
I   <- as.matrix(read_excel("I.xlsx",col_names = TRUE))[133:(133+(length(mes)-1)),(33:(33+32-1))]


row.names(abss)<-mes
row.names(W)<-dom
row.names(I)<-mes
length(dom)

View(abss)
View(W)
View(dom)
View(I)

dim(abss)
length(dom)
dim(W)
dim(I)

#====================
#3261
#====================

y<-"3261"
x<-c("326110","326120","326140","326150","326160","326191","326192","326193","326194")

Y<-matrix(c(rep(NA,(length(x)*length(mes)))),nrow = length(mes),byrow = TRUE)
dim(Y)

k<-1
l<-c(rep(NA,length(x)))
for (k in 1:length(x)) {
  cat('intera:',k,'\n')
  ij<-as.matrix(round(((abss[,which(dom==x[k])[1]]/mean(abss[1:12,which(dom==x[k])[1]]))*100),22))
  Y[,k]<-ij
  l[k]=which(dom==x[k])[1]
}

table(round(Y,1)==I[,l])
dim(Y)

m<-1
n<-1
Z<-as.matrix(c(rep(0,length(mes))))
dim(Z)

for (m in 1:dim(Y)[2]) {
  cat('intera:',m,'\n')
  ij<-as.matrix(round(Y[,m],1)*((W[which(row.names(W)==x[m])[1],n]))/100)
    
  Z[,1]<-Z+ij
}

table(round(Z,1)==I[,which(dom==y)])
table(round(Z,1)-I[,which(dom==y)])
sum(abs(round(Z,1)-I[,which(dom==y)]))


#====================
#3262
#====================

y<-"3262"
x<-c("326211","326220","326290")

Y<-matrix(c(rep(NA,(length(x)*length(mes)))),nrow = length(mes),byrow = TRUE)
dim(Y)

k<-1
l<-c(rep(NA,length(x)))
for (k in 1:length(x)) {
  cat('intera:',k,'\n')
  ij<-as.matrix(round(((abss[,which(dom==x[k])[1]]/mean(abss[1:12,which(dom==x[k])[1]]))*100),22))
  Y[,k]<-ij
  l[k]=which(dom==x[k])[1]
}

table(round(Y,1)==I[,l])
dim(Y)

m<-1
n<-1
Z<-as.matrix(c(rep(0,length(mes))))
dim(Z)

for (m in 1:dim(Y)[2]) {
  cat('intera:',m,'\n')
  ij<-as.matrix(round(Y[,m],1)*((W[which(row.names(W)==x[m])[1],n]))/100)
  
  Z[,1]<-Z+ij
}


table(round(Z,1)==I[,which(dom==y)])
table(round(Z,1)-I[,which(dom==y)])
sum(abs(round(Z,1)-I[,which(dom==y)]))


#====================
#326
#====================

z<-c("326")
y<-list(c("3261"),c("3262"))
x<-list(c("326110","326120","326140","326150","326160","326191","326192","326193","326194"),
        c("326211","326220","326290"))
a<-13

X<-matrix(rep(NA,(length(mes)*length(unlist(x)))),ncol = length(unlist(x)))
Y<-matrix(rep(0,(length(mes)*length(unlist(y)))),ncol = length(unlist(y)))
Z<-matrix(rep(0,(length(mes)*length((z)))),ncol = length((z)))

l<-c(rep(NA,length(unlist(x))))
for (k in 1:length(unlist(x))) {
  cat('intera:',k,'\n')
  ij<-as.matrix(round(((abss[,which(dom==unlist(x)[k])[1]]/mean(abss[1:12,which(dom==unlist(x)[k])[1]]))*100),22))
  X[,k]<-ij
  l[k]=which(dom==unlist(x)[k])[1]
}

table(round(X,1)==I[,l])
dim(X)

counter<-c(0,sapply(x, length))
#m<-1
#n<-1

o<-c(rep(NA,length(unlist(y))))

for (m in 1:length(y)) {
  cat('itera m:',(m),'\n')
  
  for (n in 1:length(x[[m]])) {
    cat('itera n:',(n+sum(counter[1:m])),'\n')
    
    ij<-as.matrix(round(X[,(n+sum(counter[1:m]))],1))*(W[which(row.names(W)==unlist(x)[(n+sum(counter[1:m]))]),a][1]/100)
    Y[,m]<-Y[,m]+ij
    
    print(Y)
  }
  
  o[m]<-which(dom==unlist(y)[m])
  
  sij<-round(Y[,m],1)*((W[which(row.names(W)==unlist(y)[m]),a])/(100))
  Z[,1]=Z[,1]+sij
  
  
}

table(round(X,1)==I[,l])
table(round(Y,1)==I[,o])
table(round(Z,1)==I[,which(dom==z[1])])

table(round(X,1)-I[,l])
table(round(Y,1)-I[,o])
table(round(Z,1)-I[,which(dom==z[1])])


#====================
#331
#====================

z<-c("331")
y<-list(c("3311"),c("3312"),c("3313"),c("3314"),c("3315"))
x<-list(c("331111","331112"),
        c("331210","331220"),
        c("331310"),
        c("331411","331412","331419","331420"),
        c("331510","331520"))
a<-13

X<-matrix(rep(NA,(length(mes)*length(unlist(x)))),ncol = length(unlist(x)))
Y<-matrix(rep(0,(length(mes)*length(unlist(y)))),ncol = length(unlist(y)))
Z<-matrix(rep(0,(length(mes)*length((z)))),ncol = length((z)))

list(dim(X),dim(Y),dim(Z))

l<-c(rep(NA,length(unlist(x))))
for (k in 1:length(unlist(x))) {
  cat('intera:',k,'\n')
  ij<-as.matrix(round(((abss[,which(dom==unlist(x)[k])[1]]/mean(abss[1:12,which(dom==unlist(x)[k])[1]]))*100),22))
  X[,k]<-ij
  l[k]=which(dom==unlist(x)[k])[1]
}

table(round(X,1)==I[,l])
dim(X)

counter<-c(0,sapply(x, length))
#m<-1
#n<-1

o<-c(rep(NA,length(unlist(y))))

for (m in 1:length(y)) {
  cat('itera m:',(m),'\n')
  
  for (n in 1:length(x[[m]])) {
    cat('itera n:',(n+sum(counter[1:m])),'\n')
    
    ij<-as.matrix(round(X[,(n+sum(counter[1:m]))],1))*(W[which(row.names(W)==unlist(x)[(n+sum(counter[1:m]))]),a][1]/100)
    Y[,m]<-Y[,m]+ij
  }
  
  o[m]<-which(dom==unlist(y)[m])
  
  sij<-round(Y[,m],1)*((W[which(row.names(W)==unlist(y)[m]),a])/(100))
  Z[,1]=Z[,1]+sij
  
  
}

table(round(X,1)==I[,l])
table(round(Y,1)==I[,o])
table(round(Z,1)==I[,which(dom==z[1])])

#====================
#SARIMAX
#====================

ts331 <- ts(data=round(cbind(Z,Y),1),start = c(2018,1),frequency = 12)
ts.plot(ts331,col=c('black','blue','red','green','orange','yellow'))

Serie<-ts331[,2]
modela1 <- auto.arima(Serie, stepwise = FALSE)
foreca1  <- forecast(modela1, h = 3)[[4]][1:3]

Serie<-ts331[,3]
modela2 <- auto.arima(Serie, stepwise = FALSE)
foreca2  <- forecast(modela2, h = 3)[[4]][1:3]

tsnueva<-ts(cbind(c(ts331[,1],c(rep(NA,3))),c(ts331[,2],foreca1),c(ts331[,3],foreca2)),start = c(2018,1),frequency = 12)

ts.plot(tsnueva,col=c('black','blue','red'))

Serie<-tsnueva[,1]
SerieX<-cbind(tsnueva[,2],tsnueva[,3])
modelaX <- auto.arima(Serie, stepwise = FALSE, xreg = SerieX)

forecaX  <- forecast(modelaX, xreg = SerieX,h = 3)[[4]][1:3]

tsnuevaX<- ts(cbind(c(ts331[,1],forecaX),SerieX),start = c(2018,1),frequency = 12)

ts.plot(tsnuevaX,col=c('black','blue','red'))
abline(v = 2026 + (7-1)/12, col = "green")

print(ccf(ts331[,1],ts331[,2]))

print(cor.test(ts331[,1],ts331[,3]))


#====================
#ESTIMA
#====================

library(survey)
library(foreign)

#setwd("~/ecovid/bd")
setwd("~/prueba/ajuste/ecis")

ecoie2 <- read.dbf("ECIS2024.dbf")
#ecoie2 <- read.dbf("ecoR2.dbf")
names(ecoie2)
View(ecoie2)

#================
#ECOVIDIE-R2
#================

ecoie2$nh<-ifelse(ecoie2$TAM_EMPRES==1,table(ecoie2$TAM_EMPRES)[[1]],
           ifelse(ecoie2$TAM_EMPRES==2,table(ecoie2$TAM_EMPRES)[[2]],
           ifelse(ecoie2$TAM_EMPRES==3,table(ecoie2$TAM_EMPRES)[[3]],
           ifelse(ecoie2$TAM_EMPRES==4,table(ecoie2$TAM_EMPRES)[[4]],NA))))

table(ecoie2$nh)


ecoie2$Nh<-ifelse(ecoie2$TAM_EMPRES==1,tapply(ecoie2$FAC_EXPA, ecoie2$TAM_EMPRES, sum)[[1]],
           ifelse(ecoie2$TAM_EMPRES==2,tapply(ecoie2$FAC_EXPA, ecoie2$TAM_EMPRES, sum)[[2]],
           ifelse(ecoie2$TAM_EMPRES==3,tapply(ecoie2$FAC_EXPA, ecoie2$TAM_EMPRES, sum)[[3]],
           ifelse(ecoie2$TAM_EMPRES==4,tapply(ecoie2$FAC_EXPA, ecoie2$TAM_EMPRES, sum)[[4]],NA))))

sum(table(ecoie2$nh))

sum(ecoie2$FAC_EXPA)

#=========================================================================
#CATALOGOS
#=========================================================================

tamano<-c(levels(ecoie2$TAM_EMPRES))


dis<-svydesign(data = ecoie2,
               strata = ecoie2$TAM_EMPRES,
               ids = ~(ecoie2$CONSEC),
               weights = ~(ecoie2$FAC_EXPA),
               fpc = ~(I(nh/Nh)),
               nest = FALSE)

alpha<-0.05

estima<-svytotal(x = ifelse(!ecoie2$FAC_EXPA%in%0,1,0),
                 design = dis,
                 FUN=svytotal,
                 level=1-(alpha/2),
                 na.rm = TRUE)


estimator<-estima[1]
var<-vcov(estima)
error<-sqrt(vcov(estima))
cv<-(error/estimator)*100
ls<-estimator+(qnorm(p = 1-(alpha/2),mean = 1,sd = 0))*(error)
li<-estimator-(qnorm(p = 1-(alpha/2),mean = 1,sd = 0))*(error)

cat('Estimacion (li,total,la: (',li,',',estimator,',',ls,')')

estima<-cbind('Nacional',as.data.frame(matrix(data = c(estimator,var,error,cv,li,ls),nrow = 1)))
dim(estima)
colnames(estima)<-c('Nivel','Abs','Var','Error','CV','LI','LS')

graf1 <- ggplot(data=estima, 
                aes(x=Nivel, 
                    y=Abs),
                Position=Abs) + 
  geom_bar(stat="identity",colour = "black")+
  geom_point()+ 
  geom_errorbar(aes(ymin=LI, ymax=LS), width=0.2) +
  xlab(c('2022')) + 
  ylab('empresas') +
  ggtitle('Gráfica de estimación') +
  theme_bw()

print(graf1)

#====================
#IMPUTACION
#====================

# 1. Instalar y cargar la librería
install.packages("VIM")
library(VIM)

# 2. Crear un dataset de ejemplo con datos faltantes (NA)
datos <- data.frame(
  Edad = c(23, 25, NA, 45, 42, 22),
  Ingresos = c(2100, 2300, 2200, 4600, NA, 1900),
  Hijos = c(0, 0, 0, 2, 3, 0)
)

# 3. Aplicar imputación kNN (por defecto usa k = 5 vecinos)
datos_imputados <- kNN(datos, k = 1)


datos_imputados[1]


# Nota: VIM crea columnas sobrantes llamadas "_imp" que indican qué se imputó. 
# Puedes borrarlas quedándote solo con las columnas originales:
datos_final <- datos_imputados[, 1:ncol(datos)]
print(datos_final)


# 1. Instalar y cargar el paquete si no lo tienes
if(!require(mice)) install.packages("mice")
library(mice)

# 2. Crear un conjunto de datos de ejemplo con valores faltantes (NA)
set.seed(123)
datos <- data.frame(
  Edad = c(23, 25, NA, 45, 38, NA, 50, 21),
  Ingresos = c(1500, NA, 1800, 3500, 2800, 1200, NA, 1400),
  Hijos = c(0, 1, 0, 2, 1, 0, 3, 0)
)

# 3. Aplicar imputación kNN (por defecto usa k = 5 vecinos)
datos_imputados <- kNN(datos, k = 3)


# 3. Aplicar la imputación por Vecino Más Cercano (PMM)
# m = 5 indica que creará 5 bases de datos con diferentes imputaciones posibles
datosimputados <- mice(datos, m = 3, method = "pmm", seed = 500)

datosimputados$m

# 4. Extraer uno de los conjuntos de datos completamente lleno (ej. el primero)
datos_finales <- complete(datosimputados, 1)

# 5. Ver el resultado
print(datos_finales)


datos_final <- datos_imputados[, 1:ncol(datos)]
print(datos_final)



# 1. Instalar y cargar paquetes del ecosistema tidymodels
install.packages("recipes")
library(recipes)

# 2. Definir la receta de imputación
receta <- recipe(~ ., data = datos) %>%
  step_impute_knn(all_predictors(), neighbors = 3)

# 3. Entrenar y aplicar la receta
receta_entrenada <- prep(receta, training = datos)
receta_entrenada$requirements
data_imputada_ml <- bake(receta_entrenada, new_data = datos)

# 4. Ver el resultado
print(data_imputada_ml)


# 1. Cargar librerías necesarias
library(VIM)
library(cluster)
library(dplyr)

# Crear un dataset de ejemplo con datos faltantes (NA)
set.seed(123)
datos <- data.frame(
  id = 1:6,
  Edad = c(25, 47, NA, 52, 22, 40),
  Ingresos = c(2200, 4100, 3900, NA, 1900, 3000)
)

print("Datos Originales:")
print(datos)

# 2. Identificar qué filas están completas (potenciales donadores) y cuáles incompletas
filas_incompletas <- que_filas_tienen_na <- los_que_tienen_na <- la_gente_con_na <- que_tienen_na_linea <- which(!complete.cases(datos[, c("Edad", "Ingresos")]))
filas_completas <- que_filas_estan_completas <- which(complete.cases(datos[, c("Edad", "Ingresos")]))

# 3. Calcular la matriz de distancias (Gower maneja numéricos y categóricos perfectamente)
# Excluimos la columna 'id' para no sesgar la distancia
dist_matrix <- as.matrix(daisy(datos[, c("Edad", "Ingresos")], metric = "gower"))

# 4. Extraer los 'K' donadores (ejemplo con K = 2)
k_vecinos <- 1
lista_donadores <- list()

for (i in filas_incompletas) {
  # Tomamos las distancias de la fila incompleta 'i' hacia las filas que SÍ están completas
  distancias_a_completas <- dist_matrix[i, filas_completas]
  
  # Ordenamos para obtener los índices de los vecinos más cercanos
  vecinos_cercanos_indices <- filas_completas[order(distancias_a_completas)[1:k_vecinos]]
  
  # Guardamos los ID reales de los donadores
  lista_donadores[[as.character(i)]] <- datos$id[vecinos_cercanos_indices]
}

# 5. Mostrar los resultados de los donadores mapeados
cat("\nMapeo de Donadores para filas incompletas:\n")
for (fila in names(lista_donadores)) {
  cat(paste("Fila con NA (ID:", datos$id[as.numeric(fila)], ") -> Sus", k_vecinos, "donadores son IDs:", paste(lista_donadores[[fila]], collapse = ", "), "\n"))
}

# 6. Realizar la imputación real con VIM para comparar resultados
datos_imputados <- kNN(datos, variable = c("Edad", "Ingresos"), k = k_vecinos, imp_var = FALSE)
cat("\nDataset Imputado Final:\n")
print(datos_imputados)


























# Tabla de datos
datos = matrix(c(3,4,5,2,3,1,4,6,2,6,3,4,5,NA,3,
                 1,4,1,3,5,4,2,6,3,4,1,NA,3,6,4),nrow=15,ncol=2)
# Dividimos en 2 conjuntos
# Datos completos y datos incompletos
x = as.matrix(datos)
N = dim(x)
p = N[2]
N = N[1]
nas = is.na(drop(x %*% rep(1, p)))
xcomplete = x[!nas, ]
xbad = x[nas, , drop = FALSE]


# Paso 1: Calculamos las distancias
missing = c()
for(i in seq(nrow(xbad))){
  missing[i] = sum(is.na(xbad[i, ]))
}
missingorder = order(missing)
xnas = is.na(xbad)
xbadhat = xbad
cat(nrow(xbad), fill = TRUE)
j = order(missingorder[1])
xinas = xnas[missingorder[1], ]
xd = as.matrix(scale(xcomplete, xbad[missingorder[1],],
                     FALSE)[, !xinas])
dd = drop(xd^2 %*% rep(1, ncol(xd)))

# Paso 2: Usamos un promedio de k vecinos mas cercanos con k=10
K = 10
od = order(dd)[seq(K)]
od = od[!is.na(od)]
K = length(od)
distance = dd[od]

# Paso 3: Asignaciones y calculos finales
s = sum(1/(distance + 1e-15))
weight = (1/(distance + 1e-15))/s
xbadhat[missingorder[1], ]= drop(weight %*% xcomplete[od, xinas, drop = FALSE])
xbadhat[missingorder[1], ]



ts.plot(ts331[,2],col=c('black','blue','red','green','orange','yellow'))

ts.plot(cbind(tsf, tsfa,tsfm1, tsfm2), 
        col = c(1,"#080BFF","#08989C","#AD0A2D"))
abline(v = 2025 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2024 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2022 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2021 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2020 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2019 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2023 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2018 + (9 - 1)/12, col = "red", lwd = 1, lty = 2)
abline(v = 2018 + (8 - 1)/12, col = "green", lwd = 2, lty = 2)
legend(x = 2004,y = 180000,   legend = c("Real", "Óptimo", "Manual 1", "Manual 2"), 
       title= "", lty = 1.0, lwd = 1.0, horiz = FALSE, cex = 0.8,
       bty = "n", seg.len = 0.2,yjust = 0.00,x.intersp = 0.1,y.intersp = 2.0, 
       title.adj = 0.195,pt.cex = 0.5,
       pt.lwd = 0.5,angle = 90,
       xjust = 0.20,
       adj = c(0, NA),
       col = c(1,"#080BFF","#08989C","#AD0A2D")) #, pch=1


#====================



















#====================


for (m in 1:dim(X)[2]) {
  cat('intera:',m,'\n')
  ij<-as.matrix(round(X[,(m+n[m])],1)*((W[which(row.names(W)==x[(m+n[m])])[1],n]))/100)
  
  Yi[,1]<-Yi+ij
}




for (m in 1:dim(X)[2]) {
  cat('intera:',m,'\n')
  ij<-as.matrix(round(X[,(m+n[m])],1)*((W[which(row.names(W)==x[(m+n[m])])[1],n]))/100)
  
  Yi[,1]<-Yi+ij
}










for (i in 1:length(x)) {
  cat('intera en ramas',i,'\n')

  k<-1
  l<-c(rep(NA,length(unlist(x))))
  for (k in 1:length(unlist(x))) {
    cat('intera:',k,'\n')
    ij<-as.matrix(round(((abss[,which(dom==unlist(x)[k])[1]]/mean(abss[1:12,which(dom==unlist(x)[k])[1]]))*100),22))
    Y[,k]<-ij
    l[k]=which(dom==unlist(x)[k])[1]
  }
  
  
  
}







k<-1
l<-c(rep(NA,length(x[[a]])))
for (k in 1:length(x[[a]])) {
  cat('intera:',k,'\n')
  ij<-as.matrix(round(((abss[,which(dom==x[[a]][k])[1]]/mean(abss[1:12,which(dom==x[[a]][k])[1]]))*100),22))
  Y[,k]<-ij
  l[k]=which(dom==x[[a]][k])[1]
}

table(round(Y,1)==I[,l])
dim(Y)

m<-1
n<-1
Z<-as.matrix(c(rep(0,length(mes))))
dim(Z)

for (m in 1:dim(Y)[2]) {
  cat('intera:',m,'\n')
  ij<-as.matrix(round(Y[,m],1)*((W[which(row.names(W)==x[m])[1],n]))/100)
  
  Z[,1]<-Z+ij
}




table(round(Z,1)==I[,which(dom==y)])
table(round(Z,1)-I[,which(dom==y)])
sum(abs(round(Z,1)-I[,which(dom==y)]))






ij<-as.matrix(round(((abss[,which(dom==x)[1]]/mean(abss[1:12,which(dom==x)[1]]))*100),1))













