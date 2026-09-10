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

W[,13]

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













