library(readxl)
IntencidadesPremier <- read_excel("IntencidadesPremier.xlsx")
View(IntencidadesPremier)

Puntos<-function(vector1,vector2){
  Puntos1<-0
  Puntos2<-0
  for (i in 1:10000) {
    if (vector1[i]>vector2[i]){
      Puntos1<-Puntos1+3
    } else if (vector1[i]<vector2[i]){
      Puntos2<-Puntos2+3
    } else {
      Puntos1<-Puntos1+1
      Puntos2<-Puntos2+1
    }
  }
  Puntos1<-Puntos1/10000
  Puntos2<-Puntos2/10000
  PuntosEsperados<-c(Puntos1,Puntos2)
}


simuacion_Por_partido<-function(anotados1Visitatne, contra1Visitante, anotados1Local, contra1Local,anotados2Visitatne, contra2Visitante, anotados2Local, contra2Local){
  goles1Local<-rpois(10000,(anotados1Local+contra2Visitante)/2)
  goles2Visit<-rpois(10000,(anotados2Visitatne+contra1Local)/2)
  goles1Visitante<-rpois(10000,(anotados1Visitatne+contra2Local)/2)
  goles2Local<-rpois(10000,(anotados2Local+contra1Visitante)/2)
  
  c(Puntos(goles1Local,goles2Visit)+Puntos(goles1Visitante,goles2Local))
}

Liga<-function(Intencidades){
  Matriz.Intencidades<-as.matrix(Intencidades)
  Matriz.Puntos<-matrix(rep(0,nrow(Intencidades)^2),nrow = nrow(Intencidades),ncol = nrow(Intencidades))
  rownames(Matriz.Puntos)<-Matriz.Intencidades[,1]
  colnames(Matriz.Puntos)<-Matriz.Intencidades[,1]
  #print(Matriz.Puntos)
  for (i in 1:nrow(Intencidades)) {
    Pivote<-i+1
    while (Pivote<=nrow(Intencidades)) {
     Inti1<-as.numeric(Intencidades[i,2])
     Inti2<-as.numeric(Intencidades[i,3])
     Inti3<-as.numeric(Intencidades[i,4])
     Inti4<-as.numeric(Intencidades[i,5])
     IntPiv1<-as.numeric(Intencidades[Pivote,2])
     IntPiv2<-as.numeric(Intencidades[Pivote,3])
     IntPiv3<-as.numeric(Intencidades[Pivote,4])
     IntPiv4<-as.numeric(Intencidades[Pivote,5])
      Pts<-simuacion_Por_partido(Inti1,Inti2,Inti3,Inti4,IntPiv1,IntPiv2,IntPiv3,IntPiv4)
      Matriz.Puntos[i,Pivote]<-Pts[1]
      Matriz.Puntos[Pivote,i]<-Pts[2]
     Pivote<-Pivote+1
    }
  }
  print(Matriz.Puntos)
  PuntosFinales<-matrix(rep(0,nrow(Intencidades)),nrow = 1,ncol = nrow(Intencidades))
  colnames(PuntosFinales)<-Matriz.Intencidades[,1]
  for (i in 1:nrow(Intencidades)) {
    PuntosFinales[1,i]<-sum(Matriz.Puntos[i,])
  }
  print(PuntosFinales)
  Podio<-order(PuntosFinales[1,],decreasing = TRUE)
  PuntosFinales<-as.matrix(PuntosFinales[Podio])
  #print(PuntosFinales)
  #print(Podio)
  nombres<-Matriz.Intencidades[,1]
  #print(nombres[Podio])
  rownames(PuntosFinales)<-c(nombres[Podio])
  colnames(PuntosFinales)<-c("Puntos")
  PuntosFinales
}

Liga(IntencidadesPremier)

simuacion_Por_partido(2.157894737,0.578947368, 3.052631579,0.789473684,2.368421053, 0.894736842, 2.578947368, 0.473684211)