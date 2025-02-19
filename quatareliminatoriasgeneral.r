library(readxl)

Ganador<-function(vector1,vector2){
  Puntos1<-0
  Puntos2<-0
  #empatados<-0
  for (i in 1:10000) {
    if (vector1[i]>vector2[i]){
      Puntos1<-Puntos1+1
    } else if (vector1[i]<vector2[i]){
      Puntos2<-Puntos2+1
    } else {
      U<-runif(1)
      if (U<=1/2){Puntos1<-Puntos1+1}else{Puntos2<-Puntos2+1}
    }
  }
c(Puntos1,Puntos2)
}

BinomialNegativa<-function(nsim,mu,deltacua){
  Simulaciones<-vector()
  alpha<-mu^2/(deltacua-mu)
  beta<-mu/(deltacua-mu) 
  for (i in 1:nsim) {
    U<-runif(1)
    n<-0
    p<-(beta/(1+beta))^alpha
    efe<-p
    while(U>=efe){
      p<-(n+alpha)*p/((1+n)*(1+beta))
      efe<-efe+p
      n<-n+1
    }
    Simulaciones[i]<-n
  }
  Simulaciones
}

Partido<-function(MediaLocal,VarianzaLocal,DistLocal,MediaVisitante,VarianzaVisitante,DistVisitante,ValorPLocal){
  #Dice Local y visitante, pero Local nos referiremos al equipo que anotar? gole
  #y visitante es al que le meter?n dichos goles
  Distribucines<-c(DistLocal,DistVisitante) 
  Medias<-c(MediaLocal,MediaVisitante)
  varianzas<-c(VarianzaLocal,VarianzaVisitante)
  goles<-c(0,0)
  for (i in 1:2) {
    if (Distribucines[i]=="P") {
      goles[i]<-rpois(1,Medias[i])
    } else if (Distribucines[i]=="B"){
      goles[i]<-rbinom(1,90,Medias[i]/90)
    } else {
      goles[i]<-BinomialNegativa(1,Medias[i],varianzas[i])
    }
  }
  GolesAnotados<-floor(ValorPLocal*(goles[1]+goles[2]))
  GolesAnotados
}

SimulacionFase2<-function(Datos){
  EloLocal<-0
  EloVisit<-0
  Nombres<-unique(Datos[,1])
  Tamano<-nrow(unique(Datos[,1]))
  ValoresE<-matrix(rep(0,Tamano),nrow = Tamano,ncol = 1)
  for (i in 1:Tamano) {
    ValoresE[i,1]<-as.numeric(Datos[i,14])
  }
  
  #row.names(ValoresE)<-Nombres
  #ValoresE<-Eloinicial(ValoresE,ValoresP)
  N<-"a"
  for (i in 1:Tamano) {
    N[i]<-as.character(Nombres[i,1])
  }
  Nombres<-N
  Puntuacion<-matrix(rep(0,2*Tamano),nrow = Tamano,ncol = 2)
  rownames(Puntuacion)<-Nombres
  rownames(ValoresE)<-Nombres
  colnames(Puntuacion)<-c("Partidos Ganados","Nuevos Rating")
  i<-1
  #Nombres<-unique(Datos[,1])
  #Puntuacion<-matrix(rep(0,nrow(Nombres)*2),nrow = nrow(Nombres),ncol = 2)
  #rownames(Puntuacion)<-c(Nombres[1,1],Nombres[2,1],Nombres[3,1],Nombres[4,1],Nombres[5,1],Nombres[6,1],Nombres[7,1],Nombres[8,1],Nombres[9,1],Nombres[10,1],Nombres[11,1],Nombres[12,1],Nombres[13,1],Nombres[14,1],Nombres[15,1],Nombres[16,1])
  #colnames(Puntuacion)<-c("Probabilidad de Ganar", "Goles Esperados")
  #i<-1
  while (i<= nrow(Datos)) {
    Loc<-as.character(Datos[i,1])
    Vis<-as.character(Datos[i+1,1])
    GFL<-as.numeric(Datos[i,2])
    VGFL<-as.numeric(Datos[i,3])
    DGFL<-as.character(Datos[i,4])
    GCL<-as.numeric(Datos[i,5])
    VGCL<-as.numeric(Datos[i,6])
    DGCL<-as.character(Datos[i,7])
    GFV<-as.numeric(Datos[i+1,8])
    VGFV<-as.numeric(Datos[i+1,9])
    DGFV<-as.character(Datos[i+1,10])
    GCV<-as.numeric(Datos[i+1,11])
    VGCV<-as.numeric(Datos[i+1,12])
    DGCV<-as.character(Datos[i+1,13])
    
    Ent<-ValoresE[which(rownames(ValoresE)==Loc),1]
    Enmt<-ValoresE[which(rownames(ValoresE)==Vis),1]
    eLoc<-0
    eVis<-0
    GolesLoc<-c(rep(0,10000))
    GolesVis<-c(rep(0,10000))
    d<-0
    for (j in 1:10000) {
      p<-CalcularValorP(Ent,Enmt)
      GolesLoc[j]<-Partido(GFL,VGFL,DGFL,GCV,VGCV,DGCV,p)
      GolesVis[j]<-Partido(GFV,VGFV,DGFV,GCL,VGCL,DGCL,1-p)
      d<-GolesLoc[j]-GolesVis[j]
      Nuevos<-ActualizarElo(Ent,Enmt,d)
      #Ent<-Nuevos[1]
      #Enmt<-Nuevos[2]
      eLoc<-eLoc+Nuevos[1]
      eVis<-eVis+Nuevos[2]
    }
    ValoresE[which(rownames(ValoresE)==Loc),1]<-eLoc/10000
    ValoresE[which(rownames(ValoresE)==Vis),1]<-eVis/10000
    
    PunEsperados<-Ganador(GolesLoc,GolesVis)
    PosLoc<-which(rownames(Puntuacion)==Loc)
    PosVis<-which(rownames(Puntuacion)==Vis)
    Puntuacion[PosLoc,1]<-Puntuacion[PosLoc,1]+PunEsperados[1]
    Puntuacion[PosVis,1]<-Puntuacion[PosVis,1]+PunEsperados[2]
    i<-i+2
  }
  for (i in 1:Tamano) {
    Puntuacion[i,2]<-ValoresE[i,1]
  }
  #print(ValoresE)
  #print(Puntuacion)
  i<-1
  Orden<-1
  Ganadorer<-matrix(rep(0,3*Tamano/2),ncol = 3,nrow = Tamano/2)
  while (i<=Tamano) {
    if (Puntuacion[i,1]>Puntuacion[i+1,1]){ 
      Ganadorer[Orden,1]<-Nombres[i]
      Ganadorer[Orden,2]<-Puntuacion[i,1]
      Ganadorer[Orden,3]<-Puntuacion[i,2]
      #print(Ganadorer)
    }else{
      Ganadorer[Orden,1]<-Nombres[i+1]
      Ganadorer[Orden,2]<-Puntuacion[i+1,1]
      Ganadorer[Orden,3]<-Puntuacion[i+1,2]
      #print(Ganadorer)
    }
    Orden<-Orden+1
    i<-i+2
  }
  #print(matrix(c(GolesLoc,GolesVis), nrow = 10000, ncol = 2))
  Ganadorer
}

#octavos Qatar
OctavosQatar <- read_excel("Eric/UNI/Semestre 7/Modelos de sim/SEGUNDA RONDA Qatar.xlsx", sheet = "TABLA UNIDA")
Simulaci?nFase2(OctavosQatar)

#cuartos Qatar
CuartosQatar <- read_excel("Eric/UNI/Semestre 7/Modelos de sim/Rercera Fase Qatar.xlsx", sheet = "Hoja1")
Simulaci?nFase2(CuartosQatar)

#Semi Qatar
SemiQatar <- read_excel("Eric/UNI/Semestre 7/Modelos de sim/Semifinales Qatar.xlsx",sheet = "Hoja1")
Simulaci?nFase2(SemiQatar)

#Final Qatar
FinalQatar <- read_excel("Eric/UNI/Semestre 7/Modelos de sim/Final Qatar.xlsx",sheet = "Hoja1")
Simulaci?nFase2(FinalQatar)


#octavos Russia
Octavos_Final_2018 <- read_excel("Eric/Qatar/SEGUNDA RONDA Rusia.xlsx", sheet = "TABLA UNIDA")
SimulacionFase2(Octavos_Final_2018)

#Cuartos Russia
Cuartos_Final_2018 <- read_excel("Eric/Qatar/Tercera Fase Rusia.xlsx", sheet = "Hoja1")
SimulacionFase2(Cuartos_Final_2018)

#semi Final Russia
semi_Final_2018 <- read_excel("Eric/Qatar/Semifinales Rusia.xlsx", sheet = "Hoja1")
SimulacionFase2(semi_Final_2018)

#Final Russia
Fase_Final_2018 <- read_excel("Eric/Qatar/Final Rusia.xlsx",sheet = "Hoja1")
SimulacionFase2(Fase_Final_2018)
