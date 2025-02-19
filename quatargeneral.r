library(readxl)

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

SimulacionFase1<-function(Datos){
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
  colnames(Puntuacion)<-c("Puntos","Nuevos Rating")
  i<-1
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
    
    PunEsperados<-Puntos(GolesLoc,GolesVis)
    PosLoc<-which(rownames(Puntuacion)==Loc)
    PosVis<-which(rownames(Puntuacion)==Vis)
    Puntuacion[PosLoc,1]<-Puntuacion[PosLoc,1]+PunEsperados[1]
    Puntuacion[PosVis,1]<-Puntuacion[PosVis,1]+PunEsperados[2]
    i<-i+2
  }
  for (i in 1:Tamano) {
    Puntuacion[i,2]<-ValoresE[i,1]
  }
  print(ValoresE)
  Puntuacion
}

#Quatar
MediayVarianza_2022GrupoA <- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx",sheet = "A")
MediayVarianza_2022GrupoB<- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx", sheet = "B")
MediayVarianza_2022GrupoC <- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx", sheet = "C")
MediayVarianza_2022GrupoD <- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx", sheet = "D")
MediayVarianza_2022GrupoE <- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx", sheet = "E")
MediayVarianza_2022GrupoF <- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx", sheet = "F")
MediayVarianza_2022GrupoG <- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx", sheet = "G")
MediayVarianza_2022GrupoH <- read_excel("Eric/Qatar/Primera ronda Qatar 2022.xlsx", sheet = "H")

SimulacionFase1(MediayVarianza_2022GrupoA)
SimulacionFase1(MediayVarianza_2022GrupoB)
SimulacionFase1(MediayVarianza_2022GrupoC)
SimulacionFase1(MediayVarianza_2022GrupoD)
SimulacionFase1(MediayVarianza_2022GrupoE)
SimulacionFase1(MediayVarianza_2022GrupoF)
SimulacionFase1(MediayVarianza_2022GrupoG)
SimulacionFase1(MediayVarianza_2022GrupoH)

#Russia

MediayVarianza_2018A <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "A")
MediayVarianza_2018B <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "B")
MediayVarianza_2018C <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "C")
MediayVarianza_2018D <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "D")
MediayVarianza_2018E <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "E")
MediayVarianza_2018F <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "F")
MediayVarianza_2018G <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "G")
MediayVarianza_2018H <- read_excel("Eric/Qatar/Primera ronda Rusia 2022.xlsx", sheet = "H")

SimulacionFase1(MediayVarianza_2018A)
SimulacionFase1(MediayVarianza_2018B)
SimulacionFase1(MediayVarianza_2018C)
SimulacionFase1(MediayVarianza_2018D)
SimulacionFase1(MediayVarianza_2018E)
SimulacionFase1(MediayVarianza_2018F)
SimulacionFase1(MediayVarianza_2018G)
SimulacionFase1(MediayVarianza_2018H)

