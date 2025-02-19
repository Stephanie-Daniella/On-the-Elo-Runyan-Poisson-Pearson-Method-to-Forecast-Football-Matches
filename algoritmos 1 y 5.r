Fun1<-function(Ent,Enmt,W,G){
  K<-60
  NuevoEnt<-Ent+K*G*(W-(1+10^((Enmt-Ent)/400))^(-1))
  NuevoEnt
}

ActualizarElo<-function(Ent,Enmt,dif){
  
  if (dif==0){
    G<-1
    W<-.5
    NuevoEnt<-Fun1(Ent,Enmt,W,G)
    NuevoEnmt<-Fun1(Enmt,Ent,W,G)
  } else if (abs(dif)==1){
    G<-1
    if(dif>0){
    W<-1
    NuevoEnt<-Fun1(Ent,Enmt,W,G)
    W<-0
    NuevoEnmt<-Fun1(Enmt,Ent,W,G) 
    } else {
      W<-0
      NuevoEnt<-Fun1(Ent,Enmt,W,G)
      W<-1
      NuevoEnmt<-Fun1(Enmt,Ent,W,G)
    }
  } else if (abs(dif)==2){
    G<-1.5
    if(dif>0){
      W<-1
      NuevoEnt<-Fun1(Ent,Enmt,W,G)
      W<-0
      NuevoEnmt<-Fun1(Enmt,Ent,W,G)
    } else {
      W<-0
      NuevoEnt<-Fun1(Ent,Enmt,W,G)
      W<-1
      NuevoEnmt<-Fun1(Enmt,Ent,W,G)
    }
  } else {
    G<-(11+abs(dif))/8
    if(dif>0){
      W<-1
      NuevoEnt<-Fun1(Ent,Enmt,W,G)
      W<-0
      NuevoEnmt<-Fun1(Enmt,Ent,W,G)
    } else {
      W<-0
      NuevoEnt<-Fun1(Ent,Enmt,W,G)
      W<-1
      NuevoEnmt<-Fun1(Enmt,Ent,W,G)
  }
    }
  return(c(NuevoEnt,NuevoEnmt))
}

CalcularValorP<-function(Ent,Enmt){
  p<-(1+10^((Enmt-Ent)/400))^(-1)
  p
}

Eloinicial<-function(A,Datos){
  for (i in 1:nrow(Datos)) {
    L<-which(rownames(A)==Datos[i,1])
    A[L,1]<-as.numeric(Datos[i,2])
  }
  A
}
