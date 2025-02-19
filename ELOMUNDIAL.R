library(dplyr)
library(EloRating)
library(elo)
library(ggplot2)

datosss =  read.csv("dirección.csv")

head(datosss)

#Creamos nuevas columnas para cada caso ganador
datosss$winner = case_when(datosss$RTC == 'L'~ as.character(datosss$LOCAL),
                           datosss$RTC == 'V'~ as.character(datosss$VISITANTE),
                           datosss$RTC == 'E'~ as.character(datosss$LOCAL))

datosss$loser = case_when(datosss$RTC == 'V'~ as.character(datosss$LOCAL),
                          datosss$RTC == 'L'~ as.character(datosss$VISITANTE),
                          datosss$RTC == 'E'~ as.character(datosss$VISITANTE))


datosss$draw = case_when(datosss$RTC == 'E'~ TRUE,
                         datosss$RTC != 'E'~ FALSE )

head(datosss[,c('winner', 'loser', 'draw')])


datosss$FECHA <- as.Date(datosss$FECHA,"%d/%m/%Y") #Cambiar formato de FECHA
datosss$FECHA <- as.character(datosss$FECHA)
substr(datosss$FECHA, 1, 5) <- "20"
datosss$FECHA <- as.Date(datosss$FECHA)
head(datosss[,1:5])

#Aplicar elo.seq
resul_elo <- elo.seq(winner = datosss$winner,loser = datosss$loser, draw = datosss$draw, Date = datosss$FECHA, startvalue = 1500, k=20, 
                     progressbar =  FALSE, runcheck =  FALSE )


elo_final <- resul_elo$mat #matriz
elo_final<- as.data.frame(elo_final) #pasarlo a data frame
head(elo_final[1:6])

Dates <- resul_elo$truedates
elo_final$Dates <- Dates #FECHAs modificadas

final_elo <- as.data.frame(extract_elo(resul_elo))
equipos<- rownames(final_elo) #fila para cada equipo
final_elo$Equipo <- equipos #equipos se almacenan 
rownames(final_elo)<- NULL

colnames(final_elo) <- c("Elo","Posición") #nombramos las columnas
head(final_elo,26)

AFC <- subset(final_elo$Elo, equipos == "AFC")
CAF <- subset(final_elo$Elo, equipos == "CAF")
elo.prob(AFC,CAF)

dev.off()
eloplot(resul_elo, ids = "first.2")#plot

write.csv(final_elo, file = "ELO 20202021.csv")