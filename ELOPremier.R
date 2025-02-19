library(dplyr)
library(EloRating)
library(elo)
library(ggplot2)

datosss=  read.csv("dirección.csv")
head(datosss)

#Creamos nuevas columnas para cada caso ganador
datosss$winner = case_when(datosss$RTC == 'L'~ as.character(datosss$Local),
                            datosss$RTC == 'V'~ as.character(datosss$Visitante),
                            datosss$RTC == 'E'~ as.character(datosss$Local))

datosss$loser = case_when(datosss$RTC == 'V'~ as.character(datosss$Local),
                            datosss$RTC == 'L'~ as.character(datosss$Visitante),
                            datosss$RTC == 'E'~ as.character(datosss$Visitante))


datosss$draw = case_when(datosss$RTC == 'E'~ TRUE,
                           datosss$RTC != 'E'~ FALSE )

head(datosss[,c('winner', 'loser', 'draw')])


datosss$Fecha <- as.Date(datosss$Fecha,"%d/%m/%Y") #Cambiar formato de fecha
datosss$Fecha <- as.character(datosss$Fecha)
substr(datosss$Fecha, 1, 2) <- "20"
datosss$Fecha <- as.Date(datosss$Fecha)
head(datosss[,1:17])

#Aplicar elo.seq
resul_elo <- elo.seq(winner = datosss$winner,loser = datosss$loser, draw = datosss$draw, Date = datosss$Fecha, startvalue = 1500, k=30, 
                     progressbar =  FALSE, runcheck =  FALSE )


elo_final <- resul_elo$mat #matriz
elo_final<- as.data.frame(elo_final) #pasarlo a data frame
head(elo_final[1:5])

Dates <- resul_elo$truedates
elo_final$Dates <- Dates #fechas modificadas

final_elo <- as.data.frame(extract_elo(resul_elo))
equipos<- rownames(final_elo) #fila para cada equipo
final_elo$Equipo <- equipos #equipos se almacenan 
rownames(final_elo)<- NULL

colnames(final_elo) <- c("Elo","Posición") #nombramos las columnas
head(final_elo,20)

Burnley <- subset(final_elo$Elo, equipos == "Burnley")
Arsenal <- subset(final_elo$Elo, equipos == "Arsenal")
elo.prob(Burnley, Arsenal)


dev.off()
eloplot(resul_elo, ids = "first.20")#plot

write.csv(final_elo, file = "ELO 20202021.csv")

