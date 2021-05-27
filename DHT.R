#datos hidrologicos ejercicio explorativo 

inp <- read.csv("FDC.csv", na.strings = "")

inp <- read.csv("FDC.csv")

head(inp)
dim(inp)

inp[!complete.cases(inp),]

#newinp <- na.omit

plot(inp[,2], type = "l", col="blue")
lines(inp[,3], col="green")

summary(inp[,2:3])
hist(inp[,2])
hist(inp[,3])

names(inp) <- c("fecha", "Estrella", "Banano")
attach(inp)
plot(Estrella)

tempdate <- strptime(inp[,1], format= "%d/%m/%Y")

MAQ_Estrella <- tapply(Estrella, format(tempdate, format= "%Y"), FUN=sum)
MAQ_Banano <- tapply(Banano, format(tempdate, format= "%Y"), FUN=sum)

write.csv(rbind(MAQ_Estrella, MAQ_Banano), file="MAQ.csv")

plot(MAQ_Banano, ylim=c(100,3000))
lines(MAQ_Estrella, col=2)

MMQ_Estrella <- tapply(Estrella, format(tempdate, format= "%m"), FUN=sum)
MMQ_Banano <- tapply(Banano, format(tempdate, format= "%m"), FUN=sum)

#Analisis de correlacion

corinp <- cor (inp[,2:3] , method= "spearman")

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)
summary(inp.lm)

plot(inp.lm)




plot(inp[,2], type = "l", col="blue", xlab = 'fechas', ylab = 'volumeno de agua x tiempo')
lines(inp[,3], col="green")

hist(inp[,2], col="green",main= 'Rio Estrella', xlab= 'clase', ylab = 'R.Estrella')
hist(inp[,3], col="blue",main= 'Rio Banano', xlab= 'clase', ylab = 'R.Banano')