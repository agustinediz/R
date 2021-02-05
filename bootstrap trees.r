
dat <- read.table("ulmifolia.txt", header = T, stringsAsFactors = T)
dat <- dat[, 6:7]
str(dat)
robs <- cor(dat$ramas, dat$semillas)

##No asumiendo Normalidad:

##función
zapa <- function(datos, indices){
  newdata <- datos[indices,]
  corr <- cor(newdata)
  return(corr)
}

##bootstrap
library(boot)
boot1 <- boot(dat, zapa, 9999)
boot1
summary(boot1)

##histograma
hist(boot1$t[,2])
abline(v = robs, col = "red")

##intervalos de confianza
int90 <- boot.ci(boot1, conf = 0.90, type = "bca", index = 2)
int90
int95 <- boot.ci(boot1, conf = 0.95, type = "bca", index = 2)
int95
int99 <- boot.ci(boot1, conf = 0.99, type = "bca", index = 2)
int99
        ##el intervalo de 0.95 no incluye el 0, el de 0.99 si. Elegimos el de 0.95
##grafico histograma, qqplot y jack plot
plot(boot1, index = 2, jack = T)
##comparo con cor.test() que asume normalidad
(cor.test(dat$ramas, dat$semillas))
##los intervalos de confianza son más grandes, pero el resultado es igual.
