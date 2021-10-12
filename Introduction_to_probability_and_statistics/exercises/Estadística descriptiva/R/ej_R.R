data <- iris
head(data, 6)
#media
mean(data$Sepal.Length)
#mediana
median(data$Sepal.Length)
#moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$Sepal.Length)

require(modeest)
mfv(data$Sepal.Length)
#mínimo y máximo
min(data$Sepal.Length)
max(data$Sepal.Length)
#rango
range(data$Sepal.Length)
#quantiles
quantile(data$Sepal.Length)
#rango intercuartil
IQR(data$Sepal.Length)
#varianza y desviación estándar:
var(data$Sepal.Length)
sd(data$Sepal.Length)
#desviación mediana
mad(data$Sepal.Length)
#summary
summary(data$Sepal.Length)

require("pastecs")
stat.desc(data)

#boxplot
boxplot(data$Sepal.Length)
boxplot(data)
require("ggpubr")
ggboxplot(data,y = "Sepal.Length", width = 0.5)
#histograma
hist(data$Sepal.Length)
gghistogram(data, x = "Sepal.Length", bins = 9, 
            add = "mean")

