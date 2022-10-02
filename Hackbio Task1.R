getwd()
read.csv('IRIS.csv', header = T)
Data <- read.csv('IRIS.csv', header = T)
SL <- iris$Sepal.Length
SW <- iris$Sepal.Width
plot(SL, SW)
dev.off()
plot(SL, SW)
par(mar=c(1,1,1,1))
plot(SL, SW)
plot(SL, SW)
View(Data)
iris$species
plot(x = SL, y = SW, col = 'red', pch = 21, main = 'Length vs Width' , xlab = 'Sepal Length', ylab = 'Sepal Width')
plot(x = SL, y = SW, col = factor(iris$Species), pch = 19, main = 'Sepal Length vs Width' , xlab = 'Sepal Length', ylab = 'Sepal Width')
legend('bottomright', legend = levels(iris$Species), col = 1:3, pch = 19)

PL <- iris$Petal.Length
PW <- iris$Petal.Width
plot(x = PL, y = PW, col = factor(iris$Species), pch = 19, main = 'Petal Length vs Width' , xlab = 'Petal Length', ylab = 'Petal Width')
legend('topleft', legend = levels(iris$Species), col = 1:3, pch = 19)
df <- iris[1:4]
pairs(df)