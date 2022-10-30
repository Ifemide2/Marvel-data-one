31*78
697/41
314+654
56765-23
x <- 39
y <- 14
z <- x-y
log2(sqrt(2345))
vec1 <- c(2,5,8,12,16)
seq(from=2,by=3,length.out=100)
a <- seq(from=2,by=3,length.out=100)
a[c(5,10,15,20)]
a[10:30]
mouse_color <- c("purple","red","yellow","brown")
mouse_weight <- c(23,21,18,16)
mouse_info <- data.frame(color=mouse_color, weight=mouse_weight)
mouse_info
b <- read.delim("https://raw.githubusercontent.com/HackBio-Internship/public_datasets/main/R/small_file.txt")
b
c <- read.csv("https://github.com/HackBio-Internship/public_datasets/blob/main/R/Child_Variants.csv?raw=true")
c
mean(c$MutantReadPercent)
d <- subset(c, MutantReadPercent>=70, select = c(MutantReadPercent))
d
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