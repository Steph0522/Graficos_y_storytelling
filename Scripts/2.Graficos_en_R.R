par(mfrow= c(1,3)) 
plot(x=iris$Sepal.Length, y=iris$Sepal.Width)
plot(x=iris$Sepal.Length, y = iris$Species)
plot(x=iris$Sepal.Length)

par(mfrow= c(1,3))
plot(x = iris$Species, y = iris$Sepal.Length)
plot(x=iris$Species, y=iris$Species)
plot(x=iris$Species)

hist(x = iris$Sepal.Length, 
     main = "Histograma de longitud de sepalo", 
     xlab = "Longitud", ylab = "Frecuencia",
     col = "purple")


plot(x = iris$Petal.Length, 
     y = iris$Petal.Width, 
     col = iris$Species, 
     xlab = "Largo", 
     ylab = "Ancho")

plot(x = iris$Petal.Length, 
     y = iris$Petal.Width, 
     col = iris$Species)
legend(x = "topleft", 
       legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = c("black", "red", "green"), 
       title = "Especie")

plot(x=iris$Species, 
     y = iris$Sepal.Length, 
     xlab = "Especie",
     ylab = "Longitud Sépalo", 
     col = c("purple", "pink", "blue"))

boxplot(formula = Sepal.Length ~ Species, 
        data = iris, 
        xlab = "Especie", 
        ylab = "Longitu Sépalo", 
        col = c("purple", "pink", "blue"))

# ?plot

#dataset de ensayo
df<- data.frame(x= c(1:5),y= c(200, 400, 600, 700, 500))
par(mfrow = c(1, 3))
plot(df$x, df$y, type = "p", main = 'type = "p"')
plot(df$x, df$y, type = "l", main = 'type = "l"')
plot(df$x, df$y, type = "b", main = 'type = "b"')


par(mfrow = c(1, 3))
plot(df$x, df$y, type = "c", main = 'type = "c"')
plot(df$x, df$y, type = "s", main = 'type = "s"')
plot(df$x, df$y, type = "h", main = 'type = "h"')

par(mfrow = c(1, 3))
plot(df$x, df$y, type = "l", lty=1, main = 'type = "l1"')
plot(df$x, df$y, type = "l", lty=2, main = 'type = "l2"')
plot(df$x, df$y, type = "l", lty=3, main = 'type = "l3"')

par(mfrow = c(1, 3))
plot(df$x, df$y, type = "l", lty=4, main = 'type = "l4"')
plot(df$x, df$y, type = "l", lty=5, main = 'type = "l5"')
plot(df$x, df$y, type = "l", lty=6, main = 'type = "l6"')

barplot(y ~ x , 
        data = df, 
        main = "Barplot", 
        col = "darkred")

# plot(df$x,
#      df$y,
#      type = "p",
#      main = 'type = "p-capas"')
# abline(h=400,
#        v=3,
#        col="red",
#        lty=2)
# text(df,
#      labels=rownames(df),
#      cex=0.7,
#      pos=2,
#      col="blue")

plot(df$x, 
     df$y, 
     type = "p", 
     main = 'type = "p-capas"')
abline(h=400, 
       v=3, 
       col="red", 
       lty=2)
text(df, 
     labels=rownames(df), 
     cex=0.7, 
     pos=2, 
     col="blue")

# png(filename="gráfica1.png", width=648, height=432)
# plot(df$x, df$y, type = "p", main = 'type = "p"')
# dev.off()
