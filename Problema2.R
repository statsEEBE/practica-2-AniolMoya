#Codigo para problema 2
md <- iris
mean(md$Petal.Length)
sd(md$Petal.Length) #desviación típica
hist(md$Petal.Length) #hisotgrama
x<-md$Petal.Length
y<-md$Sepal.Length
m<- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2) #formula m regresion lineal min cuad
b<-mean(y)-mean(x) #formula b regresion lineal min cuad
mod<- lm(y~x) #per ajustar models de regressio lineal(et calcula m i b)
summary(mod) #resumen estadistico del modelo de regresion lineal
ypredict <- predict(mod, data.frame(x=x)) #asignar prediccio de y mitjançant la recta de regressió lineal
plot(x,y, col='blue',pch=16)#plot dels punts en blau i no buits
lines(x, ypredict,col='red') #dibuixar linea
R^2<- sum((ypredict-mean(y))^2)/sum((y-mean(y))^2) #Coef de determinacion (error de la recta de regresion)

