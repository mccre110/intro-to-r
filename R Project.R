library(ggplot2)
library(data.table)
library(ggthemes)

avo <-read.csv(file.choose(), sep="|")
head(avo)

#avo$Month <- format(as.Date(avo$Date), "%m")
#avo$region <- factor(avo$region)

avo$year <- factor(avo$year)
setDT(avo)[, Month := format(as.Date(Date), "%m") ]
avo$Month <- factor(avo$Month)

#1
m1 <- lm(AveragePrice~Total.Volume,avo)
summary(m1)

ggplot(avo, aes(x =Total.Volume, y =AveragePrice)) + geom_point() +
  geom_smooth(method = "lm") + theme_economist()


ggplot(avo, aes(x =Date, y = AveragePrice, color = year)) + geom_point() +
  geom_smooth(method = "lm") + theme_economist()


ggplot(avo, aes(x =Month, y = AveragePrice, fill=type)) + geom_boxplot() +
  geom_smooth(method = "lm")+ theme_economist()

ggplot(avo,aes(year,AveragePrice) ) + 
  geom_bar(stat = "identity", aes(fill = type)) + theme_economist()

ggplot(avo,aes(year,Total.Volume) ) + 
  geom_bar(stat = "identity", aes(fill = type))

#2
m2 <- aov(AveragePrice~type+month+year, data = avo)
summary(m2)

ggplot(avo, aes(x = type, y = AveragePrice)) +
  geom_boxplot(aes(group = type, fill =type)) + theme_economist()


#3
m3 <- kmeans(avo[,c("Total.Bags", "AveragePrice")],centers = 2)

avo$cluster <- factor(m3$cluster)

ggplot(avo,aes(x =Total.Bags, y =AveragePrice, color = cluster, shape=type)) +
  geom_point()+ theme_economist()


m3 <- kmeans(mtcars[,c("mpg","qsec")],centers = 2)

mtcars$cluster <- factor(m3$cluster)

ggplot(mtcars,aes(x = mpg, y= qsec, color = cluster)) +
  geom_point()

#4
m4 <- glm(type~AveragePrice, data = avo,
          family = "binomial")
summary(m4)

predict <- predict(m4, type = 'response')
table(avo$type, predict > 0.5)

#5
t <- xtabs(~type+Month, data = avo)
chisq.test(t)

m5 <- aov(AveragePrice~Month+type, data = avo)
summary(m5)

ggplot(avo, aes(x = Month, y = Total.Volume, fill = type)) +
  geom_boxplot() + theme_economist()



