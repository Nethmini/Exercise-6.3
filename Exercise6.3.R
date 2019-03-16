hmv = read.csv("Home Market Value.csv")
hmv$Market.Value = as.numeric(gsub('[$,]','',hmv$Market.Value))
hmv$Square.Feet = as.numeric(gsub('[$,]','',hmv$Square.Feet))

summary(hmv$House.Age)
summary(hmv$Market.Value)
summary(hmv$Square.Feet)
summary(hmv)

plot(hmv)

hist(hmv$House.Age)
hist(hmv$House.Age, xlab="House.Age", ylab="Frequency", main="HOUSE AGE FREQUENCY", col="BLUE")
hist(hmv$Square.Feet, xlab="Square.Feet", ylab="Frequency", main="SQUARE FEET AMOUNT FREQUENCY", col="RED")
hist(hmv$Market.Value, xlab="Market.Value", ylab="Frequency", main="MARKET VALUE FREQUENCY", col="GREEN",)

##Scatter Plot
plot(hmv$House.Age, hmv$Market.Value)
plot(hmv$Square.Feet, hmv$Market.Value)

par(mfrow=c(1,2))

boxplot(hmv$Market.Value~hmv$House.Age, data = hmv)
boxplot(hmv$Market.Value~hmv$Square.Feet, data = hmv)

model = lm(hmv$Market.Value~hmv$Square.Feet+hmv$House.Age)
summary(model)

#Predict market value
tarAge = c(26,28,29,30,31)
tarSquareFeet = c(1650,1500,1800,2200,2400)
tarData = data.frame(House.Age = tarAge,Square.Feet = tarSquareFeet)
hmv.pred = predict(model,tarData, level = 0.95, interval = "confidence")

z = hmv$Market.Value
x = hmv$Square.Feet
y = hmv$House.Age
model = lm(z~x + y)
tarData = data.frame(x = tarSquareFeet, y = tarAge)
hmv.pred = predict(model, tarData, level = 0.95, interval = "confidence")
hmv.pred
