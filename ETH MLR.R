#Preprocessing
ETH <- as.data.frame(ETH)
data.class(ETH)
names(ETH)

#Histograms
hist(ETH$`Price (Close)`, main = "Histogram of ETH Price", xlab = "Price", ylab = "Frequency")
hist(ETH$`Bitcoin Price (Close)`, main = "Histogram of Bitcoin Price", xlab = "Bitcoin Price", ylab = "Frequency")
hist(log(ETH$Volume), main = "Histogram of ETH Volume", xlab = "Volume in Billions", ylab = "Frequency")
hist(ETH$`Ethereum (Google Interest)`, main = "Histogram of ETH Interest", xlab = "Google Interest", ylab = "Frequency")
hist(ETH$`NFT (Google Interest)`, main = "Histogram of NFT Interest", xlab = "NFT Interest", ylab = "Frequency")

#Time-Series Plots
ts.plot(ETH$`Price (Close)`, main = "Time Series Of ETH Price", ylab = "ETH Price ($)")
ts.plot(ETH$Volume, main = "Time Series Of ETH Volume", ylab = "ETH Volume ($)")
ts.plot(ETH$`Bitcoin Price (Close)`, main = "Time Series Of Bitcoin Price", ylab = "Bitcoin Price ($)")
ts.plot(ETH$`Ethereum (Google Interest)`, main = "Time Series Of ETH Interest (Google)", ylab = "Google Interest Score")
ts.plot(ETH$`NFT (Google Interest)`, main = "Time Series Of NFT Interest", ylab = "NFT Interest Score")

#Time Series Plots using ggplot/plotly
library("plotly")
P <- ggplot(ETH, aes(x= Date, y= `Price (Close)`)) + geom_line() +  xlab("Date")
P

BP <- ggplot(ETH, aes(x= Date, y= `Bitcoin Price (Close)`)) + geom_line() +  xlab("Date")
BP

V <- ggplot(ETH, aes(x= Date, y= Volume)) + geom_line() +  xlab("Date") + scale_y_continuous(labels = scales::unit_format(unit = "B", scale = 1e-9))
V

EI <- ggplot(ETH, aes(x= Date, y= `Ethereum (Google Interest)`)) + geom_line() +  xlab("Date")
EI

NFT <- ggplot(ETH, aes(x= Date, y= `NFT (Google Interest)`)) + geom_line() +  xlab("Date")
NFT



#Plots
plot(log(ETH$Volume),ETH$`Price (Close)`, main = "ETH Price by Volume", ylab = "ETH Price ($)", xlab = "ETH Volume (Billions of Dollars)" )
plot(ETH$`Bitcoin Price (Close)`,ETH$`Price (Close)`, main = "ETH Price by Bitcoin Price", ylab = "ETH Price ($)", xlab = "Bitcoin Price ($)")
plot(ETH$`Ethereum (Google Interest)`,ETH$`Price (Close)`, main = "ETH Price by Interest", ylab = "ETH Price ($)", xlab = "Google Interest Score")
plot(ETH$`NFT (Google Interest)`, ETH$`Price (Close)`, main = "ETH Price by NFT Interest", ylab = "ETH Price ($)", xlab = "NFT Interest Score")


#Summary
library("YRmisc")
summary(ETH[,2:6])     
ds.summ(ETH[,2:6])   

# Correlation
names(ETH)
cor(ETH[,2:6])      
round(cor(ETH[,2:6]))

#Regression Model
fit<-lm(ETH$`Price (Close)` ~ ETH$Volume + ETH$`Bitcoin Price (Close)` + ETH$`Ethereum (Google Interest)` + ETH$`NFT (Google Interest)` + ETH$Obs, data=ETH)
fit

summary(fit)
names(summary(fit))
summary(fit)$r.squared

vdf<-data.frame(ETH,p=fit$fitted.values,r=fit$residuals)


hist(vdf$r, main = "Histogram Of Residuals", xlab = "Residuals")      
coefs <- fit$coefficients


plot(vdf$p,vdf$Price..Close., main = "Actual vs Predicted ETH Price ($)", xlab = "Predicted Values",  ylab = "Actual Values")

abline(lm(vdf$Price..Close. ~ vdf$p), col = "red")

pl.2ts(vdf$Price..Close.,vdf$p, "Times Series Plot of Actual Price and Predicted")


plot(vdf$Date, vdf$Price..Close., type = "l", col = "black", main = "Times Series Plot of Actual Price and Predicted", ylab = "ETH Price ($)", xlab = "Year")
points(vdf$Date, vdf$p, type = "l", col = "red")

