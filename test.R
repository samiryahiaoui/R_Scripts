# Problem1

data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)

pdf("LabChap4.pdf", width=6, height=5)
plot(EuStockMarkets)


# Problem2

logR <- diff(log(EuStockMarkets))
plot(logR)

# Problem3

plot(as.data.frame(logR))

index.names <- dimnames(logR)[[2]]
par(mfrow=c(2,2))
for(i in 1:4){
  qqnorm(logR[,i],datax=T, main=index.names[i])
  qqline(logR[,i],datax=T)
  print(shapiro.test(logR[,i]))
}

n <- dim(logR)[1]
q.grid <- (1:n)/(n+1)
df <- c(1,4,6,10,20,30)
for(i in 1:4){
  windows()
  par(mfrow=c(3,2))
  for(j in 1:6){
    qqplot(logR[,i],qt(q.grid,df[j]), main=paste(index.names[i], ", df=",df[j]))
    abline(lm(qt(c(0.25,0.75),df=df[j])~quantile(logR[,i],c(.25,.75))))
  }
}

library("fGarch")
x <- seq(-.1,.1,by=.001)
par(mfrow=c(1,1))
plot(density(logR[,1]), lwd=2, ylim=c(0,60))
lines(x,dstd(x,mean=median(logR[,1]), sd=mad(logR[,1]), nu=5), lty=5, lwd=2)
lines(x,dnorm(x,mean=mean(logR[,1]), sd=sd(logR[,1])), lty=3, lwd=4)
legend("topleft",c("KDE","t: df=5", "normal"), lwd=c(2,2,4), lty=c(1,5,3))

plot(density(logR[,1]), lwd=2, ylim=c(0,10), xlim=c(-0.1,0))
lines(x,dstd(x,mean=median(logR[,1]), sd=mad(logR[,1]), nu=5), lty=5, lwd=2)
lines(x,dnorm(x,mean=mean(logR[,1]), sd=sd(logR[,1])), lty=3, lwd=4)
legend("topleft",c("KDE","t: df=5", "normal"), lwd=c(2,2,4), lty=c(1,5,3))

plot(density(logR[,1]), lwd=2, ylim=c(0,10), xlim=c(0,0.1))
lines(x,dstd(x,mean=median(logR[,1]), sd=mad(logR[,1]), nu=5), lty=5, lwd=2)
lines(x,dnorm(x,mean=mean(logR[,1]), sd=sd(logR[,1])), lty=3, lwd=4)
legend("topleft",c("KDE","t: df=5", "normal"), lwd=c(2,2,4), lty=c(1,5,3))

graphics.off()
