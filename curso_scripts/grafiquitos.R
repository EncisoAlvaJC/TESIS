N = 5000

set.seed(2017)
x = rchisq(N,1)

plot(x,type='l')

y = seq(-3,3,by=0.01)
plot(y,dt(y,5),type='l',xlab='',ylab='prob',bty='n')
rect(-1,0,1,.4,col=rgb(0,0,1,alpha=.1),border=NA)
rect(-2,0,2,.4,col=rgb(0,0,1,alpha=.1),border=NA)
rect(-3,0,3,.4,col=rgb(0,0,1,alpha=.1),border=NA)
abline(v=0,col='red',lwd=2)




dev.off()

N = 400
set.seed(2017)
e = rnorm(N)
X = rep(0,N)
for(i in 3:length(X)){
  X[i] = 0.5*(X[i-1]+X[i-2]+e[i])
}

plot(X,xlab='',ylab='',type='l',bty='n')

dev.off()

par(mfrow=c(1,2))
plot(X,xlab='',ylab='',type='l',bty='n')
acf(X,bty='n',main='')

dev.off()

par(mfrow=c(1,2))
plot(X,xlab='',ylab='',type='l',bty='n')

y = seq(min(X),max(X),by=0.01)

MM = mean(X)
SSD = sd(X)

plot(dnorm(y,MM,SSD),y,type='l',xlab='',ylab='prob',bty='n')
rect(0,MM-1*SSD,.37,MM+1*SSD,col=rgb(0,0,1,alpha=.1),border=NA)
rect(0,MM-2*SSD,.37,MM+2*SSD,col=rgb(0,0,1,alpha=.1),border=NA)
rect(0,max(MM-3*SSD,min(X)),.37,
     min(MM+3*SSD,max(X)),col=rgb(0,0,1,alpha=.1),border=NA)
abline(h=MM,col='red',lwd=2)

dev.off()


plot(X,xlab='',ylab='',type='l',bty='n')
rect(0,MM-1*SSD,400,MM+1*SSD,col=rgb(0,0,1,alpha=.1),border=NA)
rect(0,MM-2*SSD,400,MM+2*SSD,col=rgb(0,0,1,alpha=.1),border=NA)
rect(0,max(MM-3*SSD,min(X)),.37,
     min(MM+3*SSD,max(X)),col=rgb(0,0,1,alpha=.1),border=NA)
abline(h=MM,col='red',lwd=2)



dev.off()
N = 100
s = seq(0,N,by=1)
plot(s*s,type='l',col='blue',bty='n',xlab='N',ylab='')
lines(7*s*log(s),type='l',col='red')





dev.off()

par(mfrow=c(1,2))
plot(X,xlab='t',ylab='mV',type='l',bty='n')

f = Mod(fft(X))
f = f[1:length(f)/2]
plot((f),type='l',bty='n',xlab='Hz',ylab='dB')


plot(dnorm(y,MM,SSD),y,type='l',xlab='',ylab='prob',bty='n')
rect(0,MM-1*SSD,.37,MM+1*SSD,col=rgb(0,0,1,alpha=.1),border=NA)
rect(0,MM-2*SSD,.37,MM+2*SSD,col=rgb(0,0,1,alpha=.1),border=NA)
rect(0,max(MM-3*SSD,min(X)),.37,
     min(MM+3*SSD,max(X)),col=rgb(0,0,1,alpha=.1),border=NA)
abline(h=MM,col='red',lwd=2)