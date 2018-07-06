parameters <- c(s = 10, r = 28, b = 8/3)
state <- c(X = .1, Y = 1, Z = 1)

Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- s * (Y - X)
    dY <- X * (r - Z) - Y
    dZ <- X * Y - b * Z
    list(c(dX, dY, dZ))
  })
}

times <- seq(0, 50, by = 0.01)
library(deSolve)
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

#par(oma = c(0, 0, 3, 0))
#plot(out, xlab = "time", ylab = "-")
#plot(out[, "Y"], out[, "Z"], pch = ".", type = "l")
#mtext(outer = TRUE, side = 3, "Lorenz model", cex = 1.5)

require(ggplot2)
require(ggpubr)

out = as.data.frame(out)

A = ggplot(out,aes(x=time,y=X)) + geom_line(color='red') + theme_bw() + labs(x=NULL)
B = ggplot(out,aes(x=time,y=Y)) + geom_line(color='blue') + theme_bw() + labs(x=NULL)
C = ggplot(out,aes(x=time,y=Z)) + geom_line(color='green') + theme_bw() + labs(x=NULL)

ggarrange(A,B,C,ncol = 1,nrow=3,align='v',common.legend=TRUE)

ggplot(out,aes(x=X,y=Y)) + geom_line() + theme_bw()
ggplot(out,aes(x=X,y=Z)) + geom_line() + theme_bw()
ggplot(out,aes(x=Y,y=Z)) + geom_line() + theme_bw()

require(rgl)

ggplot(out,aes(x=X,y=Z,color=time)) + 
  geom_line() + theme_bw() + labs(x=NULL)

ggplot(out,aes(x=X,y=Y,color=time)) + 
  geom_point() + theme_bw() + labs(x=NULL)
ggplot(out,aes(x=X,y=Z,color=time)) + 
  geom_point() + theme_bw() + labs(x=NULL)
