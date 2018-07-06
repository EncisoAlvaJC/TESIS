t = seq(-pi,pi,by=.01)

q = 10

par(mar = c(2,1,1,1))

plot(c(-pi,pi),type='o',col='white',yaxt='n',xlim=c(-pi,pi),ylim=c(0,2.5*(q+1)+1),bty='n',
     xlab='',ylab='')

k = 1
for(i in 0:q+1){
  #lines(t,2*(i)+t*0,col='gray1')
  lines(t,k+cos(i*t/2),col='red')
  lines(t,k-cos(i*t/2),col='blue')
  k = k +2.5
}

# plot(c(-pi,pi),type='o',col='white',yaxt='n',xlim=c(-pi,pi),ylim=c(0,2*(su+1)),bty='n',
#      xlab='',ylab='')
# 
# su = 1
# for(i in 1:q){
#   lines(t,2*(su)+t*0,col='gray')
#   lines(t,2*(su)+sin(i*t)/i,col='black')
#   su = su + 1/i
# }

gg = function(uu,MM){
  val = 0
  if(abs(uu)<MM){
    val = 0.5 + 0.5*cos(pi*uu/MM)#/(pi*uu/MM)
  }
  return(val)
}

wind = t
for(j in 1:length(t)){
  wind[j] = gg(t[j],pi)
}

plot(c(-pi,pi),type='o',col='white',yaxt='n',xlim=c(-pi,pi),ylim=c(0,2.5*(q+1)+1),bty='n',
     xlab='',ylab='')

k = 1
for(i in 0:q+1){
  #lines(t,2*(i)+t*0,col='gray1')
  lines(t,k+cos(i*t/2)*wind,col='red')
  lines(t,k-cos(i*t/2)*wind,col='blue')
  k = k +2.5
}
