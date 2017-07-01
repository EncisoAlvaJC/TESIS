library(tuneR)

frec = 512
dur_s = 10

P = attr(noise(kind=c('pink'),samp.rate=frec,duration=dur_s,
               xunit=c('time')),
         'left')
te = seq(1:(frec*dur_s))/frec
plot(te,P,type='l')

################################################################
################################################################
################################################################

gg = function(e,t){
  val = 0
  if(abs(t)<e){
    val = 0.5 + 0.5*cos(pi*t*e)
  }
  return(val)
}

gg = function(M,t){
  val = 0
  if(abs(t)<M){
    val = 0.5 + 0.5*cos(pi*t/M)/(pi*t/M)
  }
  return(val)
}

################################################################
################################################################

X = P
frec = 512
dur_s = 10

num_fr = 50

ii = complex(real=0,imaginary=1)

MM = frec
NN = frec/2

st = NN/2

tt = 1:length(X)

tt_sm = seq(floor(st),ceiling(length(X)-st))

ww_sm = seq(-pi,pi,2*pi/num_fr)

U = matrix(nrow=length(tt_sm),ncol=length(ww_sm))

U_tmp = 0