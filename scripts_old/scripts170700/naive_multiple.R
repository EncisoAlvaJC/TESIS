is.visible = function(y,a,b){
  isit = TRUE
  c    = a+1
  while( (isit) && (c<b) ){
    isit = y[c] < ( y[b]+(y[a]-y[b])*(b-c)/(b-a) )
    c    = c+1
  }
  return(isit)
}

require(plotrix)

center = getwd()

for(maxi in c(0.25,0.5,1,5,10,20,30)*512){
  for(sujeto in 1:2){
    for(ch in c(7,8,9,10)){
      source('naive_vc.R')
      print(paste(sujeto,toString(maxi),toString(ch)))
      
      setwd(center)
    }
  }
}