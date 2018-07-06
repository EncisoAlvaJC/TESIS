require(tseriesChaos)

for(maxi in c(0.25,0.5,1,5,10,20,30)*512){
  for(sujeto in 1:2){
    for(ch in c(7,8,9,10)){
      source('naive_recurr.R')
      print(paste(sujeto,toString(maxi),toString(ch)))
    }
  }
}