library(fractal)

n7 = scan('nF7.csv')
n8 = scan('nF8.csv')
d7 = scan('dF7.csv')
d8 = scan('dF8.csv')


n7.t = ts(n7,frequency=512)
n8.t = ts(n8,frequency=512)
d7.t = ts(d7,frequency=512)
d8.t = ts(d8,frequency=512)

s.n7 = stl(n7.t,s.window='periodic',robust=T)
s.n8 = stl(n8.t,s.window='periodic',robust=T)
s.d7 = stl(d7.t,s.window='periodic',robust=T)
s.d8 = stl(d8.t,s.window='periodic',robust=T)

n7.r.t = s.n7$time.series[,'remainder']
n8.r.t = s.n8$time.series[,'remainder']
d7.r.t = s.d7$time.series[,'remainder']
d8.r.t = s.d8$time.series[,'remainder']

n7.r = unclass(n7.r.t)
n8.r = unclass(n8.r.t)
d7.r = unclass(d7.r.t)
d8.r = unclass(d8.r.t)

n7.r = n7.r[1:length(n7.r)]
n8.r = n8.r[1:length(n8.r)]
d7.r = d7.r[1:length(d7.r)]
d8.r = d8.r[1:length(d8.r)]



zn7o = stationarity(n7)
zn8o = stationarity(n8)
zd7o = stationarity(d7)
zd8o = stationarity(d8)

zn7f = stationarity(n7.r)
zn8f = stationarity(n8.r)
zd7f = stationarity(d7.r)
zd8f = stationarity(d8.r)





print(zn7o)
print(zn8o)
print(zd7o)
print(zd8o)

print(zn7f)
print(zn8f)
print(zd7f)
print(zd8f)




yn7o = zn7o$anova
yn8o = zn8o$anova
yd7o = zd7o$anova
yd8o = zd8o$anova

yn7f = zn7f$anova
yn8f = zn8f$anova
yd7f = zd7f$anova
yd8f = zd8f$anova



fn7o = zn7o$freq
fn8o = zn8o$freq
fd7o = zd7o$freq
fd8o = zd8o$freq

fn7f = zn7f$freq
fn8f = zn8f$freq
fd7f = zd7f$freq
fd8f = zd8f$freq



write.csv(yn7o,'yn7o.csv')
write.csv(yn8o,'yn8o.csv')
write.csv(yd7o,'yd7o.csv')
write.csv(yd8o,'yd8o.csv')

write.csv(yn7f,'yn7f.csv')
write.csv(yn8f,'yn8f.csv')
write.csv(yd7f,'yd7f.csv')
write.csv(yd8f,'yd8f.csv')



write.csv(fn7o,'fn7o.csv')
write.csv(fn8o,'fn8o.csv')
write.csv(fd7o,'fd7o.csv')
write.csv(fd8o,'fd8o.csv')

write.csv(fn7f,'fn7f.csv')
write.csv(fn8f,'fn8f.csv')
write.csv(fd7f,'fd7f.csv')
write.csv(fd8f,'fd8f.csv')
