dF7 = scan('dF7.csv')
dF8 = scan('dF8.csv')

nF7 = scan('nF7.csv')
nF8 = scan('nF8.csv')


###########################################

df7.t = ts(dF7,frequency=512)
nf7.t = ts(nF7,frequency=512)

df8.t = ts(dF8,frequency=512)
nf8.t = ts(nF8,frequency=512)

###########################################

dev.off()
pdf('graficaintro.pdf',width=11.744,height=5.544)

par(mfrow=c(2,1),mar=c(2,2,2,1)+0.1)
plot(nf8.t,main='Sin deterioro cognitivo',cex.main=2)
plot(df8.t,main='PDC',cex.main=2)

dev.off()

#dev.copy(pdf,'graficaintro.pdf')

###########################################

dev.off()
pdf('estudio.pdf',width=11.744,height=5.544)

par(mfrow=c(2,2),mar=c(2,2,2,1)+0.1)
plot(nf7.t,main='F7 , Sin deterioro cognitivo',cex.main=1.5)
plot(nf8.t,main='F8 , Sin deterioro cognitivo',cex.main=1.5)
plot(df7.t,main='F7 , PDC',cex.main=1.5)
plot(df8.t,main='F8 , PDC',cex.main=1.5)

dev.off()

###########################################
###########################################

S.d7 = stl(df7.t,s.window='periodic',robust=TRUE)
S.n7 = stl(nf7.t,s.window='periodic',robust=TRUE)
S.d8 = stl(df8.t,s.window='periodic',robust=TRUE)
S.n8 = stl(nf8.t,s.window='periodic',robust=TRUE)

dev.off()
pdf('stl_n_f7.pdf',width=5.872,height=6.544)
plot(S.n7,main='F7',cex=4)
dev.off()
dev.off()
pdf('stl_n_f8.pdf',width=5.872,height=6.544)
plot(S.n8,main='F8',cex=4)
dev.off()

dev.off()
pdf('stl_d_f7.pdf',width=5.872,height=6.544)
plot(S.d7,main='F7',cex=4)
dev.off()
dev.off()
pdf('stl_d_f8.pdf',width=5.872,height=6.544)
plot(S.d8,main='F8',cex=4)
dev.off()

