# librerias
require('ggpubr')
require('reshape')

# variables: tamnno
N     = 2000
iter  = 6

# inicializacion
K = seq(0,1,1/N)
tt    = K

N = length(K)

M = matrix(0,ncol=iter+1,nrow = N)
M[,1]      = K
M[,iter+1] = tt

for(j in 2:iter){
  K_old = K
  {
    lk = length(K_old)
    K_new  = rep(.5,lk)
    for(i in seq(1,lk/3)){
      K_new[i] = .5*K_old[3*i]
    }
    cl = 2*lk
    for(i in seq(ceiling(2*lk/3+1),lk)){
      #print(i)
      K_new[i] = .5*K_old[ceiling(3*i-cl)] + .5
    }
  }
  K = K_new
  M[,j] = K
  #plot(tt,K_new,type='l')
}

M = as.data.frame(M)
colnames(M) = c(1:iter,'tt')

M2 = melt(M,id.vars = 'tt')
colnames(M2) = c('tt','ind','yy')

ggplot(M2,aes(x=tt,y=yy,color=ind)) +
  #scale_color_manual(values = rev(gray(.5+.5*(0:(iter-1))/(iter-1))))+
  scale_color_brewer(palette = 'Blues',
                     labels=c(expression(K[0]),expression(K[1]),
                              expression(K[2]),expression(K[3]),
                              expression(K[4]),expression(K[5])))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_bw() +
  xlab('x') + ylab(expression(K[n](x)))+
  labs(color='')+
  theme(legend.position = c(.1,.75),legend.background = element_blank()) +
  geom_line()

ggsave('cantor.pdf',device='pdf',dpi=600,
       width = 15,height = 8,unit='cm')
