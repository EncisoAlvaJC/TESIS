###########################################################
## Funciones de ventana #####################################

require('ggpubr')

###########################################################
setwd('C:/Users/EQUIPO 1/Desktop/julio/Tesis_respaldo/TESIS/img_ventanas')
grabar = T

#n = 1

# funcion generadora k, definicion

if(n==1){
  # Bartlett
  nombre = 'bartlett'
  k.base = function(u){
    if(abs(u)>1){
    return(0)
    }
    return(1)
  }
  
  k.trans = function(th){
    if(th==0){
      return(1/pi)
    }
    return((1/pi)*(sin(th))/th)
  }
}
if(n==2){
  # Fejer
  nombre = 'fejer'
  k.base = function(u){
    if(abs(u)>1){
      return(0)
    }
    return(1-abs(u))
  }
  
  k.trans = function(th){
    if(th==0){
      return(1/(2*pi))
    }
    return((.5/pi)*((sin(th/2)/(th/2))**2))
  }
}
if(n==3){
  # Daniell
  nombre = 'daniell'
  k.base = function(u){
    if(abs(u)==0){
      return(1)
    }
    return(sin(pi*u)/(pi*u))
  }
  
  k.trans = function(th){
    if(th>pi){
      return(0)
    }
    return(1/(2*pi))
  }
}
if(n==4){
  # Parzen 1
  nombre = 'parzen1'
  k.base = function(u){
    if(abs(u)>1){
      return(0)
    }
    return(1-u*u)
  }
}
if(n==5){
  # Parzen 2
  nombre = 'parzen2'
  k.base = function(u){
    if(abs(u)>1){
      return(0)
    }
    return(1/(1+abs(u)))
  }
}
if(n==6){
  # Parzen 3
  nombre = 'parzen3'
  k.base = function(u){
    #if(abs(u)>1){
    #  return(0)
    #}
    return(1/(1+u*u))
  }
}
if(n==7){
  # Parzen 4
  nombre = 'parzen4'
  k.base = function(u){
    if(abs(u)>1){
      return(0)
    }
    if(abs(u)<.5){
      return(1-6*u*u+6*abs(u)*abs(u)*abs(u))
    }
    return((2*((1-abs(u))**3)))
  }
}
if(n==8){
  # Tukey
  nombre = 'tukey'
  a = .25
  k.base = function(u){
    if(abs(u)>1){
      return(0)
    }
    return(1-2*a+2*a*cos(pi*u))
  }
}

########################################

if(n==9){
  # Neave
  nombre = 'neave'
  a = .3
  b = .6
  k.base = function(u){
    if(abs(u)>1){
      return(0)
    }
    if(abs(u)<a){
      return(1)
    }
    if(abs(u)<b){
      return( (1/(1-a))*(1 -u +((b-a)/pi)*sin(((b-u)/(b-a))*pi) ) )
    }
    return( (1/(1-a))*(1 -u -((1-b)/pi)*sin(((u-b)/(1-b))*pi) ) )
  }
}
if(n==10){
  # Cuadratica
  nombre = 'cuadratica'
  k.base = function(u){
    if(u==0){
      return(1)
    }
    return((25/(12*((pi*u)**2)))*((sin(pi*u*6/5))/(pi*u*6/5) - cos(pi*u*6/5)))
  }
}
if(n==11){
  # Bartlett-Priestley
  nombre = 'bartlet_priestley'
  k.base = function(u){
    if(u==0){
      return(1)
    }
    return((3/((pi*u)**2))*((sin(pi*u))/(pi*u) - cos(pi*u)))
  }
  
  k.trans = function(th){
    if(th>pi){
      return(0)
    }
    return( (3/(4*pi))*(1-th/pi) )
  }
}
if(n==12){
  # Papoulis
  nombre = 'papoulis'
  k.base = function(u){
    if(u>1){
      return(0)
    }
    if(u==0){
      return(1)
    }
    return(((1-u)*cos(pi*u)+sin(pi*u)/(pi*u))*.5)
  }
}
if(n==13){
  # Cosenoidal
  nombre = 'cosenoidal'
  k.base = function(u){
    if(u>1){
      return(0)
    }
    return(cos(pi*u/2))
  }
}
if(n==14){
  # Trapezoidal
  nombre = 'trapezoidal'
  a = .5
  k.base = function(u){
    if(u>1){
      return(0)
    }
    if(abs(u)<a){
      return(1)
    }
    return((u-1)/(a-1))
  }
}
if(n==15){
  # Normal
  nombre = 'normal'
  s = 1/3
  k.base = function(u){
    #if(u>1){
    #  return(0)
    #}
    return(exp(-(u**2)/(2*(s**2))))
  }
}

#################################################################
# funcion de respuesta

# funcion k, vectorizada
k = function(u){
  sapply(u,function(tt) k.base(tt) )
}

# # funcion generadora k, truncada
# k.t.base = function(u){
#   if(abs(u)>1){
#     return(0)
#   }else{
#     if(u==0){
#       return(1)
#     }
#   }
#   return((3/((pi*u)**2))*((sin(pi*u))/(pi*u) - cos(pi*u)))
# }
# 
# # funcion k, vectorizada
# k.t = function(u){
#   sapply(u,function(tt) k.t.base(tt) )
# }

# grafica de k
u = seq(0,1.5,by=1/500)
u = as.data.frame(u)
colnames(u) = c('xx')
u$yy = k(u$xx)

ggplot(u,aes(x=xx,y=yy)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw()+
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
       plot.background = element_rect(fill = 'transparent',colour = NA))+
  scale_x_continuous(expand=c(0,0),breaks=c(.5,1,1.5))+
  scale_y_continuous(expand=c(0,0),breaks=c(.5,1))+
  theme(panel.border = element_blank())+
  #annotate('rect',#inherit.aes = F,
  #          xmin=1,xmax=1.5,ymin=-Inf,ymax=Inf,fill='gray',alpha=0.2)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_line(col='red')


# grabar
q = 2
ggsave(file=paste0('ventana_',nombre,'.pdf'),width=3,height=2,
       scale=q,bg='transparent',unit='cm')

#################################################################
# funcion de transferencia

# funcion k, vectorizada
k = function(u){
  sapply(u,function(tt) k.trans(tt) )
}

# grafica de k
u = seq(0,2.5*pi,by=1/500)
u = as.data.frame(u)
colnames(u) = c('xx')
u$yy = k(u$xx)

print(
ggplot(u,aes(x=xx,y=yy)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw()+
  theme(panel.background = element_rect(fill = 'transparent',colour = NA),
        plot.background = element_rect(fill = 'transparent',colour = NA))+
  expand_limits(y=1/pi) +
  scale_x_continuous(labels=math_format(.x * pi, format = function(x) x / pi),
                     expand=c(0,0),breaks=pi*c(1,2))+
  #scale_y_continuous(labels=math_format(.x * pi, format = function(x) x / pi),
  #                   expand=c(0,0),
  #                   breaks = (pretty_breaks(n=3)(u$yy/pi))*pi)+
  scale_y_continuous(labels=math_format(.x / pi, format = function(x) x * pi),
                     expand=c(0,0),
                     breaks = (1/pi)*c(0,.5,1))+
  theme(panel.border = element_blank())+
  #annotate('rect',#inherit.aes = F,
  #          xmin=1,xmax=1.5,ymin=-Inf,ymax=Inf,fill='gray',alpha=0.2)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_line(col='red')
)

# grabar
q = 2
ggsave(file=paste0('ventana_2_',nombre,'.pdf'),width=3,height=2,
       scale=q,bg='transparent',unit='cm')
