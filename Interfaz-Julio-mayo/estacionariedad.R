#
# para vc, EMG es una copia de LOG
#
estacionariedad <- function(data_dir, central_dir, result_dir, nom_dir, nom_arch, nom_facil, grupo_de, frecuenciasss, sizeEpoc){
  setwd(central_dir)
  
  source('multipsr08.R' ) 
  source('colorcitos_usable02.R')
  
  
  for(sujeto in 1:length(nom_dir)){
    print(sujeto)
    nombre   = nom_arch[sujeto]
    print(nombre)
    work_dir = paste0(data_dir,nom_dir[sujeto])
    print(work_dir)
    res_dir  = paste0(result_dir)
    print(res_dir)
    
    setwd(work_dir)
    
    w_dir = work_dir
    ch = 1
    z=1
    
    for(z in 1:22){
      multipsr(z,nombre,work_dir,central_dir,res_dir,
               frecuenciasss[sujeto],sizeEpoc)
      # valores de interes: 30,15,7.5,3.75,7.875,0.9375, 60,120
    }
    im <- colorcitos(sujeto, frecuenciasss, nom_arch, nom_facil, nom_dir, result_dir, sizeEpoc)
  }
  
}
