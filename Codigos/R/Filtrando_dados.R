###########################################################################################################################################
###########################################################################################################################################
#Leitura das bibliotecas necessárias
rm(list=ls())

ptm <<- proc.time()#captura o tempo de início do algoritmo


if(!require("fda"))  install.packages("fda", repos = "http://cran.us.r-project.org")
tryCatch(if(!require("rstudioapi"))  install.packages("rstudioapi", repos = "http://cran.us.r-project.org"),#Verificação se a biblioteca está instalada, caso não esteja é instalada automaticamente
         error=function(x, ...) {  a <- "Com Erro" ; return(a)} )
###########################################################################################################################################


###########################################################################################################################################
###########################################################################################################################################
# Definição do diretório base
Dir=tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(x, ...) {  a <- "Com Erro" ; return(a)} )
if(Dir=="Com Erro"){Dir <-getwd()}

file_p=gsub("/Codigos/R","",Dir)
###########################################################################################################################################


###########################################################################################################################################
###########################################################################################################################################
# Definição das variáveis de entrada
nclases=15;#Nº de padrões considerados, se NA cria o número automatico, proporcional um cluster por mês
nepocas=100;#Nº de iterações usadas no modelo se classificação de padrões
fator_loess=0.13

Dat_inicial=20200101
Dat_final  =20201231
###########################################################################################################################################

###########################################################################################################################################
###########################################################################################################################################
# Inicialização de variáveis secundarias
FontesDados=c("Fonte1","Fonte2","Fonte3")
arquivo=paste(file_p,"Dados de Entrada",sep="/")
setwd(arquivo)
Dad_Simul=read.csv2("Dados das Usinas.txt",header=FALSE, sep=";",comment.char = "#")
Dad_Simul=as.matrix(Dad_Simul)
Usi_consid=2
Dad_Simul=Dad_Simul[c(1,Usi_consid),]

Dad=as.matrix(Dad_Simul)
tam=dim(Dad)

Hora_ini_Prev=1

Usinas=Dad[2:tam[1],1]
Pinst=as.numeric(Dad[2:tam[1],2])
Ini_Oper=as.numeric(Dad[2:tam[1],3])
Plotar=as.numeric(Dad[2:tam[1],4])
Imprimir=as.numeric(Dad[2:tam[1],5])



arquivo=paste(file_p,"Dados de Entrada","Geração Verificada","Dados Originais",sep="/")
setwd(arquivo)
dados <- read.table(paste(Usinas[1],"_Ger_Verif_",FontesDados[1],".txt",sep = ""),header=FALSE, sep=";")


####################################################
####################################################
#Estrategia para eliminar um caractere indesejado que aparece na primeira na primeira data do arquivo
dat=as.matrix(dados[,1])
if(substr(dat[1],1,1)=="ï"){
  dat[1]=substr(dat[1],4,12)
}
dat=as.numeric(dat)
####################################################

DeH=which(dat==Dat_inicial)#pega a posição da data inicial do historico
AteH=which(dat==Dat_final)-1

###########################################################################################################################################
###########################################################################################################################################
# Inicio do processo de filtro por usinas

Nusinas=length(Usinas)
for (iusi in 1:Nusinas){
  if(Ini_Oper[iusi]>Dat_inicial){
    DeH=which(dat==Ini_Oper[iusi])#pega a posição da data inicial do historico
  }else{
    DeH=which(dat==Dat_inicial)#pega a posição da data inicial do historico
  }
  
  ############################################################################
  ############################################################################
  # Filtrando geração verificado
  
  ####################################################
  ####################################################
  # Leitura e preparação de dados
  arquivo=paste(file_p,"Dados de Entrada","Geração Verificada","Dado Pre Tratado",sep="/")
  setwd(arquivo)
  dad1_n <- read.table(paste(Usinas[iusi],"_Ger_Verif_",FontesDados[1],".txt",sep = ""),header=FALSE, sep=";")
  dad1_n=dad1_n[(DeH:(AteH)),]
  dad1_n[dad1_n==999]=NA
  Vdat=dad1_n[,1]
  dad1_n[,1]=NA
  dad1_n[dad1_n>Pinst[iusi]]=NA
  dad1_n[,1]=Vdat
  
  dad1=dad1_n
  dad1_orig=dad1_n
  
  arquivo=paste(file_p,"Dados de Entrada","Geração Verificada","Dados Originais",sep="/")
  setwd(arquivo)
  dad_PI_n <- read.table(paste(Usinas[iusi],"_Ger_Verif_",FontesDados[1],".txt",sep = ""),header=FALSE, sep=";")
  dad_PI_n=dad_PI_n[(DeH:(AteH)),]
  dad_PI=dad_PI_n
  
  dad_extra_n1 <- read.table(paste(Usinas[iusi],"_Ger_Verif_",FontesDados[2],".txt",sep = ""),header=FALSE, sep=";")
  dad_extra_n1=dad_extra_n1[(DeH:(AteH)),]
  dad_extra1=dad_extra_n1
  
  
  if(is.na(nclases)){
    nclases=round(nrow(dad_extra_n1)*(8/365),0)
  }
  if(nclases<2){nclases=2}
  ####################################################
  
  
  ####################################################
  ####################################################
  # Execução do filtro de correção de dados
  Tipo_var=1
  arquivo=paste(file_p,"Codigos/R","Filtro_Eolico.R",sep="/")
  source(arquivo)
  ####################################################
  
  ####################################################
  ####################################################
  # Escrita dos históricos corrigidos
  Ger_Filtrada=DadoFiltrado
  # Ger_Filtrada=Filtro(dad1_n,dad_PI_n,dad1,dad_PI,dad_extra1,nclases,nepocas,file_p,fator_loess,Plotar[iusi],Usinas[iusi],1,Pinst[iusi],Hora_ini_Prev)
  Ger_Filtrada_n=dad1_orig
  Ger_Filtrada_n[(DeH:(AteH)),]=Ger_Filtrada
  #escrevendo a tabela com os dados corrigidos
  if (Imprimir[iusi]==1){
    arquivo=paste(file_p,"Dados de Saida/Arquivos","Geração Verificada",paste(Usinas[iusi],"_Ger_Verif_Filtr_Consis.txt",sep=""),sep="/")
    write.table(Ger_Filtrada_n,  file.path(arquivo), sep=";",row.names = FALSE, col.names = FALSE)
  }
  ####################################################
  ############################################################################
  
  ############################################################################
  ############################################################################
  # Filtrando vento verificado
  
  ####################################################
  ####################################################
  # Leitura e preparação de dados
  arquivo=paste(file_p,"Dados de Entrada","Vento Verificado","Dado Pre Tratado",sep="/")
  setwd(arquivo)
  dad1_n <- read.table(paste(Usinas[iusi],"_Ven_Verif_",FontesDados[1],".txt",sep = ""),header=FALSE, sep=";")
  dad1_n=dad1_n[(DeH:(AteH)),]
  dad1_n[dad1_n==999]=NA
  Vdat=dad1_n[,1]
  dad1_n[,1]=NA
  dad1_n[dad1_n>20]=NA
  dad1_n[,1]=Vdat
  
  dad1=dad1_n
  dad1_orig=dad1_n
  
  arquivo=paste(file_p,"Dados de Entrada","Vento Verificado","Dados Originais",sep="/")
  setwd(arquivo)
  
  dad_PI_n <- read.table(paste(Usinas[iusi],"_Ven_Verif_",FontesDados[1],".txt",sep = ""),header=FALSE, sep=";")
  dad_PI_n=dad_PI_n[(DeH:(AteH)),]
  dad_PI=dad_PI_n
  
  dad_extra_n1 <- read.table(paste(Usinas[iusi],"_Ven_Verif_",FontesDados[3],".txt",sep = ""),header=FALSE, sep=";")
  dad_extra_n1=dad_extra_n1[(DeH:(AteH)),]
  dad_extra1=dad_extra_n1
  ####################################################
  
  ####################################################
  ####################################################
  # Execução do filtro de correção de dados
  Tipo_var=2
  arquivo=paste(file_p,"Codigos/R","Filtro_Eolico.R",sep="/")
  source(arquivo)
  ####################################################
  
  ####################################################
  ####################################################
  # Escrita dos históricos corrigidos
  Ven_Filtrada=DadoFiltrado
  Ven_Filtrada_n=dad1_orig
  Ven_Filtrada_n[(DeH:(AteH)),]=Ven_Filtrada
  #escrevendo a tabela com os dados corrigidos
  if (Imprimir[iusi]==1){
    arquivo=paste(file_p,"Dados de Saida/Arquivos","Vento Verificado",paste(Usinas[iusi],"_Ven_Verif_Filtr_Consis.txt",sep=""),sep="/")
    write.table(Ven_Filtrada_n,  file.path(arquivo), sep=";",row.names = FALSE, col.names = FALSE)
  }
  ####################################################
  ############################################################################
}
###########################################################################################################################################

###########################################################################################################################################
###########################################################################################################################################
# Impressão dos tempos de simulação
Tempo_proc=(proc.time() - ptm)/60
Tempo=Tempo_proc[1:3]
Dados_Simul=cbind("Tempo Comutacional gasto em min:",Tempo[3])
arquivo=paste(file_p,"Dados de Saida/Tempo computacional","Tempo de Simulação.txt",sep="/")
write.table(Dados_Simul,  file.path(arquivo), sep=";",row.names = FALSE, col.names = FALSE)
###########################################################################################################################################
