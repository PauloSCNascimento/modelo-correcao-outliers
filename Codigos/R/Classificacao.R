############################################################################
############################################################################
# Classificação
dados=dad1_n[1:(tam1[1]-1),]

tam=dim(dados)
dias=tam[1]
Per=tam[2]-1
dados2=dados[,2:(Per+1)];
p=dados2;

pos=which(is.na(p)|p==999,arr.ind = T)
p1=pos[!duplicated(pos[,1]),1]
p[p1,]=0
clusterizar=p

####################################################
####################################################
# Classificação K-means
fit.km <- kmeans(clusterizar, nclases, nstart=25)#realiza a classificação dos padrões
centroides=fit.km$centers# vetor com os padroes classificados
O_clusteres=fit.km$cluster#vetor com a identificação (qual padrão a serie pertence)
####################################################

xrange <- c(0,Per)
yrange <- c(0,lim[2])
x=1:Per
Padroes=matrix(0, nrow = Per, ncol = (nclases+1))
Padroes[,1]=1:Per
des=0
stdv=0
############################################################################

############################################################################
############################################################################
#plot padroes e series pertecentes
for (ic in 1:nclases){
  ####################################################
  ####################################################
  # Gera gráficos
  Padroes[,ic+1]=t(centroides[ic,])
  if (vPlotar==1){
    plot(xrange, yrange, type="n", xlab="Período[30-30m]",ylab="Dado" )
    colors <- rainbow(dias) 
    linetype <- c(1:dias) 
    plotchar <- seq(18,18+dias,1)
    y=centroides[ic,]
    lines(x,y, type="l", lwd=8.5,col=colors[ic], pch=plotchar[ic]) 
  }
  for ( idia in 1:dias){
    if (ic==O_clusteres[idia]){
      y=p[idia,1:Per]
      if (vPlotar==1){
        tree <- subset(Orange, Tree==idia) 
        lines(x,y, type="l", lwd=1.5, col=colors[idia], pch=plotchar[idia])
      }
      des=cbind(des,y)
    }
  }
  ####################################################
  
  ####################################################
  ####################################################
  # Salva gráficos de geração
  if(Tipo_var==1){
    title(toString(paste("Classe ",ic,"para Ger. da",vUsina)),cex.main = 1.5)
    arquivo=paste(file_p,"Dados de Saida/Graficos/Padrões/Geração Verificada",sep="/")
    setwd(arquivo)
    dev.copy(device = jpeg, file = paste(vUsina,"_Classe_",ic,"Ger.jpeg",sep=""), width = 700, height = 700, res = 100)
    dev.off()
  }
  ####################################################
  
  ####################################################
  ####################################################
  # Salva gráficos de vento verificado
  if(Tipo_var==2){
    title(toString(paste("Classe ",ic,"para Vento da",vUsina)),cex.main = 1.5)
    arquivo=paste(file_p,"Dados de Saida/Graficos/Padrões/Vento Verificado",sep="/")
    setwd(arquivo)
    dev.copy(device = jpeg, file = paste(vUsina,"_Classe_",ic,"Vento.jpeg",sep=""), width = 700, height = 700, res = 100)
    dev.off()
  } 
  ####################################################
  stdv=cbind(stdv,apply(des[,-1],1, sd))
}
############################################################################

ret=list(Padroes, stdv,O_clusteres)



############################################################################
############################################################################
# # Plota todos os padrões em um grafico
# plot(xrange, yrange, type="n", xlab="Período[30-30m]",ylab="Dado" )
# 
#   for ( idia in 1:dias){
#       y=dad1_n[idia,1:Per]
#         tree <- subset(Orange, Tree==idia) 
#         lines(x,y, type="l", lwd=1.5, col=colors[idia])
#   }
# 
# for (ic in 1:nclases){
#   Padroes[,ic+1]=t(centroides[ic,])
#   if (vPlotar==1){
#     colors <- rainbow(dias) 
#     linetype <- c(1:dias) 
#     plotchar <- seq(18,18+dias,1)
#     y=centroides[ic,]
#     lines(x,y, type="l", lwd=8.5,col=colors[20*ic-10], pch=plotchar[ic]) 
#   }
# }
# title(toString(paste("Reconhecimento de padrões")),cex.main = 1.5)
#############################################################################