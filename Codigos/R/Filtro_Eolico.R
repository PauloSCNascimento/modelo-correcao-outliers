#########################################################################################################################################
#     Inicio do codigo para correção de missings e outliers
########################################################################################################################################

#########################################################################################################################################
##############################################################################################################################################
#Gera gráficos comparativos com todas as curvas antes e após a correção
vPlotar_graficos_Tot<-function(dad_PI,corrigido,dia_inicial,dia_final,vPinst,Tipo_var){
  
  cores=colors()
  
  ############################################################################
  ############################################################################
  # Gráficos de geração verificada
  if(Tipo_var==1){
    arquivo=paste(file_p,"Dados de Saida/Graficos/Geração Verificada",sep="/")
    setwd(arquivo)
    jpeg(filename=paste(vUsina,"_Ger_Verif_Filtr_Total.jpeg",sep=""),width = 1200, height = 700, units = "px", pointsize = 14,quality = 75)
   
    layout(matrix(c(1,2,3,4,5,6,7,8,9,10),2,1,byrow = TRUE),  TRUE)
    x=c(0,max(Per))
    y=c(0,vPinst)
    plot(x, y, type="n", xlab="Período 30m-30m",col=2,ylab="Potência [MW]",cex.axis = 1.5,cex.lab = 1.6)
    for (idia in dia_inicial:dia_final){
      lines(1:48,dad_PI[idia,2:49], type="l", col=cores[round(runif(1,10,650))],lwd=1.5)
    }
    title(toString(paste("Dado original"),cex.main = 2.2))
    plot(x, y, type="n", xlab="Período 30m-30m",col=2,ylab="Potência [MW]",cex.axis = 1.5,cex.lab = 1.6)
    for (idia in dia_inicial:dia_final){
      lines(1:48,corrigido[idia,2:49], type="l", col=cores[round(runif(1,10,650))],lwd=1.5)
    }
    title(toString(paste("Dado Filtrado"),cex.main = 2.2))
    
    dev.off()
  }
  ############################################################################
  
  ############################################################################
  ############################################################################
  # Gráficos de vento verificado
  if(Tipo_var==2){
    arquivo=paste(file_p,"Dados de Saida/Graficos/Vento Verificado",sep="/")
    setwd(arquivo)
    jpeg(filename=paste(vUsina,"_Ven_Verif_Filtr_Total.jpeg",sep=""),width = 1200, height = 700, units = "px", pointsize = 14,quality = 75)
    
    layout(matrix(c(1,2,3,4,5,6,7,8,9,10),2,1,byrow = TRUE),  TRUE)
    x=c(0,max(Per))
    y=c(0,22)
    plot(x, y, type="n", xlab="Período 30m-30m",col=2,ylab="Vento [m/s]",cex.axis = 1.5,cex.lab = 1.6)
    for (idia in dia_inicial:dia_final){
      lines(1:48,dad_PI[idia,2:49], type="l", col=cores[round(runif(1,10,650))],lwd=1.5)
    }
    title(toString(paste("Dado original"),cex.main = 2.2))
    plot(x, y, type="n", xlab="Período 30m-30m",col=2,ylab="Vento [m/s]",cex.axis = 1.5,cex.lab = 1.6)
    for (idia in dia_inicial:dia_final){
      lines(1:48,corrigido[idia,2:49], type="l", col=cores[round(runif(1,10,650))],lwd=1.5)
    }
    title(toString(paste("Dado Filtrado"),cex.main = 2.2))
    
    dev.off()
  }
  ############################################################################
  return(0)
}
##############################################################################################################################################


#########################################################################################################################################
##############################################################################################################################################
#Gera gráficos comparativos com os resultados diários da correção
vPlotar_graficos<-function(DadSoutlier,dad_pre,dad_filtr,dad_PI,padrao,dad_extra1,file_p,vUsina,Tipo_var,vPinst){

    tam=length(dad_pre)
  Per=1:(tam-1)
  Dat=dad_pre[1]
  dad_pre=dad_pre[2:49]
  
  
  ############################################################################
  ############################################################################
  # Gráficos de geração verificada
  if(Tipo_var==1){
    
    arquivo=paste(file_p,"Dados de Saida/Graficos/Geração Verificada",sep="/")
    setwd(arquivo)
    jpeg(filename=paste(vUsina,"_",Dat,"_Ger_Verif_Filtr.jpeg",sep=""),width = 900, height = 700, units = "px", pointsize = 14,quality = 75)
    layout(matrix(c(1,2,3,4,5,6,7,8,9,10),1,1,byrow = TRUE),  TRUE)
    
    x=c(0,max(Per))
    # y=c(0,max(cbind(max(dad_pre[which(dad_pre!='999')]),max(dad_filtr[which(dad_filtr!='999')]),max(dad_PI[which(dad_PI<150)])),max(padrao)))  
    y=c(0,vPinst)
    plot(x, y, type="n", xlab="Período 30m-30m",col=2,ylab="Potência [MW]",cex.axis = 1.5,cex.lab = 1.6)
    lines(Per,DadSoutlier, type="l", col="purple",lwd=16)
    lines(Per,dad_pre, type="l", col="cyan",lwd=11)
    lines(Per,dad_PI, type="l", col="red",lwd=7)
    lines(Per,dad_extra1, type="l", col="blue",lwd=2.5)
    lines(Per,dad_filtr, type="l", col="black",lwd=2.5)
    lines(Per,padrao, type="l", col="green",lwd=2)
    title(toString(paste("Geração usina teste Nº",iusi," \n",substr(Dat, 7, 8),"/",substr(Dat, 5, 6),"/",substr(Dat, 0, 4))),cex.main = 2.2)
    legend("topleft", c("Sem outlier",paste0(FontesDados[1]," Pré-Trat"),paste0(FontesDados[1]," Orig"),FontesDados[2],"Filtrado","Padrão"), col = c("purple","cyan","red","blue","black","green"),
           text.col = "black", lty = c(1, 1, 1),
           merge = TRUE,lwd=3,cex = 2.2)
    dev.off()
  }
  ############################################################################
  
  ############################################################################
  ############################################################################
  # Gráficos de vento verificado
  if(Tipo_var==2){
    arquivo=paste(file_p,"Dados de Saida/Graficos/Vento Verificado",sep="/")
    setwd(arquivo)
    jpeg(filename=paste(vUsina,"_",Dat,"_Ven_Verif_Filtr.jpeg",sep=""),width = 900, height = 700, units = "px", pointsize = 14,quality = 75)
    layout(matrix(c(1,2,3,4,5,6,7,8,9,10),1,1,byrow = TRUE),  TRUE)
    
    x=c(0,max(Per))
    # y=c(0,max(cbind(max(dad_pre[which(dad_pre!='999')]),max(dad_filtr[which(dad_filtr!='999')]),max(dad_PI[which(dad_PI<150)])),max(padrao)))          
    y=c(0,22)
    plot(x, y, type="n", xlab="Período 30m-30m",col=2,ylab="Velocidade [m/s]",cex.axis = 1.5,cex.lab = 1.6)
    lines(Per,DadSoutlier, type="l", col="purple",lwd=16)
        lines(Per,dad_pre, type="l", col="cyan",lwd=11)
    lines(Per,dad_PI, type="l", col="red",lwd=7)
    lines(Per,dad_extra1, type="l", col="blue",lwd=2.5)
    lines(Per,dad_filtr, type="l", col="black",lwd=3.5)
    lines(Per,padrao, type="l", col="green",lwd=2)
    title(toString(paste("Vento usina teste Nº",iusi," \n",substr(Dat, 7, 8),"/",substr(Dat, 5, 6),"/",substr(Dat, 0, 4))),cex.main = 2.2)
    legend("topleft", c("Sem outlier",paste0(FontesDados[1]," Pré-Trat"),paste0(FontesDados[1]," Orig"),FontesDados[3],"Filtrado","Padrão"), col = c("purple","cyan","red","blue","black","green"),
           text.col = "black", lty = c(1, 1, 1),
           merge = TRUE,lwd=3,cex = 2.2)
    dev.off()
  }
  ############################################################################
  return(0)
}
##############################################################################################################################################

#########################################################################################################################################
#########################################################################################################################################
# Filtro de nível e tendência local para preenchimento de dados faltantes  
Filtro_Dinamico<-function(ret,dad_filtar,fator_loess){#Metodo para filtragem de serie com base em Filtro de Kalman
  ############################################################################
  ############################################################################
  # Identifica grupo para inicializacao das variaveis do modelo atraves da menor distancia euclidiana
  Padroes=ret[[1]]
  devPadroes=ret[[2]]
  tam=dim(Padroes)
  ntempos=tam[1]
  nclases=tam[2]
  Per=1:length(dad_filtar)
  Padroes=cbind(Padroes,dad_filtar);
  nclases=nclases-1
  
  ####################################################
  ####################################################
  # Escolha do padrão associado a série a filtrar
  erro=matrix(0, nrow = 1, ncol = tam[1])
  dg=matrix(0, nrow = 1, ncol = 2)
  for (ic in 1:nclases){
    for(it in 1:tam[1]){
      if(!is.na(Padroes[it,(nclases+2)])){
        erro[it]=Padroes[it,(nclases+2)]-Padroes[it,(ic+1)]#erro do padrão para a serie a filtrar
      }
      dg[ic]=sqrt(sum((erro)^2))#distancia euclidiana
    }
  }
  
  dm = min(dg);
  pos=which(dg==dm)
  col=pos+1;
  
  dsa    = Padroes[,nclases+2]; #backup vetor de dados
  padrao = Padroes[,col]; #padrao identificado
  ####################################################
  ############################################################################
  
  ############################################################################
  ############################################################################
  #identifica outliers no dia a ser corrigido
  ds= rep(0,ntempos); #inicializa vetor de Padroes

  ####################################################
  ####################################################
  # Cria fator que ajusta nível do padrao referente ao dia a ser corrigido
  pos=which(dad_filtar!='999'|!is.na(dad_filtar))
  M_dado=mean(dad_filtar[pos])
  M_padrao = mean(padrao)
  if(M_padrao==0){M_padrao=0.001}
  fator=M_dado/M_padrao
  ####################################################
  
  ####################################################
  ####################################################
  # Cria limites inferiores para identificação de outliers
  if(fator!=0){
    padrao = padrao * fator;
    lm.D9 <- lm(dsa ~ padrao)
    stats <- 1 - (sum((dsa-predict(lm.D9))^2,na.rm=T)/sum((dsa-mean(dsa,na.rm=T))^2,na.rm=T))#Pega o R² para analise da qualidade do ajuste
    #stats=summary(lm.D9)$r.squared
    
    # Ajusta Reta 45º
    err_reta     = rep(0,ntempos);
    # reta_scatter = [ ];
    for (t in 1:ntempos){
      if(!is.na(dsa[t])){
        err_reta[t] = dsa[t]-padrao[t];
      }
    }
    desv_reta = sd(err_reta);

    if (stats <= 0.7){
      m_qtddesv = 1;}
    if ((stats > 0.7)&&(stats <= 0.8)){
      m_qtddesv = 2;}
    if ((stats > 0.8)&&(stats <= 0.9)){
      m_qtddesv = 3;}
    if (stats > 0.90){
      m_qtddesv = 4;}  
    
    m_qtddesv=2*m_qtddesv+0.#ajuste pra eliminar mais ou menos outliers
    
    m_li      = rep(1,ntempos)*(-desv_reta)*m_qtddesv;
    m_ls      = rep(1,ntempos)*desv_reta*m_qtddesv;
    
    # minimo=min(c(err_reta,m_li,m_ls))
    # maximo=max(c(err_reta,m_li,m_ls))
    # plot(c(1,ntempos), c(minimo,maximo), type="p", xlab="Período",col="blue",ylab="Erro" )
    # points(1:ntempos, err_reta,col="blue")
    # lines(1:ntempos,m_li, type="l", col="green",lwd=2.5)
    # lines(1:ntempos,m_ls, type="l", col="red",lwd=2.5)
    # title(toString("Processo de Identificação de Dados Aberrantes"),cex.main = 1.5)
    # legend("topleft", c("Lim Sup","Lim Inf", "Erro"), col = c("red","green","blue"),text.col = "black", lty = c(1, 1, 1), merge = TRUE,cex = 1.6)
    # 
    # # plot regressao
    # plot(c(0,padrao), c(0,dsa), type="p", xlab="Valor",col="red",ylab="Valor",lwd=2)
    # lines(padrao,padrao, type="l", col="blue",lwd=4)
    # lines(padrao,lm.D9$coefficients[2]*padrao+lm.D9$coefficients[1],lwd=4,type='l',col="black")
    # legend("topleft", c("Dado Pré-Trat","Padrão","Regressão"), col = c("red","blue","black"),text.col = "black", lty = c(1, 1, 1), merge = TRUE,cex = 1.6)
    # title(toString("Ajuste da Reta"),cex.main = 1.5)
    # 
    # plot(1:48,dsa,col='red',type='l',xlab="Período",ylab="Valor",lwd=2)
    # lines(1:48,padrao,col='blue',lwd=2)
    # lines(1:48,lm.D9$coefficients[2]*padrao+lm.D9$coefficients[1],col='black',lwd=2)
    # title(toString("Ajuste da regressão"),cex.main = 1.5)
    # legend("topleft", c("Dado Pré-Tra","Padrão","Regressão"), col = c("red","blue","black"),text.col = "black", lty = c(1, 1, 1), merge = TRUE,cex = 1.6)
    
    # %identifica outliers transformando-os em missings
    for (t in 1:ntempos){
      ds[t] = dad_filtar[t];
      check  = (err_reta[t] >= m_li[t]) & (err_reta[t] <= m_ls[t]);
      if (check==FALSE){
        ds[t] = NA;
      }
    }
  }else{
    ds[1:ntempos]=NA
  }
  
  DadSoutlier=ds
  ####################################################
  ############################################################################
  
  ############################################################################
  ############################################################################
  # Executa o modelo do Filtro_Dinamico de nível e tendencia local
  
  ####################################################
  ####################################################
  # Inicializa variaveis
  fatdesc = 20;
  var0    = mean(devPadroes[,col])*2;
  
  F  = 1
  G  = cbind(c(1,0),c(0,1))
  V  = matrix(1, nrow = 2, ncol = 2)
  W  = cbind(c(var0,0),c(0,var0/fatdesc)) 
  f1  = rep(0,ntempos);
  
  # posteriori at t-1
  b0     = padrao[2]-padrao[1];
  n0     = padrao[1];
  C0     = W;
  m0     = cbind(c(n0, b0))
  d_prob = 0.5;
  ####################################################
  
  ####################################################
  ####################################################
  # Início de processo iterativo de Kalmam
  for (t in 1:ntempos){
    # captura prob. do dado anterior em relacao ao IC do padrao
    m_media = padrao[t];
    m_desv  = devPadroes[t,col];
    # x=rnorm(100000)
    # curve(pnorm(x,m_media,m_desv),-0,12,col="red")
    if (!is.na(ds[t])){
      d_prob = pnorm(ds[t],m_media,m_desv);
    }
    
    
    # priori at t-1
    a    = G %*% m0;
    R    = G %*% C0 %*% t(G) + W;
    
    # previsão 1-step
    f1[t] = ((F)%*%rep(1,2)) %*% a;
    Q1    = (F) * R * F + V;
    
    # posteriori at t
    A    =  F*R%*% ginv(Q1);
    if (is.na(ds[t])){
      e=qnorm(d_prob,m_media,m_desv)- f1[t]
    }else{
      e = ds[t] - f1[t];
    }
    
    # ajusta matrizes para o proximo passo
    t1=matrix(1,2,1)
    m    = a + A%*%(e*t1);
    C    = R - A %*% Q1 %*% t(A);
    m0   = m;
    C0   = C;
    
    # points(t,f[t], col="green",lwd=8.5)
  }
  ####################################################
  
  ####################################################
  ####################################################
  # %substitui missings por previsao do modelo Filtro_Dinamico
  for (t in 1:ntempos){
    if (is.na(ds[t])){
      ds[t] = f1[t];
    }}
  ds_cor=matrix(0,nrow = 1,ncol = 49)
  ds_cor[1,2:49]=ds
  ####################################################
  
  ####################################################
  ####################################################
  # Suavização dos valores corrigidos pelo filtro de loess
  ds_cor=Filtro_loess(ds_cor,fator_loess,dias_Filtar)
  
  # plot(2:49,ds[1:48], xlab="Período",col="blue",ylab="dado",lwd=2)
  # lines(2:49,ds[1:48],col="blue",ylab="dado",lwd=2)
  # ds_cor[1,2:49]=ds;ds_cor=Filtro_loess(ds_cor,0.9,dias_Filtar)
  # lines(2:49,ds_cor[2:49],col="black",ylab="dado",lwd=2)
  # ds_cor[1,2:49]=ds;ds_cor=Filtro_loess(ds_cor,0.5,dias_Filtar)
  # lines(2:49,ds_cor[2:49],col="green",ylab="dado",lwd=2)
  # ds_cor[1,2:49]=ds;ds_cor=Filtro_loess(ds_cor,0.13,dias_Filtar)
  # lines(2:49,ds_cor[2:49],col="red",ylab="dado",lwd=2)
  # legend("topleft", c("Dado Original","a = 0.9","a = 0.5", "a = 0.13"), col = c("blue","black","green","red"),
  #        text.col = "black",
  #        merge = TRUE,lwd=3,cex = 1)
  # title(toString(paste("Suavização loess"),cex.main = 2.2))
  ####################################################
  
  
  ds_corrig=ds_cor[1,2:49]
  ds_corrig[ds_corrig<0]=0#Elmina valores mal estimados
  ############################################################################
  
  return(list(ds_corrig,padrao,DadSoutlier))
} 
#########################################################################################################################################

#########################################################################################################################################
#########################################################################################################################################
# Elimina padrões poucos representativos
Elimina_Padroes<-function(ret,nclases){
  Padroes=ret[[1]]
  devPadroes=ret[[2]]
  Clas_series=ret[[3]]
  Pad_novo=Padroes[,1]
  devPadr_novo=devPadroes[,1]
  nclases_novo=nclases
  tot=length(Clas_series)/nclases_novo
  
  ####################################################
  ####################################################
  #elimina padroes com um nº de series muito pequenas pertecentes a ele
  for (iclas in 1:nclases){
    to=length(which(Clas_series==iclas))
    perc=to/tot*100
    if (perc>10){
      Pad_novo=cbind(Pad_novo,Padroes[,iclas+1])
      devPadr_novo=cbind(devPadr_novo,devPadroes[,iclas+1])
    }else{
      nclases_novo=nclases_novo-1
    }
  }
  ####################################################
  ret=list(Pad_novo, devPadr_novo,nclases_novo)
  return(ret)
}
#########################################################################################################################################

#########################################################################################################################################
#########################################################################################################################################
# Filtro de suavização
Filtro_loess<-function(dados,ordem,dias_Filtar){
  tam=dim(dados)
  ndias=tam[1]
  Per=tam[2]-1
  x <- 1:Per
  
  for (idia in 1:ndias){
    
    y <- as.numeric(dados[idia,2:(Per+1)])
    
    # Aplica a suavização de loess
    yloess <- loess(y ~ x, span=ordem, data.frame(x=x, y=y))
    
    #  Cálculo dos valores suavizados pelo loess para todos os pontos ao longo da curva
    ypredict <- predict(yloess, data.frame(x=x))
    
    dados[idia,2:(Per+1)]=ypredict
  }
  return(dados)
}
#########################################################################################################################################

trunc.dig <- function(x, digits){
  return(trunc(x*10^digits)/10^digits)
} 

#########################################################################################################################################
# Inicio da montagem dos dados para posteriormente usar as funçoes
#########################################################################################################################################

############################################################################
############################################################################
# Inicialização de variáveis
vPlotar=Plotar[iusi]
vUsina=Usinas[iusi]
vPinst=Pinst[iusi]

tam1=dim(dad1_n)


dad1_n=as.matrix(dad1_n)
class(dad1_n)<-"numeric"

dad1=as.matrix(dad1)
class(dad1)<-"numeric"

tam=dim(dad1)



intervalo=tam[2]-1;#intervalo considerado (24h correspondem a 48 valores de 30 em 30m)
dia_final=tam[1]#dia final que será filtrado das series passadas
dia_inicial=1#dia inicial que será filtrado das series passadas

dad1_ns999=dad1_n
dad1_ns999[dad1_ns999==999|is.na(dad1_ns999)]=0
lim=c(0,max(dad1_ns999[1:(tam1[1]-1),2:(intervalo+1)]))
############################################################################

############################################################################
############################################################################
#realização da classificação dos padrões requeridos
arquivo=paste(file_p,"Codigos/R","Classificacao.R",sep="/")
for(i in 1:20){
  Cl=tryCatch(source(arquivo),error=function(x, ...) {  a <- "Com Erro" ; return(a)} )
  if(Cl!="Com Erro"){
    break
  }
  if(i==20){
    Erro=Com_Erro#Variável não existente, sempre quando o processo de classificação não funcionar
  }
}
############################################################################

############################################################################
############################################################################
#eliminação de padrões muito atípicos do conjunto de padrões
par(mfrow=c(1,1))
ret=Elimina_Padroes(ret,nclases)
############################################################################

############################################################################
############################################################################
# correção de missing e outlier
corrigido=matrix(NA, nrow = dia_final, ncol = 49)
Dat=dad1[,1]
PadrAnt=ret[[1]][,2]
for (idia in dia_inicial:dia_final){
  # idia=31
  dad_filtr=dad1[idia,2:49]
  dad_filtr_Orig=dad_filtr
  pos=which(!is.na(dad_filtr))
  if(length(pos)<5){
    dad_filtr=PadrAnt
  }
  
  ####################################################
  ####################################################
  # Aplicação do filtro
  cor=Filtro_Dinamico(ret,dad_filtr,fator_loess)
  ####################################################
  
  dad_pre=dad1[idia,1:49]
  padrao=cor[[2]]
  PadrAnt=padrao
  DadSoutlier=cor[[3]]
  
  if((idia==dia_final)&&((Tipo_var==1)||(Tipo_var==2))){
    corrigido[idia,2:(2*Hora_ini_Prev+1)]=cor[[1]][1:(2*Hora_ini_Prev)]
  }else{
    corrigido[idia,2:49]=cor[[1]]
  }
  
  corrigido[idia,1]=dad1[idia,1]
  
  ####################################################
  ####################################################
  # Geração de gráficos
  if (vPlotar==1){
    dad_filtr=dad_filtr_Orig
    vPlotar_graficos(DadSoutlier,dad_pre,corrigido[idia,2:49],dad_PI[idia,2:49],padrao,dad_extra1[idia,2:49],file_p,vUsina,Tipo_var,vPinst)
  }
  ####################################################
}
DadoFiltrado=corrigido
############################################################################

############################################################################
############################################################################
# Gera gráficos com todas as curvas 
vPlotar_graficos_Tot(dad_PI,corrigido,dia_inicial,dia_final,vPinst,Tipo_var)
############################################################################