
atual<-c(700,rep(NA,29))
for (i in 2:length(atual)){
  desconto<-1.3-(0.04*i)
  atual[i]<- atual[i-1]*(desconto)
}

round(atual)


anteontem=977
ontem=1128
hoje = 1546

atual <-c(700, rep(NA,29))

for (i in 2:length(atual)){
  desconto<-1.3
  atual[i]<- atual[i-1]*(desconto)
}

round(atual)

plot(atual)