
# ===============================
# SKETCHING WITH SMALL MULTIPLES
# ===============================



library(HistData)
library(showtext)

# Data Processing

data(Armada)
Armada <- Armada[order(Armada$artillery),]

vars.int <- setdiff(names(Armada), c('Armada'))


# Normalisation [1,2]
for(i in vars.int){
  Armada[,paste(i,'_n', sep='')] <- (Armada[,i] - min(Armada[,i]))/
      (max(Armada[,i]) - min(Armada[,i])) + 1
  
}

# Ranking each variable
for(i in vars.int){
  Armada[,paste(i,'_r', sep='')] <- round(rank(Armada[,paste(i,'_n', sep='')]))
}


set.seed(1234) # to make it replicable

# Colour Selection

FTbg <- rgb( 254, 241, 224, maxColorValue =255 ) # background colour 
FTtx <- rgb( 114, 108, 97, maxColorValue =255 ) # text colour

#pal <- c("#3B9AB2","#56A6BA","#71B3C2","#9EBE91","#D1C74C","#E8C520","#E4B80E","#E29E00","#EA5C00","#F21A00")
#pal <- c("#DD8D29", "#DFAB16", "#E1CA04", "#AEC542", "#68B49B", "#69A39B", "#B09243", "#DF7804", "#C94312" ,"#B40F20")
#pal <- rev(c("#446455", "#818859" ,"#BFAD5D", "#FDD262", "#EFD58A", "#E1D9B3", "#D3DDDC", "#CFCEC6", "#CBBFB1", "#C7B19C"))
pal <- c("#EAD3BF", "#CDB7A5", "#B19B8C", "#AE8F73", "#B38859", "#9A7246", "#624D39", "#352E2C", "#282222", "#1C1718")

showtext_auto()

font_add_google('Indie Flower', 'indie')
font_add_google('Gochi Hand', 'gochi')

x11()
back <- '#FAEBD7'

par( bg = back,
     mar=c(0.1,0.1,0.1,0.1),  # the plot margins 
     oma=c( 15,0,0,0 ) )      # outer marginsbottom


# Colour Palette

top <-14




plot(-99,-99, xlim=c(0,29), ylim=c(0,top), ann=F, axes=F, type='n', xaxs='i', yaxs='i')


for(i in seq(0,nrow(Armada)-1,1)){
  
  polygon(c(i*3,i*3+runif(1,0.5,1.5),2+i*3), 
          c(0,Armada$ships_n[i+1],0),
          col = pal[Armada$ships_r[i+1]], border = NA)
  extra <- Armada$ships_n[i+1]
  polygon(c(i*3,i*3+runif(1,0.5,1.5),2+i*3),
          c(extra,Armada$soldiers_n[i+1]+extra,extra),
          col = pal[Armada$soldiers_r[i+1]], border = NA)
  extra<- extra+Armada$soldiers_n[i+1]
  polygon(c(i*3,i*3+runif(1,0.5,1.5),2+i*3),
          c(extra,Armada$sailors_n[i+1]+extra,extra),
          col = pal[Armada$sailors_r[i+1]], border = NA)
  
}


war <- c('artillery_n','balls_n','gunpowder_n','lead_n','rope_n')
war.r <- c('artillery_r','balls_r','gunpowder_r','lead_r','rope_r')

for(i in seq(0,nrow(Armada)-1,1)){
  
  polygon(c(i*3,i*3+runif(1,0.5,1.5),2+i*3), 
          c(top,top-Armada[i+1,war[1]],top),
          col = pal[Armada[i+1,war.r[1]]], border = NA)
  extra <- Armada[i+1,war[1]]
  polygon(c(i*3,i*3+runif(1,0.5,1.5),2+i*3),
          c(top-extra,top-extra-Armada[i+1,war[2]],top-extra),
          col = pal[Armada[i+1,war.r[2]]], border = NA)
  extra <- extra+Armada[i+1,war[2]]
  polygon(c(i*3,i*3+runif(1,0.5,1.5),2+i*3),
          c(top-extra,top-extra-Armada[i+1,war[3]],top-extra),
          col = pal[Armada[i+1,war.r[3]]], border = NA)
  extra <- extra+Armada[i+1,war[3]]
  polygon(c(i*3,i*3+runif(1,0.5,1.5),2+i*3),
          c(top-extra,top-extra-Armada[i+1,war[4]],top-extra),
          col = pal[Armada[i+1,war.r[4]]], border = NA)
  
}
op <- par(family = "indie")
axis(side=1, seq(1,28,3),labels=c('N','Ga','U','A','P','Vi','Gu','C','P','Vr'), col=NA)



# =============
#   SUBTITLES
# =============

op <- par(family = "gochi") 
title( sub = "Sketching with Small Multiples" , 
       cex.sub =2,
       outer=T, adj=0, line=3)

title( sub = "La Gran y Felicisima Armada" , 
       cex.sub =1,
       outer=T, adj=0, line=4.2)



# ===========
#   LEGENDS
# ===========


op <- par(family = "indie")


legend(-1,-3.7, bty='n', xpd=NA, title='Fleet:',
       legend=c('N:  Napoles','Ga: Galeras','U:  Uantiscas','A:  Andalucia','P:  Portugal'),
       cex=1.3)
legend(5,-4.6, bty='n', xpd=NA,
       legend=c('Vi: Vizca','Gu: Guipuscua','C:  Castilla','P:  Pataches','Vr: Vrcas'),
       cex=1.3)


# All of these could be inserted in a loop... sorry to leave it explicit

# COLOR AND SIZE LEGEND

x <- 13.8
y <- -3.7

legend(x,y, bty='n', xpd=NA, title='Ranking*:',
       legend=rev(c('','','','','')), 
       col=rev(pal[6:10]),
       pch=c(NA,NA,NA,NA,NA), cex=1.3)
legend(x,y, bty='n', xpd=NA, title='',
       legend=rev(c('6th','','','','')), 
       col=rev(pal[6:10]),
       pch=c(NA,NA,NA,NA,17), cex=1.3, pt.cex=1.99)
legend(x,y, bty='n', xpd=NA, title='',
       legend=rev(c('','7th','','','')), 
       col=rev(pal[6:10]),
       pch=c(NA,NA,NA,17,NA), cex=1.3, pt.cex=2.09)
legend(x,y, bty='n', xpd=NA, title='',
       legend=rev(c('','','8th','','')), 
       col=rev(pal[6:10]),
       pch=c(NA,NA,17,NA,NA), cex=1.3, pt.cex=2.19)
legend(x,y, bty='n', xpd=NA, title='',
       legend=rev(c('','','','9th','')), 
       col=rev(pal[6:10]),
       pch=c(NA,17,NA,NA,NA), cex=1.3, pt.cex=2.29)
legend(x,y, bty='n', xpd=NA, title='',
       legend=rev(c('','','','','Max')), 
       col=rev(pal[6:10]),
       pch=c(17,NA,NA,NA,NA), cex=1.3, pt.cex=2.39)

# min - 5

x <- 17.9
y <- -4.6

legend(x,y, bty='n', xpd=NA,
       legend=rev(c('Min','','','','')), 
       col=rev(pal[1:5]),
       pch=c(NA,NA,NA,NA,17), cex=1.3, pt.cex=1.49)
legend(x,y, bty='n', xpd=NA,
       legend=rev(c('','2nd','','','')), 
       col=rev(pal[1:5]),
       pch=c(NA,NA,NA,17,NA), cex=1.3, pt.cex=1.59)
legend(x,y, bty='n', xpd=NA,
       legend=rev(c('','','3th','','')), 
       col=rev(pal[1:5]),
       pch=c(NA,NA,17,NA,NA), cex=1.3, pt.cex=1.69)
legend(x,y, bty='n', xpd=NA,
       legend=rev(c('','','','4th','')), 
       col=rev(pal[1:5]),
       pch=c(NA,17,NA,NA,NA), cex=1.3, pt.cex=1.79)
legend(x,y, bty='n', xpd=NA,
       legend=rev(c('','','','','5th')), 
       col=rev(pal[1:5]),
       pch=c(17,NA,NA,NA,NA), cex=1.3, pt.cex=1.89)


# VERTICAL DISTRIBUTION EXPLANATION

legend(22,-2.2, bty='n', xpd=NA, title='Feature Distribution:',
       legend=c("Artillery","Balls","Gunpowder","Lead","Rope"," ", "Sailor","Soldier","Ships"),
       pch=c(6,6,6,6,6,NA,2,2,2),
       cex=1)

# Some further notes

title( sub = "*Rankings were obtained by normalising each feature to values within [1,2]", 
       col = FTbg , 
       cex.sub=0.8,
       outer=T, adj=0, line=13.8)

title( sub = "Source: HistData", 
       col = FTbg , 
       cex.sub=0.8,
       outer=T, adj=1,line=14)

dev.off()

