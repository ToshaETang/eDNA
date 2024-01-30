
library(ggplot2)
library(vegan)
library(ggforce)

# read data
data_LAN <- read.csv("binary_matrix_2024-01-30.csv", header = T, row.names = 1)

# do NMDS
dfNmds<-metaMDS(data_LAN,distance="bray",k = 2, trymax = 100)

# form the data
data = data.frame(dfNmds$points)


 # plot
 ggplot(data,aes(x = MDS1,y = MDS2,))+
   geom_point(size=2)+
   theme_classic()+
   geom_text(                # add label
     aes(label=rownames(data)),
     vjust=1.5,
     size=3,
     color = "black"
   )+
   labs(                     # add stress
     subtitle = paste("stress=",round(dfNmds$stress,3),sep="")
   )

 