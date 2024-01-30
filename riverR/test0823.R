
library(ggplot2)
library(vegan)
library(ggforce)

# read data
data_LAN <- read.csv("data1106.csv", header = T, row.names = 1)
#dfGroup <- read.csv("group.csv", header = T, row.names = 1)

# do NMDS
dfNmds<-metaMDS(data_LAN,distance="bray",k = 2, trymax = 100)

# form the data
data = data.frame(dfNmds$points)
#data$group = dfGroup$Group

#ggplot(data,aes(x = MDS1, y = MDS2, color = group, group = group, fill = group))+
  #geom_point(size=2)+
  #theme_classic()+
  #stat_ellipse( geom = "polygon", level = 0.95, alpha=0.3)+
  #geom_text( aes(label=rownames(data)), vjust=1.5, size=2.5, color = "black")+
  #labs( subtitle = paste("stress=",round(dfNmds$stress,3),sep=""))

 # plot
 ggplot(data,aes(x = MDS1,y = MDS2,))+
   geom_point(size=2)+
   theme_classic()+
   
   #stat_ellipse(             # add confidence interval
   #  geom = "polygon",
   #  level = 0.95,
   #  alpha=0.3)+
   
   geom_text(                # add label
     aes(label=rownames(data)),
     vjust=1.5,
     size=3,
     color = "black"
   )+
   labs(                     # add stress
     subtitle = paste("stress=",round(dfNmds$stress,3),sep="")
   )

 