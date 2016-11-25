##################################
# Creating Inset Maps in ggplot2 #
##################################

library(ggplot2)
library(raster)
library(gridExtra)

ph0<-getData("GADM", country="PHL", level=0) # download PHL level 0 map for ucdavis site
phl<-getData("GADM", country="PHL", level=2) # download PHL level 2 map for ucdavis site
mrdq<-(phl[phl$NAME_1=="Marinduque",]) # subset province of Marinduque from PHL map
munnames<-coordinates(mrdq) # get center coordinates of municipalities of Marinduque
munnames<-data.frame(munnames) # convert matrix format munnames object to data.frame
munnames$label<-mrdq@data$NAME_2

# Extent rectangle for inset map
pol<-data.frame(xmin=121.7,xmax=122.2 ,ymin=13 ,ymax=13.7)


# Main Map
p1<-
  ggplot() +
  geom_polygon(data=mrdq, aes(long+0.008,lat-0.005, group=group), fill="#9ecae1")+
  geom_polygon(data=mrdq, aes(long,lat, group=group), colour="grey10",fill="#fff7bc")+
  geom_text(data=munnames, aes(x=X1, y=X2,label=label), size=3, colour="grey20")+
  coord_equal()+theme_bw()+xlab("")+ylab("")+
  scale_x_continuous(breaks=seq(121.8,122.2, 0.1), labels=c(paste(seq(121.8,122.2, 0.1),"°E", sep="")))+
  scale_y_continuous(breaks=seq(13.2,13.6, 0.1), labels=c(paste(seq(13.2,13.6, 0.1),"°N", sep="")))+
  theme(axis.text.y =element_text(angle = 90, hjust=0.5))

#Inset
p2<-
  ggplot() +
  geom_polygon(data=ph0, aes(long,lat,group=group),colour="grey10",fill="#fff7bc")+
  coord_equal()+theme_bw()+labs(x=NULL,y=NULL)+
  scale_x_continuous(breaks=seq(117.5,125, 2.5), labels=c(paste(seq(117.5,125, 2.5),"°E", sep="")))+
  scale_y_continuous(breaks=seq(5,20, 5), labels=c(paste(seq(5,20, 5),"°N", sep="")))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank())

png(file="mrdq.png",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.3, x = 0.86, y = 0.28) #plot area for the inset map
print(p1,vp=v1) 
print(p2,vp=v2)
dev.off()
