rm(list=ls())
library(tidyverse)
library(leaflet)
library(rgdal) # convert coordinates


setwd("/Users/oysteinsolvang/OneDrive - NUPI/nordic air power")
df.cbt <- read.csv(url("https://raw.githubusercontent.com/oysteinsolvang/nordicairpower/main/cbt.csv"),
                   header = FALSE)
df.qra <- read.csv2(url("https://raw.githubusercontent.com/oysteinsolvang/nordicairpower/main/scrambles_rnoaf.csv"))
df.qra <- df.qra %>% filter(year>=2000)
is.na(df.qra$scrambles) <- 0
qra <- ggplot(df.qra,aes(x=year,y=identified)) +
  geom_line() +
  xlab("Year") +
  ylab("No. of identifications") +
  ggtitle("Aircraft identified by RNoAF QRA since 2000") +
  bbc_style()
qra <- qra + geom_curve(aes(x = 2003, y = 71, xend = 2006.7, yend = 88),
                    colour = "#555555",
                    size=0.5,
                    curvature = -0.2,
                    arrow = arrow(length = unit(0.03, "npc")))
qra +   geom_label(aes(x = 2000.3, y = 63, label = "Russia intensifies \n long range \n air patrolling"),
                 hjust = 0,
                 vjust = 0.5,
                 lineheight = 0.8,
                 colour = "#555555",
                 fill = "white",
                 label.size = NA,
                 family="Times",
                 size = 10)
ggsave("qra.png", width=13, height=10)

df.cbt <- df.cbt %>%
  select(V1,V2) %>%
  filter(V1>=2010)
colnames(df.cbt) <- c("Year","Events")
df.cbt$Events <- as.numeric(df.cbt$Events)
#df.cbt[12,1] <- 2021
#str(df.cbt)

cbt <- ggplot(df.cbt,aes(x=Year,y=Events)) +
  geom_line() +
  xlab("Year") +
  ylab("No. of CBT training events") +
  ggtitle("Cross Border Training events per year since 2010") +
  bbc_style()+
  scale_y_continuous(limits=c(0,90))

cbt <- cbt + geom_curve(aes(x = 2014.5, y = 85, xend = 2012.2, yend = 90),
                    colour = "#555555",
                    size=0.5,
                    curvature = 0.2,
                    arrow = arrow(length = unit(0.03, "npc")))

cbt +   geom_label(aes(x = 2013, y = 81, label = "Preparations for \n Arctic Challenge Exercise"),
                 hjust = 0,
                 vjust = 0.5,
                 lineheight = 0.8,
                 colour = "#555555",
                 fill = "white",
                 label.size = NA,
                 family="Times",
                 size = 10)

ggsave("cbt.png", width=13,height=10)


## FIGHTER AIRCRAFT STATISTICS ##
df.acft <- read.csv(url("https://raw.githubusercontent.com/oysteinsolvang/nordicairpower/main/fighter_airframes.csv"))
df.acft$Generation <- factor(df.acft$Generation,
                             labels=c("4th","4.5","5th"),
                             levels=c(4,4.5,5))
order.5gen <- c("Italy","Finland","Norway","UK","Netherlands","Switzerland","Belgium","Poland","Denmark","France","Germany","Sweden","Spain","Austria")
df.acft
df.acft <- df.acft %>%
    select(-Type) %>%
    spread(Generation,Number)
df.acft[is.na(df.acft)] <- 0

df <- df.acft %>%
    group_by(Country) %>%
    gather(allvar,value,"4th":"5th")
colnames(df) <- c("Country","Generation","Number")

ggplot(df,aes(x=Country,y=Number,fill=Generation)) +
    geom_bar(stat="identity",
             position="stack") +
    scale_x_discrete(limits=order.5gen) +
    theme_classic()

ggsave("fighter_aircraft.png",width=12,height=8)



## MAP ##

#setwd("/Users/oysteinsolvang/Dropbox/NUPI/memorials")
d2 <- read.csv2("minnesmerker.csv")
y <- as.numeric(d2$nord)
x <- as.numeric(d2$øst)
d2 <- as.matrix(cbind(x,y))
d2 <- vect(d2, crs="+proj=utm +zone=33 +datum=WGS84  +units=m")
d2 <- terra::project(d2, "+proj=longlat +datum=WGS84")
d2 <- as.data.frame(geom(d2)[, c("y","x")])
colnames(d2) <- c("Northing","Easting")
d2 <- d2 %>%
  select(Northing, Easting)
num.cols <- c("Northing", "Easting")
d2[num.cols] <- sapply(d2[num.cols], as.numeric)
memorials <- st_as_sf(d2,
                      coords = c("Easting", "Northing"),
                      crs = 32633)


df <- as.data.frame(
  rbind(
    cbind(700400, 0245826, "Banak"),
    cbind(671609, 0142155, "Bodø")
  )
)
colnames(df) <- c("lat","long","Name")

#convert
cord.dec <- SpatialPoints(cbind(df$long, -df$lat), proj4string = CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32748"))
cord.UTM

?SpatialPoints


leaflet() %>%
  addTiles() %>%
  setView(20,64, zoom=5) %>%
  addMapPane("memorial_point", zIndex = 420) %>%
  addMarkers(
    data=memorials,
    label=labelvector,
    labelOptions = labelOptions(noHide = T, textsize = "15px"),
    options = pathOptions(pane = "memorial_point")) %>%
  addProviderTiles("Esri.WorldStreetMap")



