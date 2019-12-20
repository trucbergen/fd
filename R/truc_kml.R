library(rgdal)
data(eberg)
eberg <- eberg[runif(nrow(eberg))<.1,]
library(sp)
library(rgdal)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
## Not run: # Simple plot
kml(eberg, file = "eberg-0.kml")
# Plot using aesthetics
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
kml(eberg, colour = SNDMHT_A, size = CLYMHT_A,
    alpha = 0.75, file = "eberg-1.kml", shape=shape)
