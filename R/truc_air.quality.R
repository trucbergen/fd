library(dplyr)
fd::list_tables()

air <- fd::tbl("airquality") %>%
  dplyr::filter(date>="2011-01-01") %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()

# give you names
fhidata::norway_locations_long_current
air[fhidata::norway_locations_long_current, on="location_code", location_name:=location_name]

# add in GPS coordinates
fhidata::norway_map_municips
midpoints <- fhidata::norway_map_municips[,.(
  long=mean(long),
  lat=mean(lat)
),keyby=.(
  location_code
)]

air[midpoints, on="location_code", long:=long]
air[midpoints, on="location_code", lat:=lat]

#############################################################
ww <- fd::tbl("weather") %>%
  dplyr::filter(date>="2009-01-01") %>%
  dplyr::collect() %>%
  fd::latin1_to_utf8()

# give you names
fhidata::norway_locations_long_current
ww[fhidata::norway_locations_long_current, on="location_code", location_name:=location_name]

# add in GPS coordinates
fhidata::norway_map_municips
midpoints <- fhidata::norway_map_municips[,.(
  long=mean(long),
  lat=mean(lat)
),keyby=.(
  location_code
)]
ww[midpoints, on="location_code", long:=long]
ww[midpoints, on="location_code", lat:=lat]
