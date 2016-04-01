lat <- runif(1000,1,50)
long <- runif(1000,1,50)
data <- data.frame(lat,long)

data_sorted <- meteo_distance(data=data,lat=12.5,long=12.5,latName='lat',longName='long',limit=100)