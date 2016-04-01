stations <- ghcnd_stations()

list_oz_stations <- grep('^ASN', stations$data$id)

oz_stations <- stations$data[list_oz_stations,]
tail(oz_stations)
