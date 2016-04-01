distance <- function(data, lat, long, latName='latitude', longName='longitude', units='deg', radius, limit=1) {
  # Data:     Expects col headers with names latName and longName
  # lat:      Latitude to centre search at
  # long:     Longitude to centre search at
  # latName:  Name of latitude header name in data, Default = 'latitude'
  # longName: Name of longitude header name in data. Default = 'longitude'
  # units:    Units of the latitude and longitude values: degrees 'deg', radians 'rad', d/m/s 'dms'. Default = 'deg'
  # radius:   Radius to search (does nothing yet)
  # limit:    Upperbound on number of results. Deafult = 1
  return( process_geographic_data(data=data,lat=lat,long=long,latName=latName,longName=longName,radius=radius)[1:limit,] )
  
}


process_geographic_data <- function(data, lat, long, latName, longName, units='deg', radius=1) {
  
  # Convert headers to lowercase for consistency across code
  names(data) <- tolower(names(data))
  # Check if lat, long exists as headers in the data frame
  if(!all(c(latName, longName) %in% colnames(data))) {
    stop('Error, missing header label. Expected latName and longName')
  } # End check for header ontology
  
  # Add new column to store distance from given location ([lat, lon] point)
  data["distance"] <- NA 
  
  # Caluclate distance between points
  data$distance <- calculate_spherical_distance(lat1 = lat, long1 = long, lat2 = data$latitude, long2=data$longitude, units='deg', radius=radius)
  
  # Sort data into ascending order by distance column
  data <- arrange(data,distance)
  return(data)
} # End process_geographic_data



calculate_spherical_distance <- function(lat1, long1, lat2, long2, units='deg', radius=1) {
  radius_earth = 6371;
  
  # Convert angle values into radians
  if(units=='deg') { 
    lat1 <- deg2rad(lat1)
    long1 <- deg2rad(long1)
    lat2 <- deg2rad(lat2)
    long2 <- deg2rad(long2)
  } else if (units=='dms') {
    
  }
  
  # Determine distance using the haversine formula, assuming a spherical earth
  a <- sin((lat2-lat1)/2)^2 + cos(lat1)*cos(lat2)*sin((long2 - long1)/2)^2
  d <- 2*atan2(sqrt(a),sqrt(1-a))*radius_earth
  return(d)
} # End calculate_spherical_distance



deg2rad <- function(deg) {
  return(deg*pi/180)
} # End deg2rad