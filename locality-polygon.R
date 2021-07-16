######################
#
# Locality <-> polygon
#
###################### 
#
#
#
require(GISTools)
#
require(ggplot2)
#
polygons <- readRDS("", refhook = NULL)
#
polygon_radius=function(polygon)
	{
  		coords=fortify(polygon)[,c(1:2)]  
  		center=as.data.frame(gCentroid(polygon)) # centroid

  			dist_fx=function(long, lat, center=center)
  				{
    				dist=sqrt((long-center[1])^2+(lat-center[2])^2)
    				return(dist)
    			}
    
  		dists=mapply(dist_fx, coords[,1], coords[,2], MoreArgs=list(center))
  		furthest_index=as.integer(which(dists==max(unlist(dists))))
  		furthest=unique(coords[furthest_index,]) # maximum point
  		distance <- acos(sin(center[[2]]*pi/180)*sin(furthest[[2]]*pi/180)+cos(center[[2]]*pi/180)*cos(furthest[[2]]*pi/180)*cos(furthest[[1]]*pi/180-center[[1]]*pi/180))*6371
  		return<- data.frame(c(center, distance)) # haversine formula to estimate distance
  		names(return) <- c("centroid (long)", "centroid (lat)", "distance (km)")
  
  		return(return)
	}
#
#
#
matrix <- list() 
				for (i in 1:length(polygons))
					{
						matrix[[i]] <- polygon_radius(polygons[i,])	
					}
#
#
#
result <- do.call("rbind", matrix)


