
  data.quality.check = function( type, p ) {

    # duplicated stations
    if (type=="stations") {
      set <- snowcrab.db( DS="setInitial" )
      must.be.unique = paste( set$yr, set$station, sep="~" )
      dups = which(must.be.unique %in% must.be.unique[which(duplicated(must.be.unique))]) # all dups
      x = set[ dups, c( "trip", "set", "station" ) ]
      x = x[ is.finite(x$station), ]
      dup.stations = x[ order(x$station) ,]
      print( "Duplicated stations:" )
      print( dup.stations  )
      return (dup.stations)
    }

    # counts of stations by area
    if(type=="count.stations") {
      set <- snowcrab.db( DS="setInitial" )
      years = sort( unique( set$yr ) )
      nyears = length(years)
      nregions = length(p$regions)
      res = matrix( NA, nrow=nyears, ncol=nregions)
      for (r in 1:nregions) {
        nr = polygon_inside(x=set, region=aegis.polygons::polygon_internal_code(p$regions[r]), planar=F)
        for (y in 1:nyears) {
          ni = which( set$yr==years[y] )
          res[y,r] = length( unique( intersect (nr, ni) ) )
      }}
      x = as.data.frame(res)
      names(x) = c(p$regions)
      x$yr = years
      x = x[ , c("yr", p$regions)]
      print( "Number of stations: ")
      print (x)
      return(x)
    }

    # positional information
    if(type=="position") {
      set <- snowcrab.db( DS="setInitial" )
      plot(set$lon, set$lat)
      inside = polygon_inside( set[, c("lon", "lat") ], "cfaall")
      if (length (inside) == nrow(set) ) {
        print("All data are within positional bounds")
        return (NULL)
      } else {
        outside = setdiff( 1:nrow(set), inside )
        points( set$lon[outside], set$lat[outside], col="red")
        print( "------------- The following are out of the cfa bounds: "  )
        print( set[ outside, ] )
        return(  set[ outside, ]  )
      }
    }
    # positional information
    if(type=="position.difference") {
      set <- snowcrab.db( DS="setInitial" )
      set.sub = split(set, set$station)
      print("Stations that are ouside of historical positions:")
      for(i in 1:length(set.sub)){
        sta.match = set.sub[[i]]
        ave.lon.start = mean(sta.match$lon)
        ave.lon.end = mean(sta.match$lon1)
        ave.lat.start = mean(sta.match$lat)
        ave.lat.end = mean(sta.match$lat1)
        out = NULL
 
        for(j in 1:nrow(sta.match)){
          if(is.na(ave.lon.start) || is.na(ave.lat.start) || is.na(ave.lon.end) || is.na(ave.lat.end)){
            print("Contains NA positions: ")
            print( sta.match[ j, ] )
            out= rbind(out, sta.match[ j, ])
          }
          else{
          if(abs(sta.match$lon[j] - ave.lon.start) > .25){
            print(paste("Average start longitude: ", ave.lon.start, sep = ""))
            print( sta.match[ j, ] )
            out= rbind(out, sta.match[ j, ])
          }
          if(abs(sta.match$lon1[j] - ave.lon.end) > .25){
            print(paste("Average end longitude: ", ave.lon.end, sep = ""))
            print( sta.match[ j, ] )
            out= rbind(out, sta.match[ j, ])
          }
          if(abs(sta.match$lat[j] - ave.lat.start) > .25){
            print(paste("Average start latitude: ", ave.lat.start, sep = ""))
            print( sta.match[ j, ] )
            out= rbind(out, sta.match[ j, ])
          }
          if(abs(sta.match$lat1[j] - ave.lat.end) > .25){
            print(paste("Average end latitude: ", ave.lat.end, sep = ""))
            print( sta.match[ j, ] )
            out= rbind(out, sta.match[ j, ])
          }
          }
        }
      }
        return(out)
      }
      
     

    if (type=="seabird.load") {
        SS = snowcrab.db( DS="set.clean")
#        sb = seabird.db( DS="set.seabird.lookuptable" )
#        SS = merge( SS, sb, by=c("trip", "set"), all.x=TRUE, all.y=FALSE, sort=TRUE )
        isb = which( is.na( SS$seabird_uid) & SS$yr %in% p$seabird.yToload & SS$yr >= 2012 )
        print( "Missing seabird matches: ")
        print( SS[ isb,] )
        return( SS[isb,])
    }


    if (type=="minilog.load") {
        SS = snowcrab.db( DS="set.clean")
        # ml = minilog.db( DS="set.minilog.lookuptable" )
        # SS = merge( SS, ml, by=c("trip", "set"), all.x=TRUE, all.y=FALSE, sort=TRUE )
        iml = which( is.na( SS$minilog_uid) & SS$yr %in% p$minilog.yToload & SS$yr >= 2004 )
        print( "Missing minilog matches: ")
        print( SS[ iml,] )
        return( SS[iml,])
    }

    if (type=="netmind.load") {
        SS = snowcrab.db( DS="set.clean")
#        nm = netmind.db( DS="set.netmind.lookuptable" )
#        SS = merge( SS, nm, by=c("trip", "set"), all.x=TRUE, all.y=FALSE, sort=TRUE )
        inm = which( is.na( SS$netmind_uid) & SS$yr %in% p$netmind.yToload & SS$yr >= 2004 )
        print( "Missing netmind matches: ")
        print( SS[ inm,] )
        return( SS[inm,])
    }


    if (type=="tow.duration") {
      set <- snowcrab.db( DS="set.clean" )
      e0 = which( ( set$dt > 9  | set$dt < 3.5 )  & set$yr >=2004 )
      if  (length(e0)>0 ) {
        print( "The following have rather short/long tow times (dt)" )
        print( set[e0, c("trip", "set", "station", "dt", "timestamp")] )
        return (set[e0,] )
      }
    }


    if (type=="tow.distance") {
      # expected = 2 knots * 5 min = 2 * 1.852 * 5/60 = 0.309 km ( so a good range is {-25%, +75%} = (0.232, 0.5408)
      set <- snowcrab.db( DS="set.clean" )
      e0 = which( ( set$distance > 0.541  | set$distance < 0.232 )  & set$yr >=2004 )
      if  (length(e0)>0 ) {
        print( "The following have rather short/long tow distances" )
        print( set[e0, c("trip", "set", "station", "distance", "timestamp")] )
        return (set[e0,] )
      }
    }


    if (type=="netmind.timestamp") {
      #  check times/data and merge remaining data using datestamps and {station, set}
      set <- snowcrab.db( DS="set.clean" )
      nm = netmind.db("stats")
      nm$netmind.timestamp = nm$t0
      set = merge(set[,c("trip", "set", "lon", "lat", "timestamp", "seabird_uid", "minilog_uid", "netmind_uid")],
                  nm[ ,c("netmind_uid","netmind.timestamp", "slon", "slat" )], by="netmind_uid", all.x=TRUE, all.y=FALSE )
      time.diff = difftime( set$netmind.timestamp, set$timestamp )
      time.thresh = lubridate::minutes(30)
      i = which( abs( time.diff ) > time.thresh )
      if (length(i)>0) {
        print ("Potential date/time mismatches::")
        print( set[i, ] )
        return(set[i,])
      }
    }

    # netmind mismatches
    if(type=="netmind.mismatches") {
      set <- snowcrab.db( DS="set.clean" )
      q = which( set$yr > 2004 & (set$netmind_uid==""| is.na(set$netmind_uid) )  )
      if ( length (q) > 0 ) {
        print( "No netmind matches for the following sets:")
        print ( set[q,c("trip", "set", "station", "t0", "timestamp") ] )
        return( set[q,c("trip", "set", "station", "t0", "timestamp") ] )
      }
    }


    # poor minilog matches
    if(type=="minilog") {
      set <- snowcrab.db( DS="set.clean" )
      must.be.unique = set$t0
      dups = which(must.be.unique %in% must.be.unique[which(duplicated(must.be.unique))]) # all dups
      x = set[ dups, c( "trip", "set", "station", "t0" ) ]
      dup.t0 = x[ is.finite(x$t0), ]
      print( "Duplicated minilog times:" )
      print( dup.t0 )
      return (dup.t0)
    }

    # minilog mismatches
    if(type=="minilog.mismatches") {
      set <- snowcrab.db( DS="set.clean" )
      q = which( set$yr > 2004 & (set$minilog_uid==""| is.na(set$minilog_uid) )  )
      if ( length (q) > 0 ) {
        print( "No minilog matches for the following sets:")
        print ( set[q,c("trip", "set", "station", "t0", "timestamp") ] )
        return( set[q,c("trip", "set", "station", "t0", "timestamp") ] )
      }
    }

    if(type=="minilog.dateproblems") {
      set <- snowcrab.db( DS="set.clean" )
      time.thresh = lubridate::hours(1)
      ii = which( abs( difftime( set$t0, set$timestamp )) > time.thresh )
      if ( length (ii) > 0 ) {
        print( "Minilog date mismatches with Trip ID, using Trip id as default pre 2005:" )
        print ( set[ii, c("trip", "set", "station", "t0", "timestamp", "minilog_uid")] )
        return ( set[ii, c("trip", "set", "station", "t0", "timestamp", "minilog_uid")] )
      }
    }

    if(type=='na.spread') {
      set <- snowcrab.db( DS="set.clean" )
      ii <- which(is.na(set$spread))
    	x = set[ii,c('trip','set','station','netmind')]
    	print(x)
    	return(x)
    }

    if(type=='na.distance') {
      set <- snowcrab.db( DS="set.clean" )
    	ii <- which(is.na(set$distance))
    	x = set[ii,c('trip','set','station','netmind')]
    	print(x)
    	return(x)
    }
  }

