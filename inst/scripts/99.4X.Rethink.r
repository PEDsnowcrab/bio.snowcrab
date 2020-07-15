#BZ July 2020- Work in progress
#Began to investigate potential alternate approaches to Harvest Control Rules / ref point
#using catch rates and potentially incorporating temperature
#explored using carstm temps and the bnam data series from BIO Oceanographers, likely bnam
#grids landings / catch rates & determines associated bottom temps for thos grids
#an example of simplistic (not using temp) catch rate reference points are provided at end of script
#Still requires investigation into how use of temp might better inform the ref points
#by accounting for temp effect, a clearer picture of catch rate may be possible for ref points


# -------------------------------------------------
# Part 1 -- import temperature grids from carstm

require(aegis)
require(aegis.bathymetry) #if doing a full refresh of logbook data
require(aegis.polygons)



#choose one, remembering that 4X season refers to start year
if (!exists("year.assessment")) {
  year.assessment=lubridate::year(Sys.Date())      # year.assessment
  year.assessment=lubridate::year(Sys.Date()) -1   # or year previous to current
}

#Choose one
#NB. The carstm temp approach is not fully functional. Left here in case to allow for future work if needed.
#BNAM yields higher spatial and temporal density data
#temp.model="carstm"
temp.model="bnam" 

if (temp.model=="carstm"){
    p = bio.snowcrab::snowcrab_carstm( DS="parameters", assessment.years=1999:year.assessment )

    #Import grid dataframe (25km)- previously created 
    sppoly = areal_units( p=p )  # to reload
    plot(sppoly)
    text(coordinates(sppoly), sapply(slot(sppoly, "polygons"), function(i) slot(i, "ID")), cex=0.5) #labels grid ID's
    spplot( sppoly, "au_sa_km2", main="AUID", sp.layout=p$coastLayout )
    
    #load carstm temperature data data layer (matches sppoloy grids)
    load("C:/bio.data/bio.snowcrab/modelled/temperature.carstm_inputs.snowcrab_assessment_25.rawdata.rdata") #data frame named M
}

# -------------------------------------------------
# Part 2 -- construct basic parameter list
p = bio.snowcrab::load.environment( year.assessment=year.assessment )

outfile=file.path(p$datadir,"assessments", lubridate::year(Sys.Date()), "4x")

#resolution of grids in km's, 25 matches the carstm assessment approach
if (temp.model=="carstm") p$pres=25 
if (temp.model=="bnam") p$pres=10 



# -------------------------------------------------
# Part 3 -- import fishery data for 4X


#res = get.fishery.stats.by.region( Reg="cfa4x") #for reference
# Uncomment following line to do a fresh logbook datadump
#redo.fishery.data(x, yrs=p$year.assessment) #can change to "yrs=1996:p$year.assessment" for more years  

x = logbook.db( DS="logbook" ) #load logbook data
x=x[x$cfa %in% "cfa4x",]
x$year=x$yr #year offset for 4X, 2017-18 season -> 2017


#Grid the records to allow summing on grids
x = lonlat2planar( x,  proj.type=p$aegis_proj4string_planar_km )
g= spatial_grid(p,"planar.coords") #breaks into grid blocks (set by p$pres)
x$plon = grid_internal( x$plon, g$plon ) #fits lon values of data into grid
x$plat = grid_internal( x$plat, g$plat ) #fits lat values of data into grid
x$id=paste(x$plat, x$plon, sep=":")
rm(g)
xraw=x #xraw data frame contains all information for landings calculations

#need to clean data for catch rate calculations
x1=x[x$year<2007 & x$effort<61, ] #removes 200 trap (smaller, non-comparable cpue) complement 
x2=x[x$year>2006, ] #Only large traps used from 2007 onwards
x=rbind(x1, x2)
rm(x1)
rm(x2)

#x = x [polygon_inside( x, region="isobath1000m"),]
x = x[ which(x$effort <= 201) ,] #200 traps is max, removes NA effort (confounding cpue)
x = x[ which(is.finite(x$landings)),] #landings- removes NA's



#Determine CPUE by license by year

yearly=aggregate(cbind(landings, effort)~licence + year, data=x, FUN="sum")
    yearly$cpue=yearly$landings/yearly$effort
    yearly= yearly[with(yearly, order(licence, year)),]

yearly.agg=aggregate(cpue~year, data=yearly, FUN="median")

year.tot=aggregate(landings~year, data=xraw, FUN="sum")
    year.tot= year.tot[with(year.tot, order(year)),]
year.tot=merge(year.tot, yearly.agg)

save(xraw, file=paste(outfile, "logs.rdata", sep="/") )
save(yearly, file=paste(outfile, "annual.by.license.rdata", sep="/") )
save(year.tot, file=paste(outfile, "annual.landings.rdata", sep="/") )

# can be moved to bio.snowcrab/ r in time
# left here for now to allow tweaking

timeseries.4x.cpue= function(x, avg="median"){ #fun can be median, mean, gm_mean
      plot(yearly$year[yearly$licence=="100201"], yearly$cpue[yearly$licence=="100201"], type="n", main="4X Catch Rates", 
      ylim=c(0, 70), xlab="year", ylab="kg/trap")
      lics=unique(x$licence)    
      for (l in lics){
          ly=yearly[yearly$licence==l,]
          points(ly$year, ly$cpue, pch=20)
          cols=c("chartreuse", "cyan", "firebrick1", "yellow", "steelblue", "tan", "lightcoral" )
          lines(ly$year, ly$cpue, col=sample(cols,1))
          }
      if (avg=="mean") x4=aggregate(cpue~year, data=yearly, FUN="mean")
      if (avg=="median") x4=aggregate(cpue~year, data=yearly, FUN="median")
      lines(x4$year, x4$cpue, lwd=4, col="red")
}

timeseries.4x.cpue(x)

#to export as csv for plotting elsewhere
if (0){
      x4=aggregate(cpue~year, data=yearly, FUN="median")
      x4$licence="median"
      x4=x4[,c("licence", "year", "cpue")]
      by.year=yearly[,c("licence", "year", "cpue")]
      by.year=rbind(by.year, x4)
      file=paste("C:/bio.data/bio.snowcrab/assessments/",year(Sys.Date()), "/4x/yearly.csv", sep="")
      write.csv(by.year, file=file)
}

# -------------------------------------------------
# Part 4 -- determine temperatures to model with catch rates for each year

#Determine which n grids contained the highest landings within a given year (season)
timeframe="recent" #can be changed to "all" if desired
    
    if (timeframe=="all") {
      grid.sums=aggregate(landings~id , data=xraw, FUN="sum")
    } else {
      grid.sums=aggregate(landings~id, data=xraw[xraw$year>(p$year.assessment- 6),], FUN="sum") #last 5 years
    }
    
#Choose one:
   tp= grid.sums[with(grid.sums,order(-landings)),]
   tp=tp[which(tp$id!="NA:NA"),] #removes unknown locations

#select number of grids to explore temps
if (p$pres==10) n=4 #67% of all 4X landings since 2014 came from four 10x10km grids
if (p$pres==25) n=2 #82% of all 4X landings since 2014 came from two 25x25km grids
      
top=tp[1:n,]

if (temp.model=="carstm"){
#Need a reference lat / long for each of those grids to determine the AUD to lookup temps 

}


if (temp.model=="carstm") { #Determine AUID grid from M of each of the grids from X

  for (i in top$id){
    top$lat[top$id==i]=median(xraw$lat[xraw$id==i])
    top$lon[top$id==i]=median(xraw$lon[xraw$id==i])
  }
  
      crs_lonlat = sp::CRS(projection_proj4string("lonlat_wgs84"))
      top$AUID = over( SpatialPoints( top[, c("lon", "lat")], crs_lonlat ), spTransform(sppoly, crs_lonlat ) )$AUID # match each datum to an area
      #Warning- some near-shore 4X positions will fall outside the temperature grid structure.
      
      yrs=2001:p$year.assessment
      M=M[M$year>2000,]
      #M$dyri is in tenths of year
      #.05=Jan 18, .15=Feb 23, .25=Mar 30, .35=May 6, .45=Jun 11
      #.55=Jul 17, .65=Aug 22, .75=Sep 27, .85=Nov 2, .95=Dec 8
     
    h=(top$AUID[c(1:2)]) #smaller grids likely larger of grids here
    *my=M[M$AUID %in% h,] 
    
    # Temp lookup from CARSTM assessment modelling
    #-----------------------------------------------------------
    pT = temperature_carstm(p=p, DS="parameters_override" )
    
    kk =  which( !is.finite(M[, pT$variabletomodel]))
    if (length(kk) > 0) {
      AD = temperature.db ( p=pT, DS="aggregated_data"  )  # 16 GB in RAM just to store!
      AD = AD[ which( AD$lon > p$corners$lon[1] & AD$lon < p$corners$lon[2]  & AD$lat > p$corners$lat[1] & AD$lat < p$corners$lat[2] ), ]
      # levelplot( eval(paste(p$variabletomodel, "mean", sep="."))~plon+plat, data=M, aspect="iso")
      
      AD$AUID = over( SpatialPoints( AD[, c("lon", "lat")], crs_lonlat ), spTransform(sppoly, crs_lonlat ) )$AUID # match each datum to an area
      AD$uid = paste(AD$AUID, AD$year, AD$dyear, sep=".")
      
       M_dyear_discret = discretize_data( M$dyear, pT$discretization$dyear )  # AD$dyear is discretized. . match discretization
      M$uid =  paste(M$AUID, M$year, M_dyear_discret, sep=".")
      
      oo = tapply( AD[, paste(pT$variabletomodel, "mean", sep="." )], AD$uid, FUN=median, na.rm=TRUE )
      
      jj = match( as.character( M$uid[kk]), as.character( names(oo )) )
      M[kk, pT$variabletomodel] = oo[jj ]
}
}


if (temp.model=="bnam") {
# Temp lookup from BNAM temperature modelling
  #----------------------------------------------------------- 
source('C:/bio/bio.snowcrab/R/bnamR.r')
  
bnam=bnamR(redo=F)
bnam$year=substr(bnam$timeS, 1, 4)
bnam$m=substr(bnam$timeS, 6, 8)
months=unique(bnam$m)
renamed=c(1:12) 
for (i in 1:length(months)){
  bnam$month[bnam$m==months[i]]=renamed[i]
}

#create a polygon for each "top" grid based on fishing positions within the grid

require(sp)
require(PBSmapping)


xyz=list()
u=unique(top$id)
  
for (i in 1:length(u)){
      t=top$id[i]
      grid=xraw[xraw$id==t,]
      inter=grid[,c("lat", "lon")]
      
      #add a category (required for later rasterizing/polygonizing)
      inter <- cbind(inter, cat = rep(1L, nrow(inter)),stringsAsFactors = FALSE)
      
      #convert to spatial points
      coordinates(inter) = ~lon + lat
      proj4string(inter) = CRS(projection_proj4string("lonlat_wgs84"))
      
      #determine bounding box of these points and convert to matrix
      bb=data.frame(inter@bbox)
      cds = matrix(c( bb[1,1], bb[2,1],
                      bb[1,2], bb[2,1],
                      bb[1,2], bb[2,2],
                      bb[1,1], bb[2,2],
                      bb[1,1], bb[2,1]), 
                      ncol = 2, byrow = TRUE)
      #create a spatial polygon
      P1 = Polygon(cds)
      Ps = SpatialPolygons(list(Polygons(list(P1), ID = i)), proj4string=CRS(projection_proj4string("lonlat_wgs84")))
      PS=SpatialPolygons2PolySet(Ps) #Converts polygon to a PBSMapping Polyset
      PS$PID=i
      xyz[[i]]=PS
      if (i==length(u)) print("xyz is a list with n PBS:: PolySets as elements")
      }

#determine overlapping bnam polygons

hg=data.frame()

for (i in 1:n){
  df = findPolys(bnam$locsP,xyz[[i]])
  hg=rbind(df, hg)    
  } 

ahr = merge(hg,bnam$locsP,by='EID')

#extract bottom temps from bnam dataset
outbnam = list()
m=0
for(i in 1:length(u)){
  m=m+1
  h = subset(ahr,PID == i)
  k = bnam$bTs[which(bnam$bTs[,1] %in% h$EID),]
  if (length(unique(h$EID))==1) k=rbind(c(k))
  k=k[,2:ncol(k)]
  if (length(unique(h$EID))==1) k=rbind(c(k))
  #colnames(k)=bnam$timeS
  outbnam[[m]]=k
}		

#average all temps grids by month
raw.temps=do.call(rbind,outbnam)
raw.temps= apply(raw.temps,2,mean)

#to plot temp time trends (with Loess smooth)
op = stl(ts(raw.temps,frequency=12, start=c(1990,1)),s.window=7)
plot(op)


temps= op$time.series[,2] #trend
#seasonality.temps= op$time.series[,1] #seasonal

Month=factor(cycle(temps), levels = 1:12, labels = month.abb)		
temps=tapply(temps, list(year = floor(time(temps)), month = Month), c)


write.csv(temps,file=paste(outfile, '/bnam.climatology.csv', sep=''))


#write.csv(seasonality.smooth.temps,file=paste(outfile, '/bnam.seasonality.csv', sep=''))

#Merge fisheries data with temp data
yrs=as.character(year.tot$year)
cnames=levels(unique(Month))
year.tot[ , cnames]=NA

ti=data.frame()

for (y in yrs){
  t=ti
  ti=year.tot[year.tot$year==y,]
  for (m in levels(unique(Month))) {
    ti[,m]=temps[y,m]
      }
ti=rbind(t, ti)
all=ti
 }

#Object "all" now contains median landings and median catch rates by year with a mean bottom temp for each month imported from bnam
# -------------------------------------------------
# Part 5 -- model with catch rates with temperature
#.....


#Simple plotting of potential Ref Pts and HCR
#Uses lobster IFMP approach of bmsy=long term median catch rate, ref points are 40 and 80% of BMSY

bmsy=median(all$cpue)
plot(all$year, all$cpue, ylim=c(0,1.1*(max(all$cpue))), type="l", xlab="Year", ylab="CPUE (kg)", main="4X")
points(all$year,all$cpue, pch=16)
abline(h=0.4*bmsy, col="red", lwd=3)
abline(h=0.8*bmsy, col="yellow", lwd=3)
abline(h=bmsy)
text(x=min(as.numeric(all$year))+1, y=1.1*median(all$cpue), "BMSY")