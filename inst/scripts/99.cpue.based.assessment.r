require(aegis)
require(aegis.bathymetry)

#choose one:
if (!exists("year.assessment")) {
  year.assessment=lubridate::year(Sys.Date())      # year.assessment
  year.assessment=lubridate::year(Sys.Date()) -1   # or year previous to current
}

p = bio.snowcrab::load.environment( year.assessment=year.assessment )

#set some variables. Set in function call if using a wrapper function.
probs=c(0:1, 0.99) #set data inclusion range by quantile, adjust as required
quant=seq(0,1, by=0.1)
p$pres=10 #resolution of grids in km's
variable='cpue' #can also be 'effort' or 'landings'

#choose one:
   if (0) {
  x = logbook.db( DS="logbook" ) #uses existing dataset
  #X= redo.fishery.data(x) #refresh all fishery data, default is p$year.assessment, can add "yrs=1996:p$year.assessment to call 
}

x = x [polygon_inside( x, region="isobath1000m"),]
x = x[ which(x$effort <= 201) ,] #200 traps is max, removes NA effort (confounding cpue)
x = x[ which(x$cpue < 400),] #400 kg/trap is max, removes NA's
x$year=x$yr #this creates proper offset for 4X, 2017-18 season =2017

x = lonlat2planar( x,  proj.type=p$aegis_proj4string_planar_km )
x = subset(x,select=c('year','plon','plat',variable,'date.fished', 'cfa'))
x=x[is.finite(x$date.fished),] #removes a few entrie pre-2004 where date not specified

names(x)=c('year','plon','plat','z','date', 'stock')

#er = quantile( x$z[x$z>0], probs=probs) #removes anything above the xth percentile, set above
#m=as.data.frame(er)[2,1]
#x=x[x$z<m]

yrs=unique(x$year)
areas=unique(x$stock)



#determine mean (or whatever) values for each grid
xyz = list()

for(i in 1:length(yrs)){
  xd = subset(x,year==yrs[i],c('plon','plat','z','stock', 'date' ))
  g= spatial_grid(p,"planar.coords") #breaks into grid blocks (set by p$pres)
  xd$plon = grid_internal( xd$plon, g$plon ) #fits lon values of data into grid
  xd$plat = grid_internal( xd$plat, g$plat ) #fits lat values of data into grid
  xyz[[i]] = aggregate(z~plon+plat+stock,data=xd,FUN="mean")
  names( xyz[[i]]) = c("plon", "plat", "stock", variable)
}


#Summarize the above grid values (i.e. mean, etc) by stock by year
out=list()

for(i in 1:length(yrs)){
  out[[i]] = setNames(aggregate(cpue~stock, data=xyz[[i]],FUN="mean"), c("stock", "mean"))
  
  med= setNames(aggregate(cpue~stock, data=xyz[[i]],FUN="median"), c("stock", "median"))
  
  #out[[i]]=cbind(out[[i]], median=med[,2])
  out[[i]]=cbind(out[[i]], med)
  
  #out[[i]] = aggregate(cpue~stock, data=xyz[[i]],FUN=function(x) quantile(x))
  #cbind multiple analyses, etc in here
  out[[i]]$year=yrs[i]
  #names(out[[i]])=c("stock", "mean", "year") #get labels named properly based on above aggregates in order
}
out = do.call(rbind, out)

all= out[with(out, order(stock, year)),] #order datafame by stock then year
all$id=paste(all$stock, all$year, sep=":") #add column to merge on

biomass=read.csv("C:/bio.data/bio.snowcrab/assessments/2020/cpue.assessment/biomass.csv")
biomass$id=paste(biomass$stock, biomass$year, sep=":") #add column to merge on

test= merge(x=biomass[,c("id", "biomass")], y=all, by="id")
