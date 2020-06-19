require(aegis)
require(aegis.bathymetry) #if doing a full refresh of logbook data
require(data.table)

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

# Uncomment following line to do a fresh logbook datadump
#X= redo.fishery.data(x, yrs=p$year.assessment) #can change to "yrs=1996:p$year.assessment" for more years  

x = logbook.db( DS="logbook" ) #load logbook data


x = x [polygon_inside( x, region="isobath1000m"),]
x = x[ which(x$effort <= 201) ,] #200 traps is max, removes NA effort (confounding cpue)
x = x[ which(x$cpue < 400),] #400 kg/trap is max & removes NA's
x$year=x$yr #this creates proper offset for 4X, 2017-18 season =2017

x = lonlat2planar( x,  proj.type=p$aegis_proj4string_planar_km )
x = subset(x,select=c('year','plon','plat','cpue','landings', 'effort', 'date.fished', 'cfa'))
x=x[is.finite(x$date.fished),] #removes a few entries pre-2004 where date not specified

names(x)=c('year','plon','plat','cpue','landings','effort','date', 'stock')

yrs=sort(unique(x$year))
areas=unique(x$stock)

#Following function allows for date filtering of logbook entries
#can be moved to "r" (functions) directory later
#Currently here for transparency of process

date.filter= function(xd, per=per){
  xd2=data.frame()
  for (ii in unique(xd$id)){
    period=per
    uid=xd[xd$id==ii,]
    qs=quantile(uid$date, names=F)
    
    #Need minimum of 5 days, or use all
    diff= time_length(max(uid$date)-min(uid$date), "day")
    if(diff<6) period="all"
    
   # print(ii) #These print statements allow manual filter checks. Sanity Check
    #print(paste("days= ", diff, sep=""))
    #print(paste("period=", period, sep=" "))
    
    #Need minimum of 1% of CFA traphauls, or use all
    focus.stock=uid$stock[1]
    grid.effort= sum(uid$effort)
    if(grid.effort < 100)  period="all"
   
    
   # print(paste("stock=", focus.stock, "grid effort=", grid.effort, "trap hauls", sep=" " ))
   # print(period)
    
    if(period=='firstq') xi=which(uid$date<=qs[2])
    if(period=='lastq') xi=which(uid$date>=qs[4])
    if(period=='all') xi=which(uid$date>=qs[1])
    tid=uid[xi,]
    xd2=rbind(xd2, tid)
  }
  return(xd2)
}

xyz = list()
per="all" #Period can be "all", "firstq", "lastq"

for(i in 1:length(yrs)){
  xd = subset(x,year==yrs[i],c('plon','plat','stock','cpue', 'landings','effort', 'date' ))
  xd$year=yrs[i]
  g= spatial_grid(p,"planar.coords") #breaks into grid blocks (set by p$pres)
  xd$plon = grid_internal( xd$plon, g$plon ) #fits lon values of data into grid
  xd$plat = grid_internal( xd$plat, g$plat ) #fits lat values of data into grid
  xd$id=paste(xd$plat, xd$plon, sep=":")

  # Time filtering within each grid
  if(per=='firstq') xd=date.filter(xd, per="firstq")
  if(per=='lastq') xd=date.filter(xd, per="lastq")
  if(per=='all') xd=date.filter(xd, per="all")
  
  xx = aggregate(landings~plon+plat+stock+year+id,data=xd,FUN="sum")
  yy= aggregate(effort~plon+plat+stock+year+id,data=xd,FUN="sum")
  xy= merge(xx,yy, all=T)
  xy$cpue=xy$landings/xy$effort
  xy$period=per
  xyz[[i]]=xy
}


#count of grids by year

cnt= do.call(rbind, xyz)
grids.by.yr= aggregate( id~ year + stock, data=cnt, FUN= function(x) length(unique(x)))


#Summarize the above grid values (i.e. mean, etc) by stock by year
out=list()

for(i in 1:length(yrs)){
  out[[i]] = setNames(aggregate(cpue~stock, data=xyz[[i]],FUN="mean"), c("stock", "mean"))

  quan=aggregate(cpue~stock, data=xyz[[i]],FUN=function(x) quantile(x))
 
  out[[i]]=cbind(out[[i]], quan[,-1])
  
  out[[i]]$year=yrs[i]
  
}

out = do.call(rbind, out)
out=out[out$year>2003,] #post-2004 biomass estimates are most stable so truncate from there

all= out[with(out, order(stock, year)),] #order datafame by stock then year
all$id=paste(all$stock, all$year, sep=":") #add column to merge on

biomass=read.csv("C:/bio.data/bio.snowcrab/assessments/2020/cpue.assessment/biomass.csv")
biomass$id=paste(biomass$stock, biomass$year, sep=":") #add column to merge on

test= merge(x=biomass[,c("id", "biomass", "landings")], y=all, all.x=T, all.y=T,  by="id")
test=na.omit(test)
setnames(test, old = c('0%', '25%', '50%', '75%', '100%'), new = c('q0', 'q25', 'q50', 'q75', 'q100'))

test.metrics=c("mean", "q0", "q25", "q50", "q75", "q100")
deps=c("biomass", "er")

#Model and test
#----------------------------------
require(dbplyr)
require(broom)

areas=unique(test$stock)

for (a in areas){
n=test[test$stock==a,]
for (m in test.metrics){
  #summary(mod)
  #can change to exploitation rate ($er)from biomass if desired 
  plot(n[,m],n[,"biomass"], xlab=m, ylab="Biomass", main=a)
  tomod=paste("biomass ~ ",paste(m, collapse="+"),sep = "")
  mod=mod=lm(tomod, data=n)
  abline(mod, col="red")
  print(paste(m,a, sep=" for " ))
  sum=dplyr::select(glance(mod), adj.r.squared, sigma, AIC, BIC, p.value) 
  print(sum)
  }
}


#Adam's Validation Code:
#---------------------------------

calc_RMSE <- function(pred, obs){
  RMSE <- round(sqrt(mean((pred-obs)^2)),3)
  return(RMSE)
}


#cross validation

xVTweedie <- function(data=sL, model=f1, prop.training=.85,nruns=100) {
 
  
  nsamp = round(nrow(data)*prop.training)
  vec = 1:nrow(data)
  RMSE = c()
  test.data = list()
  for(i in 1:nruns) {
    a = sample(vec,nsamp)
    training = data[a,]
    test = data[which(!vec %in% a),]
    mod1 = gam(model,data=training,family = Tweedie(p=1.25,link=power(.1)))
    test$pred = predict(mod1,newdata = test,type='response')
    RMSE[i] =  calc_RMSE(test$pred,test$Lobs)
    test.data[[i]] = test
  }
  return(RMSE)
}


xVresults = xVTweedie()
mean(xVresults)

