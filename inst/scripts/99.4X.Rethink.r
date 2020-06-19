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

year.tot=aggregate(landings~year, data=xraw, FUN="sum")
    year.tot= year.tot[with(year.tot, order(year)),]


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
temps=do.call(rbind,outbnam)
temps= apply(temps,2,mean)

#to plot temp time trends (with Loess smooth)
op = stl(ts(temps,frequency=12),s.window=7)
plot(op)


annual.smooth.temps= op$time.series[,2] #trend
seasonality.smooth.temps= op$time.series[,1] #seasonal
		

write.csv(annual.smooth.temps,file=paste(outfile, '/bnam.climatology.csv', sep=''))
out = read.csv(file=paste(outfile, '/bnam.climatology.csv', sep=''))

  
write.csv(seasonality.smooth.temps,file=paste(outfile, '/bnam.seasonality.csv', sep=''))
out1 = read.csv(file=paste(outfile, '/bnam.seasonality.csv', sep=''))





# -------------------------------------------------
# Part 5 -- model with catch rates with temperature
  
  logsInSeason<-lobster.db('process.logs.redo')
  logsInSeason<-lobster.db('process.logs')
  
  write.csv(logsInSeason,file.path(project.datadirectory("bio.lobster"),'data',"Logs.csv"),row.names=F)
  
  cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2002:2019,graphic='R',export=F)
  cpueLFA.dat = CPUEplot(logsInSeason,lfa= p$lfas,yrs=2006:2019,graphic='pdf',path=figdir)
  cpueSubArea.dat = CPUEplot(logsInSeason,subarea= p$subareas,yrs=2006:2019,graphic='R')
  
  
  
  ## Commercial CPUE MOdels
  mf1 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + DOS * TEMP)
  mf2 = formula(logWEIGHT ~ fYEAR + DOS + TEMP)
  mf3 = formula(logWEIGHT ~ fYEAR + DOS)
  mf4 = formula(logWEIGHT ~ fYEAR + TEMP)
  #mf5 = formula(logWEIGHT ~ fYEAR + DOS + TEMP + (1 | fYEAR/fAREA)) # combined
  
  
  TempModelling = TempModel( annual.by.area=F)
  #CPUE.data<-CPUEModelData(p,redo=T,TempModelling)
  CPUE.data<- CPUEModelData(p,redo=F)
  CPUE.data=subset(CPUE.data,!(LFA==35&SYEAR<2006)) #exclude partial year of data in 35
  CPUE.data=subset(CPUE.data,!(LFA==36&SYEAR<2005)) #exclude partial year of data in 36
  
  #Modelled Temperature at first day of season
  pL=0
  t=c()
  d=c()
  k=1
  for(i in 1:length(p$lfas)){
    for(j in 2:length(p$yrs)){
      Cdat=subset(CPUE.data,LFA==p$lfas[i]&SYEAR==p$yrs[j])
      Cdat=Cdat[order(Cdat$DATE_FISHED),]
      Cdat$pLanded = cumsum(Cdat$WEIGHT_KG)/sum(Cdat$WEIGHT_KG)
      if(nrow(Cdat)>1){
        x=abs(Cdat$pLanded-pL)
        d[k]=Cdat$DOS[which(x==min(x))]
        t[k]=Cdat$TEMP[which(x==min(x))]
        names(d)[k]=paste(p$lfas[i],p$yrs[j],sep='.')
        names(t)[k]=paste(p$lfas[i],p$yrs[j],sep='.')
        k=k+1
      }
    }
  }
  
  ##t and d Overwrite with summary
  t=with(subset(CPUE.data,DOS==1),tapply(TEMP,LFA,mean))
  d=1
  
  aicc = function(aa = model.output) {
    k = 	attr(logLik(aa),'df')
    logLiks = logLik(aa)[1]
    n = nobs(aa)
    aaa = 2*nParams-2*logLiks
    aaa + (2*k^2+2*k) / (n-k-1)
  }
  
  
  pData=list()
  
  CPUEModelResults1 = list()
  CPUEModelResults2 = list()
  CPUEModelResults3 = list()
  CPUEModelResults4 = list()
  AICs1 = c()
  AICs2 = c()
  AICs3 = c()
  AICs4 = c()
  for(i in 1:length( p$lfas)){
    
    mdata = subset(CPUE.data,LFA==p$lfas[i]&SYEAR%in%p$yrs)
    CPUEModelResults1[[i]] = CPUEmodel(mf1,mdata,t=t[i],d=d)
    CPUEModelResults2[[i]] = CPUEmodel(mf2,mdata,t=t[i],d=d)
    CPUEModelResults3[[i]] = CPUEmodel(mf3,mdata,t=t[i],d=d)
    CPUEModelResults4[[i]] = CPUEmodel(mf4,mdata,t=t[i],d=d)
    AICs1[i] = aicc(CPUEModelResults1[[i]]$model)
    AICs2[i] = aicc(CPUEModelResults2[[i]]$model)
    AICs3[i] = aicc(CPUEModelResults3[[i]]$model)
    AICs4[i] = aicc(CPUEModelResults4[[i]]$model)
    
    
  }
  names(CPUEModelResults1) = p$lfas
  names(CPUEModelResults2) = p$lfas
  names(CPUEModelResults3) = p$lfas
  names(CPUEModelResults4) = p$lfas
  
  AICs = data.frame(rbind(AICs1,AICs2,AICs3,AICs4))
  names(AICs) = p$lfas
  AICs
  AICtable=sweep(AICs,2,FUN='-',apply(AICs,2,min))
  
  pData=list()
  for(i in 1:length( p$lfas)){
    pData[[i]]=CPUEModelResults1[[i]]$pData
    pData[[i]]$LFA=p$lfas[i]
  }
  
  CPUEindex=do.call("rbind",pData)
  
  
  write.csv(AICtable,file.path( figdir,"CPUEmodelAIC.csv"),row.names=F)
  write.csv(CPUEindex,file.path( figdir,"CPUEmodelindex.csv"),row.names=F)
  
  
  
  #CPUECombinedModelResults = CPUEmodel(mf5,CPUE.data,combined=T)	
  
  cpue1= CPUEModelPlot(CPUEModelResults1,TempModelling,lfa = p$lfas,xlim=c(1989,2018.4),ylim=c(0,10.5),graphic='R',path=figdir,lab=1,wd=11,ht=8)
  
  
  ty=sapply(1:length(p$lfas),function(x){with(subset(CPUE.data,DOS==1&LFA==p$lfas[x]),tapply(TEMP,SYEAR,mean))})
  fs=lobster.db('season.dates')
  d=merge(data.frame(LFA=rep(p$lfas,lapply(ty,length)),SYEAR=names(unlist(ty)),TEMP2=unlist(ty)),fs[,c('LFA','SYEAR','START_DATE')])
  names(d)[2]="YEAR"
  
  
  CPUEindex = merge(d,CPUEindex)
  
  m2=subset(cpue1,DOS==1,c("LFA","YEAR","mu"))
  names(m2)[3]="mu2"
  
  CPUEindex = merge(CPUEindex,m2)
  
  write.csv(subset(CPUEindex,LFA==34&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex34.csv"),row.names=F)
  write.csv(subset(CPUEindex,LFA==35&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex35.csv"),row.names=F)
  write.csv(subset(CPUEindex,LFA==36&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex36.csv"),row.names=F)
  write.csv(subset(CPUEindex,LFA==38&YEAR%in%2005:2018,c("LFA","YEAR","START_DATE","TEMP2","TEMP","mu","mu2")),file.path( figdir,"figures","Brad","CPUEmodelindex38.csv"),row.names=F)
  
  #pdf2png(file.path(figdir,"CPUEmodel1"))
  cpueLFA.dat = CPUEplot(CPUE.data,lfa= p$lfas,yrs=1989:2018,graphic='png',export=T,path=figdir)
  
  cpue.annual=list()
  for(i in 1:length(p$lfas)){
    MU=c()
    MU.sd=c()
    for(j in 1:length(p$yrs)){
      MU[j]=with(subset(cpue1,LFA==p$lfas[i]&YEAR==p$yrs[j]),weighted.mean(mu,WEIGHT_KG))
      MU.sd[j]=with(subset(cpue1,LFA==p$lfas[i]&YEAR==p$yrs[j]),sqrt(sum(WEIGHT_KG/sum(WEIGHT_KG) * (mu - MU[j])^2)))
    }
    
    #MU=with(subset(cpue1,LFA==p$lfas[i]),tapply(mu,YEAR,weighted.mean,WEIGHT_KG))
    #MU.sd=with(subset(cpue1,LFA==p$lfas[i]),tapply(mu,YEAR,sd))
    #xm <- weighted.mean(x, wt)y6723
    #v <- sum(wt * (x - xm)^2)
    cpue.annual[[i]] = data.frame(Area=p$lfas[i],Year=p$yrs,CPUE=MU,CPUE.ub=MU+MU.sd,CPUE.lb=MU-MU.sd)
    
    
    #cpue.annual[[i]] = with(CPUEModelResults1[[i]]$pData,data.frame(Area=p$lfas[i],Year=YEAR,CPUE=mu,CPUE.ub=ub,CPUE.lb=lb))
    
    
    
  }
  cpueModel = subset(do.call("rbind",cpue.annual),Year<2019)
  
  
  #x11()
  #pdf(file.path( figdir,"CPUEmodelAnnualIndex.pdf"),8, 10)
  par(mfrow=c(length(p$lfas),1),mar=c(0,0,0,0),omi=c(0.5,1,0.5,0.5),las=1)
  
  for(i in 1:length(p$lfas)){
    
    
    plot(mu~YEAR,CPUEModelResults1[[i]]$pData,type='b',pch=21,bg='red',ylim=c(0,7),xlim=c(1989,2018),xaxt='n')
    points(CPUE~YEAR,subset(cpueLFA.dat$annual.dat,LFA==p$lfas[i]&YEAR<2019),pch=16,col='blue',cex=0.9)
    #lines(ub~YEAR,CPUEModelResults1[[i]]$pData,lty=2)
    #lines(lb~YEAR,CPUEModelResults1[[i]]$pData,lty=2)
    axis(1,lab=F)
    axis(4)
    if(i==length(p$lfas))axis(1)
    
    text(1989,6,paste(p$lfas[i]),cex=2,pos=4)
  }
  mtext("CPUE (kg/TH)", 2, 3, outer = T, cex = 1,las=0)	
  dev.off()
  
  cpueData2=    CPUEplot(CPUE.data,lfa= p$lfas,yrs=1981:2018,graphic='R')$annual.data
  
  save(list=c("cpueModel","cpueData2"),file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators3438.rdata"))
  save(cpueData2,file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators3438_2.rdata"))
  #write.csv(cpueLFA.dat$annual.data,"CPUEannualData.csv",row.names=F)
  #write.csv(na.omit(cpueLFA.dat$daily.data),"CPUEdailyData.csv",row.names=F)
  
  load(file=file.path(project.datadirectory("bio.lobster"),"outputs","cpueIndicators3438.rdata"))
  
  # Landings and Effort ############
  
  land = lobster.db('seasonal.landings')
  land$YEAR = as.numeric(substr(land$SYEAR,6,9))
  
  for(i in 1:length(p$lfas)){
    
    d1 = data.frame(YEAR = land$YEAR, LANDINGS = land[,paste0("LFA",p$lfas[i])])
    d2 = subset(cpueData2,LFA==p$lfas[i])
    
    d2  = merge(data.frame(LFA=d2$LFA[1],YEAR=min(d2$YEAR):max(d2$YEAR)),d2,all.x=T)
    
    fishData = merge(d2,d1) 
    fishData$EFFORT2 = fishData$LANDINGS * 1000 / fishData$CPUE
    
    # plot
    x11(width=8,height=5)
    FisheryPlot(fishData[,c("YEAR","LANDINGS","EFFORT2")],lfa = p$lfas[i],fd=figdir)
  }
  
  


#Simple plotting of Ref Pts and HCR

bmsy=median(x4$cpue)
plot(x4$year, x4$cpue, ylim=c(0,1.1*(max(x4$cpue))), type="l", xlab="Year", ylab="CPUE (kg)", main="4X")
points(x4$year,x4$cpue, pch=16)
abline(h=0.4*bmsy, col="red", lwd=3)
abline(h=0.8*bmsy, col="yellow", lwd=3)
abline(h=bmsy)
text(x=min(as.numeric(x4$year))+1, y=1.1*median(x4$cpue), "BMSY")