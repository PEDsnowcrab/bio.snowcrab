require(ROracle)

map.loc=file.path("C:", "bio.data", "bio.snowcrab", "requests", "commercial.crab.survey")
wd=file.path("C:", "bio.data", "bio.snowcrab", "maps")

con= dbConnect(DBI::dbDriver("Oracle"), 'snowcrab', 'opilio99', 'ptran')
                   

 q= dbGetQuery(con, "SELECT SNCRABDETAILS.TRIP_ID, SNCRABDETAILS.TRIP, SNCRABDETAILS.BOARD_DATE,
 SNCRABDETAILS.SET_NO, SNCRABDETAILS.EST_NUM_CAUGHT, SNCRABDETAILS.EST_DISCARD_WT, SNCRABDETAILS.FISH_NO,
 SNCRABDETAILS.SEXCD_ID, SNCRABDETAILS.FISH_LENGTH, SNCRABDETAILS.MEASURED_WGT, SNCRABDETAILS.CALC_WGT,
 SNCRABDETAILS.FEMALE_ABDOMEN, SNCRABDETAILS.CHELA_HEIGHT, SNCRABDETAILS.MATURITY_CD, SNCRABDETAILS.SHELLCOND_CD,
 SNCRABDETAILS.GONADE_CD, SNCRABDETAILS.EGGCOLOR_CD, SNCRABDETAILS.EGGPERCENT_CD, SNCRABDETAILS.DUROMETRE,
 SNCRABDETAILS.BCD, SNCRABDETAILS.MISSING_LEGS, SNCRABSETS.START_LAT, SNCRABSETS.START_LONG,
 SNCRABSETS.END_LAT, SNCRABSETS.END_LONG
 FROM SNOWCRAB.SNCRABDETAILS SNCRABDETAILS, SNOWCRAB.SNCRABSETS SNCRABSETS
 WHERE SNCRABDETAILS.TRIP = SNCRABSETS.TRIP AND SNCRABDETAILS.SET_NO = SNCRABSETS.SET_NO")

names(q)=tolower(names(q))
q$year=years(q$board_date)
q=q[is.finite(q$fish_no),]

q$tripset=NA
q$tripset=paste(q$trip, q$set_no, sep=":")
q$numcom=1   #adds a variable to sum on by compute sums

### Need to change year to match year in question
iyear="2019"
i=which(q$year==iyear)
crab=q[i,]
com=crab[crab$fish_length>=95,]



 compute.sums = function (x, var, index) {
 for (i in index) x[,i] = as.factor(x[,i])
 res = as.data.frame.table( tapply( X=x[,var], INDEX=x[,index],
  	FUN=function(q) { sum(q, na.rm=T)}, simplify=T))
 names(res) = c(index, var)
 for (i in index) { res[,i] = as.character( res[,i] ) }
return(res) }

bigcom= compute.sums( x=com, var="numcom", index=c("tripset")  )
bigcom=bigcom[order(bigcom$numcom, decreasing=T),]



h=tapply(X=com$start_lat, INDEX= com$tripset, FUN= function(q){unique(q)[1]})
h=as.data.frame(h)
i=data.frame(lat=as.vector(h$h), tripset=dimnames(h)[[1]] )
i$tripset =  as.character(i$tripset)

ii=merge(x=bigcom, y=i, by="tripset", all.x=F, all.y=T, sort=F)


h=tapply(X=com$start_long, INDEX= com$tripset, FUN= function(q){unique(q)[1]})
h=as.data.frame(h)
i=data.frame(lon=as.vector(h$h), tripset=dimnames(h)[[1]] )
i$tripset =  as.character(i$tripset)

final=merge(x=ii, y=i, by="tripset", all.x=F, all.y=T, sort=F)

bigsets=final[c(1:15),]
bigsets$rank=1:nrow(bigsets)

filename=paste("bigcommercialsets",iyear, ".csv",sep="")
write.csv(bigsets,file=paste(map.loc, filename, sep="/")) #save a csv copy in case requested

names(bigsets) = c("tripset", "numcom", "Y", "X", "PID" )
bigsets$X=-(bigsets$X)

#surveydata$X = surveydata$X*-1
#surveydata$X2 = surveydata$X2*-1



plotRaster= function(path, xlab ="", ylab="", transcol = NULL, transamt = NULL, fade = 1, axes=F, tck= "",
                     tckLab=F, cellcount = NULL, xlim = NULL, ylim = NULL, quality = quality, ...){
   
   
   #x <- GDAL.open(path)
   # if(length(dim(x)) == 3){
   #    dx <- RGB2PCT(x, band=1:dim(x)[3])
   #    writeGDAL(fname = sub(".", "_PCT.", path), dx)
   #    GDAL.close(dx)
   #    r = raster(sub(".", "_PCT.", path))
   # }
   # else{
      r = raster(path)
   # }
   # GDAL.close(x)
   # 
   
   
   
   
   if(is.null(cellcount)) cellcount = ncell(r)*quality
   if(is.null(xlim) & is.null(ylim)){
      xlim <<- c(xmin(extent(r)), xmax(extent(r)))
      ylim <<- c(ymin(extent(r)), ymax(extent(r)))
   }
   
   if(grepl("longlat", projection(r))) labelProjection = "LL"
 #  else labelProjection = "UTM"
   plt = c(.1, .96, .1, .97)
   par(...)
   par(cex =1)
   # save settings in 'options'
   options(map.xlim = xlim);
   options(map.ylim = ylim);
   options(map.projection = labelProjection);
   
   # create plot region
   .initPlotRegion(projection=labelProjection, xlim=xlim, ylim=ylim, plt=plt);
   
   
   t_amt =  as.hexmode(round(fade*255))
   if(nchar(t_amt) == 1) t_amt = paste("0", t_amt, sep = "")
   r@legend@colortable = paste(r@legend@colortable, t_amt, sep = "" )
   r@legend@colortable = toupper(r@legend@colortable)
   hex = c("#","0","1","2","3","4","5","6","7","8","9","A", "B", "C", "D", "E", "F")
   if(!is.null(transcol)){
      if(length(transcol) == length(transamt)){
         ctab = r@legend@colortable
         for(k in 1:length(transcol)){
            t_amt =  as.hexmode(round(transamt[k]*255))
            if(nchar(t_amt) == 1) t_amt = paste("0", t_amt, sep = "")
            t_col = transcol[k]
            if(t_col %in% colors())
               ind = which(col2rgb(ctab)[1,] == col2rgb( t_col)[1] & col2rgb(ctab)[2,] == col2rgb( t_col)[2] & col2rgb(ctab)[3,] == col2rgb( t_col)[3])
            if(!FALSE %in% (unlist(strsplit(toupper(rgb), "")) %in% hex))
               ind2 = which(substr(ctab, 0, 7) == substr(t_col, 0, 7))
            if(length(ind) == 0 & length(ind2) == 0 ) print("Incorrect transparent color format")
            
            if(length(ind)>0) substr(r@legend@colortable[ind], 8, 9) = as.character(t_amt)
            if(length(ind2)>0) substr(r@legend@colortable[ind2], 8, 9) = as.character(t_amt)
         }
      }
      #else{ print("The number of transparent colors must equal the number of transparent amounts")}
   }
   
   plot(r, maxpixels = cellcount, add = T)
   
   if (axes) {
      .addAxis(xlim = xlim, ylim = ylim, tckLab = tckLab, tck = tck,
               tckMinor = 0.5 * tck);
      
   }
   
   # labels must go after axis
   
   .addLabels(projection = labelProjection, xlab = xlab, ylab = ylab);
   
}



makechart = function(quality = 1){
   q = quality
   direct = map.loc
   name=paste("big.commercial.sets",iyear, "jpg", sep=".")
   jpeg(filename = file.path(direct, name), width = 7750*quality, height = 4000*quality, pointsize = 120*quality)
   #tif(filename = file.path(direct, "big.commercial.sets",iyear, ".tif"), width = 10000*quality, height = 10000*quality, pointsize = 40*quality)
   xlim = c(-61.5, -57.5)
   ylim = c(43.75, 46)
   
   plotRaster(file.path(wd, "rasters", "801_LL_WGS84_PCT_clip.tif"),
              xlab="", main = "Commercial Crab in Survey", ylab="", outer = T, axes=T, tck=0,
              tckLab=F, xlim = xlim, ylim = ylim, quality = quality, cellcount = NULL)

   x = NULL
   
   #addlinesSCAREA()
  

   #ind = which(surveydata$X < min(xlim) )
   #if(length(ind)>0) surveydata = surveydata[-ind,]
   
   #ind = which(surveydata$X > max(xlim)) 
   #if(length(ind)>0) surveydata = surveydata[-ind,]
   
   #ind = which(surveydata$Y > max(ylim))
   #if(length(ind)>0) surveydata = surveydata[-ind,]
   
   #ind = which(surveydata$Y < min(ylim) )
   #if(length(ind)>0) surveydata = surveydata[-ind,]
   
   sd = as.PolyData(data.frame(bigsets), projection = "LL")
   
   q = quality
   
   addPoints(sd, cex = .8*q, lwd = 20*q, col = "red")

   sd$label = as.character(sd$PID)
   
   # for(i in 1:nrow(sd)){
   #    if(nchar(sd$label[i]) == 1) sd$label[i] = paste("00", sd$label[i], sep = "")
   #    if(nchar(sd$label[i]) == 2) sd$label[i] = paste("0", sd$label[i], sep = "")
   # }
   # 
   sd$X = sd$X+.05*q
   
   addLabels(sd, font = 2, cex = .4*q, col = "white")
   sd$X = sd$X+.001*q
   addLabels(sd, font = 2, cex = .4*q, col = "white")
   sd$X = sd$X-.002*q
   addLabels(sd, font = 2, cex = .4*q, col = "white")
   sd$X = sd$X+.001*q
   sd$Y = sd$Y+.001*q
   addLabels(sd, font = 2, cex = .4*q, col = "white")
   sd$Y = sd$Y-.002*q
   addLabels(sd, font = 2, cex = .4*q, col = "white")
   sd$Y = sd$Y+.001*q
   addLabels(sd, font = 1, cex = 1.0*q, col = "black")
   
  degAxis(1, lwd = 5*q)
   degAxis(2, lwd = 5*q)
   
   dev.off()
   
   # writeRaster(yourRasterObject, "outputFilename", format = "GTiff")
}

