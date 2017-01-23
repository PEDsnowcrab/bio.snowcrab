  map.set.information = function(p, outdir, variables,mapyears, plot.method="levelplot",interpolate.method='tps', theta=p$pres*25, idp=2,log.variable=T,add.zeros=T,minN=10  , probs=c(0.025, 0.975), offset) {

    set = snowcrab.db( DS="set.biologicals")
    if(missing(variables)){
      variables = variable.list.expand("all.data")
      variables = intersect( variables, names(set) )
    }

   #variables = c('totmass.male.com', 'totmass.female.mat')

    if (plot.method =="gmt") {
      # overrides to defaults
      p$tension = "-T.4"  # 0.35+ for steep; 0.25 for smooth
      p$maskres = "-S16k"
      p$interpres = "-nb"
      outdir = file.path( outdir, p$spatial.domain )
      gmt.map.variables( set, p=p, variables=variables, plottimes=p$plottimes, basedir=outdir, conversions=p$conversions )
      return("Done")
    }


      # define compact list of variable year combinations for parallel processing
      if(missing(mapyears))mapyears = sort( unique(set$yr) )
      p = make.list( list(variables, mapyears ), Y=p )

      #for (i in p$libs ) require( i )

      id = 1: p$nruns

      predlocs = bio.bathymetry::bathymetry.db(p=p, DS="baseline")

      for (i in id ) {
        v = p$runs[i,1]
        y = p$runs[i,2]
        outfn = paste( v,y, sep=".")
        outloc = file.path( outdir,v)
        print(paste(p$runs[i,]))

        set_xyz = set[ which(set$yr==y), c("plon","plat",v) ]
        names( set_xyz) = c("plon", "plat", "z")
        set_xyz = na.omit(subset(set_xyz,!duplicated(paste(plon,plat))))
        if(nrow(set_xyz)<minN)next() #skip to next variable if not enough data

        if(missing(offset))offset = empirical.ranges( db="snowcrab", v, remove.zeros=T , probs=0)  # offset fot log transformation
        er = empirical.ranges( db="snowcrab", v, remove.zeros=T , probs=probs)  # range of all years
        ler = er

        if(log.variable){
          set_xyz$z = log(set_xyz$z+offset)
          ler=log(er+offset)
          #if(offset<1)if(shift) xyz$z = xyz$z + abs(log(offset))
       }
        xyzi = na.omit(set_xyz)
        
        if(nrow(xyz)<minN||is.na(er[1]))next() #skip to next variable if not enough data
        
        datarange = seq( ler[1], ler[2], length.out=50)
        corners = data.frame(rbind( cbind( plon=c(220, 990), plat=c(4750, 5270) )))

        #browser()

        #!# because 0 in log space is actually 1 in real space, the next line adds the log of a small number (offset)
        #!# surrounding the data to mimic the effect of 0 beyond the range of the data
        if(add.zeros)  xyzi =na.omit( zeroInflate(set_xyz,corners=corners,type=2,type.scaler=0.5,eff=log(offset),blank.dist=20) )

        if(interpolate.method=='tps'){

          u= fastTps(x=xyzi[,c("plon","plat")] , Y=xyzi[,'z'], theta=theta )
          res = cbind( predlocs[,1:2], predict(u, xnew=predlocs[,1:2]))
        }
        if(interpolate.method=='idw'){
          u = gstat(id = "z", formula = z ~ 1, locations = ~ plon + plat, data = xyzi, set = list(idp = idp))
          res = predict(u, predlocs[,1:2])[,1:3]
        }
        #print(summary(set_xyz))
        #print(summary(res))

        xyz = res
        names( xyz) = c("plon", "plat", "z")
        #if(shift)xyz$z = xyz$z - abs(log(offset))
       
        
        cols = colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"), space = "Lab")

        xyz$z[xyz$z>ler[2]] = ler[2]

      if (plot.method=="levelplot") {

        ckey=NULL
      
        if(log.variable){
          # create labels for legend on the real scale
          labs=as.vector(c(1,2,5)%o%10^(-4:5))
          labs=labs[which(labs>er[1]&labs<er[2])]
          ckey=list(labels=list(at=log(labs+offset),labels=labs,cex=2))
        }

       try( map( xyz, xyz.coords="planar", cfa.regions=T, depthcontours=T, pts=set_xyz[,c("plon","plat")], annot=y, fn=outfn, loc=outloc, at=datarange , col.regions=cols(length(datarange)+1), colpts=F, corners=p$planar.corners, display=F,colorkey=ckey))
      }
      if (plot.method=="pbsmapping") {}

  }

  return("Done")
}


