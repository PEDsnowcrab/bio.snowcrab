car.pred.plot=function(var=vn, year=year.assessment, p, dr=plot.dir, plt=plt, main, ...){
      fn=paste(var, year,"png", sep="." )
      png(file=paste(dr, fn, sep="/"), width=7, height=5, unit="in", res=1000)
      print(sc_carstm_plot( p=pB, res=res, vn=vn, main=main))
      dev.off()
      print(paste("file can be found at: ", dr,"/",fn, sep=""))
  }
