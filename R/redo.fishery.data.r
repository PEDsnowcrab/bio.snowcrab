redo.fishery.data= function(x, yrs=p$year.assessment){ 
  
  logbook.db(  DS="rawdata.logbook.redo", yrs=yrs ) #  datadirectory ("bio.snowcrab"), "data", "logbook", "datadump"
  logbook.db(  DS="rawdata.licence.redo" ) # datadirectory ("bio.snowcrab"), "data", "logbook", "lic.datadump.rdata"
  logbook.db(  DS="rawdata.areas.redo" ) # datadirectory ("bio.snowcrab"), "data", "observer", "datadump"
  observer.db( DS="rawdata.redo", yrs=yrs )
  
  print("Data refreshed from databases for years above")
  print("Creating datafiles: takes up to 3 minutes")

  require(aegis.bathymetry)
 
  observer.db( DS="odb.redo", p=p ) # 3 minutes
  logbook.db( DS="logbook.redo", p=p )
  logbook.db( DS="logbook.filtered.positions.redo", p=p )
  # fishing ground are used for determination of contraints for interpolation
  logbook.db( DS="fishing.grounds.redo",  p=p )
  logbook.db( DS="logbook.gridded.redo", p=p )
  
  print("Data files updated")
  print("Can be found at: C:/bio.data/bio.snowcrab/data/logbook")
  print("following NA warnings are ok")
}

  
  
  
  
  
  
  
  
  
