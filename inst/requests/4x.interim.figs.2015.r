
p = snowcrab::initialise.local.environment()

a = snowcrab.db( DS="set.merge.cat" )
a = get.time.series(x=a, regions='cfa4x', vars=c('R0.mass','t'), trim=0, from.file=F, outfile=NULL,reduced.stations=F)
   figure.timeseries.R0( outdir=file.path(p$annual.results, "timeseries", "survey"),infile = a,specific.area='cfa4x' )
   figure.timeseries.t( outdir=file.path(p$annual.results, "timeseries", "survey"),infile = a,specific.area='cfa4x' )
