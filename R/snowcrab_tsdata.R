snowcrab_tsdata = function( p, assessment_years=2000:p$year.assessment, areas=c("cfanorth", "cfasouth", "cfa4x"), carstm_model_label="production" ) {

  cfanorth =  1 # column index
  cfasouth =  2 # column index
  cfa4x =  3 # column index

  L = landings.aggregate( format="bugs" )
  L = as.data.frame( L[ match( assessment_years, rownames(L) ), areas ] )

  # biomass data: post-fishery biomass are determined by survey B)
  pSC = bio.snowcrab::snowcrab_carstm( DS="parameters", assessment.years=2000:p$year.assessment )

  B = carstm::carstm_summary(p=pSC, operation="load_timeseries", carstm_model_label=carstm_model_label  )

  rownames(B) = B$yrs
  B = as.data.frame( B[ match( assessment_years, B$yrs ), areas ] )

  # cfa4x have had no estimates prior to 2004

  cfanorth.baddata = which( assessment_years <= 1997 )
  B[ cfanorth.baddata, cfanorth ] = NA

  cfasouth.baddata = which( assessment_years <= 1998 )
  B[ cfasouth.baddata, cfasouth ] = NA

  cfa.nodata =   which( assessment_years <= 2003 )
  B[ cfa.nodata , cfa4x ] = NA

  sb = list(
    IOA = as.matrix(B), # observed index of abundance
    CAT = as.matrix(L) , # catches  , assume 20% handling mortality and illegal landings
    er = 0.2,  # target exploitation rate
    U = ncol( B),  # number of regions
    N = nrow( B) , # no years with data
    M = 3, # no years for projections
    ty = which(assessment_years == 2004),  # index of the transition year (2004) between spring and fall surveys
    cfa4x = cfa4x, # index of cfa4x
    eps = 1e-4  # small non-zero number
  )

  sb$missing = ifelse( is.finite(sb$IOA), 0, 1)
  sb$missing_n = colSums(sb$missing)
  sb$missing_ntot = sum(sb$missing_n)

  sb$IOA[ which(!is.finite(sb$IOA)) ] = 0 # reset NAs to 0 as stan does not take NAs
  sb$CAT[ which(!is.finite(sb$CAT)) ] = 1e-6 # remove NA's


  return(sb)

}

