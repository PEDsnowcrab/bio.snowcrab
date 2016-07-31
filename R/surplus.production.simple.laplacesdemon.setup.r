


surplus.production.simple.laplacesdemon.setup = function(Data) {
  
  # set up model for a simple surplus production model 
  # to be solved by the Rlibrary: LaplacesDemon (or alternately via penalized Maximum Likelihood) 


  # ----------------------------------
  # Identify location and number of missing values -- prediction locations are treated the same way

  Data$N = Data$Ndata + Data$Nforecasts # no years with data + projections

  # ----------------------------------

  Data$Missing = list(
    O = list( 
      n=length(intersect( 1:Data$Ndata, which( !is.finite(Data$O) ) )), 
      idx=intersect( 1:Data$Ndata, which( !is.finite(Data$O) ) ) 
    ), # observations of survey index
    removals = list( 
      n=length(intersect( 1:Data$Ndata, which( !is.finite(Data$removals) ) ) ), 
      idx=intersect( 1:Data$Ndata, which( !is.finite(Data$removals) ) )  
    )  # observations of removals
  )

  # ----------------------------------

  if ( Data$Nforecasts > 0 ) {
    # extend data for forecasts
    Data$O = c( Data$O, rep(NA, Data$Nforecasts) )
    Data$removals = c( Data$removals, rep(NA, Data$Nforecasts) )
    Data$Forecasts = list( 
      n = Data$Nforecasts,
      idx = Data$Ndata + 1:Data$Nforecasts,
      idx_1 = (Data$Ndata + 1:Data$Nforecasts) -1 
    )
  }

  # ----------------------------------
  # misc indexing
  Data$idx = list(
    t1 = 1,
    t2 = 2:(Data$ty-1),
    t2_1 = (2:(Data$ty-1) ) - 1,
    t3 =  Data$ty,
    t3_1 =  Data$ty-1,
    t4 =  (Data$ty+1):Data$N,
    icurrent  = 2:Data$N,
    iprevious = 1:(Data$N-1),
    idata = 1:Data$Ndata
  )

  # ----------------------------------
  # paramater names and initial values
  Data$parm.names = as.parm.names( list(
    r = Data$r0,
    K = Data$K0,
    q = Data$q0,
    S_sd = Data$cv,
    O_sd = Data$cv,
    S = rep( Data$S0, Data$N),
    S0 = Data$S0
  ) )

  # ----------------------------------
  # index position of paramaters
  Data$pos = list(
    r=grep("\\<r\\>", Data$parm.names),
    K=grep("\\<K\\>", Data$parm.names),
    q=grep("\\<q\\>", Data$parm.names),
    S_sd=grep("\\<S_sd\\>", Data$parm.names),
    O_sd=grep("\\<O_sd\\>", Data$parm.names),
    S=grep("\\<S\\>", Data$parm.names),
    S0=grep("\\<S0\\>", Data$parm.names)
  )
 
  # ----------------------------------
  # monitoring nodes
  Data$mon.names = c("LP", "r", "K", "q" )
  # Data$mon.names = c("LP", "r", "K", "q", paste0("S",1:Data$N), paste0("AR",1:(Data$N-1) ) )


  # ----------------------------------
  # Parameter Generating Function
  # these parameters are operate on the log-scale to force positive values ...
  Data$PGF = function(Data) {
    r=runif(1, Data$eps, 3 )
    K=runif(1, Data$K0/2, Data$K0*2 ) # log scale
    q=runif(1, Data$eps, 2 )
    S_sd=runif( 1, Data$eps, 1 );
    O_sd=runif( 1, Data$eps, 1);
    S=runif( Data$N, Data$eps, 1 ) 
    S0=runif(1, Data$eps, 1 ); 
    out = c(r, K, q, S_sd, O_sd, S, S0)
    return( out  )
  }
  Data$PGF = compiler::cmpfun(Data$PGF)


  # ----------------------------------
  # define the model that generates the loglikelihoods (LL) and the log-posteriors (LP)
  Data$Model = function(parm, Data) {
    
    Spred = Opred = rep(0, Data$N )   # initialize a few storage vectors
      # NOTE: for lognormal: cv = sqrt(exp(sigma^2) - 1); 
    # or sigma = sqrt(log(cv^2+ 1) ) ==> sigma = sqrt( log(0.25^2 + 1)) = 0.246 ~ cv -- i.e. cv ~ sd
#    if(0) {   
      parm[Data$pos$q] = LaplacesDemonCpp::interval( parm[Data$pos$q], Data$eps, 2 );
      parm[Data$pos$r] = LaplacesDemonCpp::interval( parm[Data$pos$r], Data$eps, 3 );
      parm[Data$pos$K] = LaplacesDemonCpp::interval( parm[Data$pos$K], Data$K0/2, Data$K0*2 );
      parm[Data$pos$S0] = LaplacesDemonCpp::interval( parm[Data$pos$S0], Data$eps, Data$smax );
      parm[Data$pos$S] = LaplacesDemonCpp::interval( parm[Data$pos$S], Data$eps, Data$smax );
 #    }

    parm[Data$pos$S_sd] = LaplacesDemonCpp::interval( parm[Data$pos$S_sd], Data$eps, 1 );
    parm[Data$pos$O_sd] = LaplacesDemonCpp::interval( parm[Data$pos$O_sd], Data$eps, 1 );
    
    q = parm[Data$pos$q]
    r = parm[Data$pos$r]
    K = parm[Data$pos$K]
    
    S = parm[Data$pos$S] 
    O_sd = parm[Data$pos$O_sd] 
    S_sd = parm[Data$pos$S_sd] 
    Spred[1] = parm[Data$pos$S0] 

    
    loglik.imputed = 0

    loglik = c()
    loglik[Data$parm.names] = 0
    loglik[Data$pos$q] = dnorm( log(parm[Data$pos$q]), log(Data$q0), Data$cv, TRUE ) ;
    loglik[Data$pos$r] = dnorm( log(parm[Data$pos$r]), log(Data$r0), Data$cv, TRUE ) ;
    loglik[Data$pos$K] = dnorm( log(parm[Data$pos$K]), log(Data$K0), Data$cv, TRUE ) ;
    loglik[Data$pos$S0] = dnorm( log(parm[Data$pos$S0]), log(Data$S0), Data$cv, TRUE ) ;
    loglik[Data$pos$O_sd] = LaplacesDemonCpp::dhalfcauchy( parm[Data$pos$O_sd], Data$cv, TRUE );
    loglik[Data$pos$S_sd] = LaplacesDemonCpp::dhalfcauchy( parm[Data$pos$S_sd], Data$cv, TRUE );

    R = Data$removals/K ;# make sure it is producing sensible values:
    # impute missing data 

    if ( Data$Missing$removals$n > 0 ) {
      R[Data$Missing$removals$idx ] = rlnorm( Data$Missing$removals$n, Data$log_removals0, sd=Data$cv )
      loglik.imputed = loglik.imputed + dnorm( log(R[Data$Missing$removals$idx ]), Data$log_removals0, Data$cv, TRUE ) ;
    }
    if ( Data$Missing$O$n > 0 ) {
      Data$O[Data$Missing$O$idx ] = rlnorm(Data$Missing$O$n, log(Opred[Data$Missing$O$idx]), sd=Data$cv)
      loglik.imputed = loglik.imputed + dnorm( log(Data$O[Data$Missing$O$idx ]), Data$log_O0, Data$cv, TRUE ) ;    
    }

    AR = 1.0 + r*{1-S[Data$idx$iprevious]} ;  # "autocorelation" component
    Spred[ Data$idx$icurrent] = S[Data$idx$iprevious] * AR - R[Data$idx$iprevious] ;  # simple logistic
    Spred = bio.utilities::truncate.vector( Spred, lower=Data$eps ) 
    loglik[Data$pos$S] = dnorm( log(S), log(Spred), S_sd, TRUE ) 

    if ( Data$Forecasts$n > 0 ) {
      R[Data$Forecasts$idx] = S[Data$Forecasts$idx_1] * Data$er 
      loglik.imputed = loglik.imputed + dnorm( log(R[Data$Forecasts$idx]), Data$log_removals0, Data$cv, TRUE ) ;
    }

    # Likelihoods for observation model
    Opred[Data$idx$t1] = S[Data$idx$t1] - R[Data$idx$t1] ; # first year approximation
    Opred[Data$idx$t2] = S[Data$idx$t2] - R[Data$idx$t2_1] ;
    Opred[Data$idx$t3] = S[Data$idx$t3] - (R[Data$idx$t3_1] + R[Data$idx$t3])/2 ; # transition year from Spring to Autumn survey
    Opred[Data$idx$t4] = S[Data$idx$t4] - R[Data$idx$t4] ;
    Opred = K*q*Opred
    Opred = bio.utilities::truncate.vector( Opred, Data$eps, Data$Omax )

    ll_obs = dnorm( log(Data$O[Data$idx$idata]), log(Opred[Data$idx$idata]), O_sd, TRUE )
    if (any(!is.finite(ll_obs))) browser()

    # additional computed variables of interest 
    ER = R / S ;
    ER = bio.utilities::truncate.vector( ER, Data$eps, Data$eps_1 ) 
    B = S*K
    C = R*K
    F = -log( 1 - ER) ; # fishing mortality
          
    LL = sum( ll_obs )  # log likelihood (of the data)
    LP = LL + sum( loglik ) + sum(loglik.imputed) # log posterior
    out = list( LP=LP, Dev=-2*LL, Monitor=c(LP, r, K, q), yhat=S*K, parm=parm )
    return( out  )
  }

  Data$Model.ML  = compiler::cmpfun( function(...) (Data$Model(...)$Dev / 2) )  # i.e. - log likelihood
  Data$Model.PML = compiler::cmpfun( function(...) (- Data$Model(...)$LP) ) #i.e., - log posterior 
  Data$Model = compiler::cmpfun(Data$Model) #  byte-compiling for more speed .. use RCPP if you want more speed

  return (Data)

}

