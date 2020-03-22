
## GEVIT Link Function (Gumbell by Default)
gev_link  <- compiler::cmpfun( function(mu, loc0, scale0, shape0){ 
    evd::qgev(mu, loc=loc0, scale=scale0, shape=shape0)
})

gev_linkinv  <- compiler::cmpfun( function(eta, loc0, scale0, shape0){ 
    pmax(.Machine$double.eps, evd::pgev(eta, loc=loc0, scale=scale0, shape=shape0) )
})

gev_mu.eta   <- compiler::cmpfun( function(eta, loc0, scale0, shape0){ 
    pmax( evd::dgev(eta, loc=loc0, scale=scale0, shape=shape0), .Machine$double.eps) 
})

gevit <- compiler::cmpfun( function(loc0=0, scale0=1, shape0=0) {

	structure(list(
	    linkfun  = function(mu)  {gev_link(mu, loc0, scale0, shape0 )},
	    linkinv  = function(eta) {gev_linkinv(eta, loc0, scale0, shape0 )},
        mu.eta   = function(eta) {gev_mu.eta(eta, loc0, scale0, shape0 )},
        valideta = function(eta) {TRUE},
        name     = paste0("gev(", loc0, ",",scale0, ",", shape0, ")" ),
    class = "link-glm") )
})


GUMBIT <- lapply( FORMS_BIN, function(form) {
    gumbit <- glm(form, family=binomial(link = gevit(0,1,0)),
        data=ALL, x=FALSE, maxit=1E4)
    return(gumbit)
})

