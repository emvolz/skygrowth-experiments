require(skygrowth)
require(phylodyn) 
require(ape)
require(parallel)

tres <- read.tree( 'sir1.nwk') 
num_ns <- 2
ns <- floor( seq(200, 1000, l = num_ns ) )
ntres <- 5
MST <- 30 




{	
	lapply( ns ,function(n) {
	mcmapply( function(tr){
		d2r <- node.depth.edgelength( tr )[1:length(tr$tip.label) ] 
		keep <- tr$tip.label [ d2r < MST ]
		tr <- drop.tip( tr, setdiff( tr$tip.label, keep )  )
		ntip <- length(tr$tip.label)
		N <- ntip - n
		tr <- drop.tip( tr, sample( tr$tip.label, size = N, replace=F) )
		skygrowth.mcmc( tr, res = 50, tau0=.10, tau_logprior='exponential')
	}, tres[1:ntres] ,mc.cores = ntres)
	} ) -> skygrowths
	
	lapply( ns ,function(n) {
	mcmapply( function(tr){
		d2r <- node.depth.edgelength( tr )[1:length(tr$tip.label) ] 
		keep <- tr$tip.label [ d2r < MST ]
		tr <- drop.tip( tr, setdiff( tr$tip.label, keep )  )
		ntip <- length(tr$tip.label)
		N <- ntip - n
		tr <- drop.tip( tr, sample( tr$tip.label, size = N, replace=F) )
		BNPR(tr)
	}, tres[1:ntres] ,mc.cores = ntres)
	} ) -> bnprs
	
}
