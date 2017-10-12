#~ t0 spaghetti plot for supp
load('t0.rda') 

pl2 <- function(k){
#~ 	X11()
	o <- skygrowths[[k]] 
	times <-  sapply( 1:ntres, function(k) {
		t <- o[,k]$time
		t - min(t)  
	})
	nes <-  sapply( 1:ntres, function(k) o[,k]$ne_ci[,2] )

	o2 <- bnprs[[k]] 
	
	times2 <-  sapply( 1:ntres, function(kk) -o2[,kk]$summary$time + max(o2[,kk]$summary$time) )
	nes2 <-  sapply( 1:ntres, function(kk) o2[,kk]$summary$mean )
	
	matplot( times , nes, type = 'l' , log = 'y', lty=1, add=FALSE, main = ns[k],  ylim = c(1, max(nes ) ) , xlim = c( 0, 30 ), col = 'black', lwd = .5
	 , xlab = 'Time', ylab = 'Effective population size', bty='n') 
	matplot( times2 , nes2, type = 'l' , add =TRUE, lty = 1, col = 'blue', lwd = .5)
}


simpl2 <- function()
{
#~ X11()
with( sim, plot( t, I , type = 'l', lty=1, col='red', xlim = c( 0, 30 ),
  , xlab = 'Time', ylab = 'Number infected' 
  , bty = 'n'
   , ylim = c(1e-1, 1500 )
   , log = 'y'
  ) )
for ( sim in sims[2:ntres] ) with (sim, lines( t, I , col = 'red' ) )	
}


svg( 'a0_2_spagh.svg' , width = 6, height = 5.5)
par(mfrow = c(2,2))
pl2(2) 
pl2(1) 
simpl2() 
dev.off()
