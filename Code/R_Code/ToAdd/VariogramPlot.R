
#----------------------------------------------------------------------
##################
# CHUNK 
################## 
# Variogram Plot and Fit

## VDlist should be a list of 2 elements from VarioJ
    ## VDlist$Dist is a 1xk matrix of distances
    ## VDlist$Vario is a 1xk matrix of (ei-ej)^2
    ## note k=i*(j-1)

#### Hexbins for Plot, LOESS, OLS
hb   <- hexbin(VJList$Dist, VJList$Vario, xbins = 1E4)
pseq <- seq(hb@xbnds[1] + 1, hb@xbnds[2] - 1, length = 100)

form <- formula(exp(hb@ycm) ~ hb@xcm)

#### Weighted LOESS
ll.fit  <- loess(form,
    weights = hb@count,
    span=0.5,
    degree=1, control=loess.control(
        surface="interpolate",
        trace.hat="approximate"))
ll.pred <- predict(ll.fit, pseq)

#### Weighted OLS
llm.fit  <- lm( form, weights = hb@count)
llm.pred <- log( cbind(1, pseq) %*% coef(llm.fit) ) #

## Variogram Plot: Hexbin Weighted Loess and OLS
pdfname <-  paste0(fdir, "Variogram_Loess.pdf")
pdf(pdfname, width=7, height=7)
	#par(mfrow=c(1,1))
	xlb <- "distance( i, j )"
	ylb <- "(e_i - e_j)^2"
	hplt   <- plot(hb, xlab=xlb, ylab=ylb)

	plot.new()
	plot.window(xlim=range(hb@xcm), ylim=range(ll.pred))
	axis(1); axis(2)
	mtext(xlb, 1); mtext(ylb,2)
	lines(pseq, ll.pred, col=rgb(0,0,1,.5), lty=1)

	plot.new()
	plot.window(xlim=range(hb@xcm), ylim=range(llm.pred))
	axis(1); axis(2)
	mtext(xlb, 1); mtext(ylb,2)
	lines(pseq, llm.pred, col=rgb(0,0,1,.5), lty=2)

	vals <- formatC(coef(llm.fit), format="E", digits=2)
	mtext(paste0(vals, c(" ", " Dist"), collapse="+ " ), 3)
dev.off()

