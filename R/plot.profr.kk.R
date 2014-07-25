plot.profr.kk <-
function(x,xlab=NULL, ..., minlabel = 0.1, angle = 0,xlim=range(x$start, x$end)) {
	plot(1,1, xlim=xlim,  ylim=range(x$level)+c(-0.5, 0.5), type="n", ..., xlab=xlab, ylab="level")
	rect(x$start, x$level - 0.5, x$end, x$level +0.5, ...)
	labels <- subset(x, time > max(time) * minlabel)
	if (nrow(labels) > 0)
		text(labels$start, jitter(labels$level,factor=2), labels$f, pos=4, srt=angle, ...)
}
