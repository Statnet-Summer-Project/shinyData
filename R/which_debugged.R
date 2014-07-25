which_debugged <-
function(objnames,ns=NULL) {
	if (!length(objnames)) return(character(0))
	objnames[sapply(objnames,isdebugged_safe,ns=ns)]
}
