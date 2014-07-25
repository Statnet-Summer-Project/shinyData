runinput <-
function(fun,para=FALSE){
	t1 <- paste(names(formals(fun)),"<<-",formals(fun))
	for (i in 1:length(t1)){
		try(eval(parse(text=t1[i])))
	}
	eval(parse(text=paste("args(",para,")")))

	if(para!=0)
	try(eval(parse(text=paste(gsub(",",";",para)))))
}
