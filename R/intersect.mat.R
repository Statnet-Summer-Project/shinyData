intersect.mat <-
function(mat1,mat2){
	if(!is.matrix(mat1))
		mat1 <- t(as.matrix(mat1))
	if(!is.matrix(mat2))
		mat2 <- t(as.matrix(mat2))
	match(intersect(apply(mat2,1,paste,collapse="_"), apply(mat1,1,paste,collapse="_")) ,apply(mat2,1,paste,collapse="_"))
}
