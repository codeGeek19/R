x=matrix(nrow=4, ncol=4, data=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 12, 30, 16))
rownames(x)=c("s1", "s2", "s3", "s4")
colnames(x)=c("t1", "t2", "t3", "t4")
x

y=matrix(nrow=4, ncol=4, data=17:32)
rownames(y)=c("p1", "p2", "p3", "p4")
colnames(y)=c("q1", "q2", "q3", "q4")
y



#Crossproduct of Matrices
crossprod(x)

#Concatenating Matrices

rbind(x, y)
cbind(x, y)

#Invesre of a Matrix
solve.default(x)