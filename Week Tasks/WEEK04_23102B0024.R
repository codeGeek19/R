# **WEEK04** #


#Matrix Initalization
x=matrix(nrow=4, ncol=4, data=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
x
#data to be filled row wise: nrow(first)
#data to be filled column wise: ncol(first)

#Accessing Matrix
x[2, 2]

#dim() -ension of a Matrix
dim(x)

#Storage of Matrix
mode(x)

#rownames() and colnames()
rownames(x)=c("s1", "s2", "s3", "s4")
colnames(x)=c("t1", "t2", "t3", "t4")
x

#Matrix with Same Elements
y=matrix(nrow=4, ncol=4, data=24)
y

#diag() -onal Matrix
d = diag(5, nrow=3, ncol=3)
d

#Transpose of a Matrix
z = matrix(nrow=4, ncol=4, data=1:16, byrow=T)
rownames(z)=c("t1", "t2", "t3", "t4")
colnames(z)=c("s1", "s2", "s3", "s4")