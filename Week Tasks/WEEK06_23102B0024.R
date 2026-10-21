x=matrix(nrow=4, ncol=4, data=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
rownames(x)=c("s1", "s2", "s3", "s4")
colnames(x)=c("t1", "t2", "t3", "t4")
x

#Row and Column Sum
rowSums(x)
colSums(x)

#Row and Column Mean
rowMeans(x)
colMeans(x)

#Accessing Rows and Columns and Submatrices
x[3,]
x[4:3, 2:4]
x[c(1, 4), c(1, 3)]

#Addition and Subtraction of Matrices
y=matrix(nrow=4, ncol=4, data=17:32)
rownames(y)=c("p1", "p2", "p3", "p4")
colnames(y)=c("q1", "q2", "q3", "q4")
y

x+y
x-y

#Multiplication of Matrices
x%*%y
y%*%x