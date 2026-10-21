> ## WEEK 09 ##
> print("The zero occurs at"); print(2*pi); print("radians");
[1] "The zero occurs at"
[1] 6.283185
[1] "radians"
> cat("The zero occurs at", 2*pi, "radians.", "\n")
The zero occurs at 6.283185 radians. 
> 
> d = date
> cat("Today's date is :", d, "\n")
Today's date is : Error in cat("Today's date is :", d, "\n") : 
  argument 2 (type 'closure') cannot be handled by 'cat'
> d=date()
> cat("Today's date is :", d, "\n")
Today's date is : Wed Sep 23 11:54:50 2026 
> 
> X=1:10
> cat(x, sep="... ")
Error: object 'x' not found
> cat(X, sep="... ")
1... 2... 3... 4... 5... 6... 7... 8... 9... 10> 
> 
> 
> x=4
> cat("The square root of", x, "is approximately", format(sqrt(x), digits=3), "\n")
The square root of 4 is approximately 2 
> x=7
> cat("The square root of", x, "is approximately", format(sqrt(x), digits=3), "\n")
The square root of 7 is approximately 2.65 
> 
> x=1:10
> x
 [1]  1  2  3  4  5  6  7  8  9 10
> cat(x, fill = 2, labels=paste("(", letters[1:10]"):"))
Error: unexpected string constant in "cat(x, fill = 2, labels=paste("(", letters[1:10]"):""
> cat(x, fill = 2, labels=paste("(", letters[1:10], "):"))
( a ): 1 
( b ): 2 
( c ): 3 
( d ): 4 
( e ): 5 
( f ): 6 
( g ): 7 
( h ): 8 
( i ): 9 
( j ): 10
> 
> paste(..., sep=" ", collapse=NULL)
Error: '...' used in an incorrect context
> paste (..., sep=" ", collapse=NULL)
Error: '...' used in an incorrect context
> paste ("Today", "is", sep=" ", collapse=NULL)
[1] "Today is"
> paste(1:12)
 [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12"
> mode(paste(1:12))
[1] "character"
> 
> 
> x = paste("Ex", 1:5, sep="_")
> x
[1] "Ex_1" "Ex_2" "Ex_3" "Ex_4" "Ex_5"
> x=paste("Ex", 1:5, sep="_", collapse="")
> x
[1] "Ex_1Ex_2Ex_3Ex_4Ex_5"
> 
> paste0(1:10)
 [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
> 
> 
> 
> #Operations on Strings
> strsplit(x, split="_")
[[1]]
[1] "Ex"  "1Ex" "2Ex" "3Ex" "4Ex" "5"  

> d=date()
> mode(d)
[1] "character"
> strsplit(dates, "-")
Error: object 'dates' not found
> strsplit(date(), "-")
[[1]]
[1] "Wed Sep 23 12:23:01 2026"

> strsplit("Shruti", split="")
[[1]]
[1] "S" "h" "r" "u" "t" "i"

> 
> 
> #String Manipulation
> x="My name is Shruti"
> nchar(x)
[1] 17
> nzchar(x)
[1] TRUE
> y=1
> nzchar(y)
[1] TRUE
> z=TRUE
> nzchar(z)
[1] TRUE
> nzchar("")
[1] FALSE
> tolower(x)
[1] "my name is shruti"
> toupper(x)
[1] "MY NAME IS SHRUTI"
> 
