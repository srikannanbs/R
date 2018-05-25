#Comments

#By default R does not support  Multiline comment#


"By default R does not support
Multiline comment"

myscript <- "Hello World"
print(myscript)

#my first program
"multi 
line
comment"

#Data Types for vector object

#Logical
v <- TRUE
print(class(v))





#Numeric
v <- 23.5
print(class(v))



#Integer
v <- 2L
print(class(v))

#Complex
v <- 2+5i
print(class(v))

#Charecter
v <- "TRUE"
print(class(v))

#Raw
v <- charToRaw("Hello")
print(class(v))
print(charToRaw("Hello"))


#More than one element in a vector
# Create a vector.
apple<- c('red','green',"yellow")

print(apple)


#rm()
#ls()


rm(apple)


# Get the class of the vector.
print(class(apple))

nv<- c(2, 3, 5) 
nv


lv<- c(TRUE, FALSE, TRUE, FALSE, FALSE) 
lv


sv<- c("aa", "bb", "cc", "dd", "ee") 
sv

#Functions
length(c("aa", "bb", "cc", "dd", "ee"))

v=c(2,5,6)
h=c(1,2,3)

v+h

v-h

v*h

v/h

# Combining Vectors

n = c(2, 3, 5)
class(n)
s = c("aa", "bb", "cc", "dd", "ee")
class(s)
ns<-c(n, s) 
class(ns)

# Recycling Rule
x=c(2,4)
y=c(5,6,8,9,7)
x+y

# Vector Index

s = c("aa", "bb", "cc", "dd", "ee") 





s[3] 



s[-3] 

s[10] 

# Numeric Index Vector


s = c("aa", "bb", "cc", "dd", "ee") 
s[c(2, 3)] 

s[c(2, 3, 3)] 

s[c(2, 1, 3)] 

s[2:4] 

help(":")

# Logical Index Vector

s = c("aa", "bb", "cc", "dd", "ee")

L = c(FALSE, TRUE, FALSE, TRUE, FALSE) 

s[L] 
s[c(FALSE, TRUE, FALSE, TRUE, FALSE)] 
#Lists
# Create a list.
list1 <- list(c(2,5,3),21.3,sin)
# Print the list.
print(list1)




# Matrix

A = matrix( 
  c(2, 4, 3, 1, 5, 7), # the data elements 
  nrow=2,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE)        # fill matrix by rows 

A                      # print the matrix 


A[2, 3]      # element at 2nd row, 3rd column 

A[2, ]       # the 2nd row 


A[ ,3]       # the 3rd column 


A[ ,c(1,3)]  # the 1st and 3rd columns 





B = matrix( 
  c(2, 4, 3, 1, 5, 7), 
  nrow=3, 
  ncol=2) 

B             # B has 3 rows and 2 columns 

# Transpose


t(B)          # transpose of B 

#Combining Matrices


C = matrix( 
  c(7, 4, 2), 
  nrow=3, 
  ncol=1) 

C             # C has 3 rows 

cbind(B, C) 


D = matrix( 
  c(6, 2), 
  nrow=1, 
  ncol=2) 

D             # D has 2 columns 

rbind(B, D) 

c(B) 

# List

n = c(2, 3, 5) 
s = c("aa", "bb", "cc", "dd", "ee") 
b = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
x = list(n, s, b, 3)   # x contains copies of n, s, b
# List Slicing

x[2] 

x[c(2, 4)] 

# Member Reference
# In order to reference a list member directly, we have to use the double square bracket "[[]]" operator. The following object x[[2]] is the second member of x. In other words, x[[2]] is a copy of s, but is not a slice containing s or its copy.

x[[2]] 

x[[2]][1] = "ta" 
x[[2]] 

s 

# Named List Members

v = list(bob=c(2, 3, 5), john=c("aa", "bb")) 
v 

v["bob"] 

v[c("john", "bob")] 

# Member Reference

v[["bob"]] 

v$bob 


attach(v) 
bob 

detach(v)
# Create an array.
a <- array(c('green','yellow'),dim=c(3,3,2))
print(a)

#Factors

# Create a vector.
apple_colors <- c('green','green','yellow','red','red','red','green')
# Create a factor object.
factor_apple <- factor(apple_colors)
# Print the factor.
print(factor_apple)
print(nlevels(factor_apple))