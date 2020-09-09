# THIS CODE GENERATES A RANDOM SAMPLE OF 10 OBSERVATIONS, 4 TIMES, AND STORES THEM IN VECTORS
# CALLED a, b, c, and d, RESPECTIVELY.
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# PRINT THE df TIBBLE TO EVALUATE.
df


# HERE IS A SEQUENCE OF COMMANDS THAT CALCULATE A NORMALIZED RANGE VALUE FOR EACH
# OF THE 10 OBSERVATIONS IN a, b, c, and d.
# IN OTHER WORDS, A NORMALIZED VALUE OF 0.5 SHOWS THE VALUE IS HALFWAY BETWEEN THE MIN AND THE MAX.
# A NORMALIZED VALUE OF 0.8 INDICATES THE VALUE IS 80% OF THE WAY TO THE MAX, ETC.
# NOTICE THE REPETITIVE NATURE OF THE CODE. THIS CODE COMES FROM THE BOOK.
# NOTICE ALSO THAT THE CALCULATION OF df$b CONTAINS A MISTAKE.
# IN THIS CASE THE MISTAKE IS INTENTIONAL BY THE AUTHORS, BUT IT ILLUSTRATES
# AN IMPORTANT POINT ABOUT HOW EASY IT IS TO MAKE MISTAKES WHEN YOU COPY AND PASTE
# THE SAME CODE AND MODIFY IT EACH TIME.

df$a <- (df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))

df$b <- (df$b - min(df$b, na.rm = TRUE)) /
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))

df$c <- (df$c - min(df$c, na.rm = TRUE)) /
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))

df$d <- (df$d - min(df$d, na.rm = TRUE)) /
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))



# TO MAKE THE ABOVE PROCESS EASIER, WE WRITE A FUNCTION FOR
# CALCULATING A NORMALIZED RANGE OF THE INPUT VARIABLE X, WHICH IS A VECTOR.
range.calc <- function(x){
  (x - min(x, na.rm = TRUE)/(max(x, na.rm = TRUE)- min(x, na.rm = TRUE)))
}

# NOW, LET'S REDO THE CODE BY RE-CREATING a,b,c,d AND USING THE range.calc FUNCTION.
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# PRINT THE df TIBBLE TO EVALUATE.
df


# HERE WE CALL THE FUNCTION FOR df$a, b, c, and d, RESPECTIVELY.
range.calc(df$a)
range.calc(df$b)
range.calc(df$c)
range.calc(df$d)

# NOTE THAT THE ABOVE IS STILL SLIGHTLY INEFFICIENT, BUT
# WE CAN STORE THE RESULT IN AN OBJECT IF WE WANT.
range.of.d <- range.calc(df$d)



# HERE IS A DIFFERENT FUNCTION INVOLVING TWO VECTORS.
# IN THIS FUNCTION WE DIVIDE THE TOTAL OF X BY THE TOTAL OF Y.
divide <- function(x,y){
  sum(x)/sum(y)
}

divide(c(1,2,10), c(2,5,20))


# EXERCISE: WRITE A FUNCTION FOR CALCULATING THE MEAN OF X FOR A VECTOR C() AND ILLUSTRATE ITS USE
# FOR THE VECTOR X <- c(1,3,5,7,9)
vector_mean <- function(x,y) {
  sum(x)/sum(y)
}
attr(vector_mean,"comment") <- "vector_mean will return Inf if y == 0"

X <- c(1,3,5,7,9)
vector_mean(X,df$d)
