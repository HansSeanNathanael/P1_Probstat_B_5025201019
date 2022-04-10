# 1
# a

p <- 0.2
chance <- dgeom(3, p)
paste("P(X = 3) = ", chance)

#b
p <- 0.2
n <- 10000

set.seed(0)
data <- rgeom(n, p)

paste("P(X = 3) = ", mean(data == 3))

#c

# hasil dari poin b lebih kecil tetapi mendekati hasil poin a.

#d
px <- data [ data %in% c(3)]
hist(px, breaks = max(data), ylim = c(0, 5000), main = "Frekuensi X = 3")

#e

mean <- 1/p
variance <- q/(p^2)

paste("mean: ", mean)
paste("variance: ", variance)

#2

p <- 0.2
q <- 1 - p
n <- 20

#a
chance <- dbinom(x = 4, size = n, prob = p)
paste("P(X = 4) = ", chance)


#b
x <- 0:n
y <- c()
for (i in x)
{
  y <- append(y, combinations(n, i) * (p ^ i) * (q ^ (n - i)) )
}

barplot(y, names.arg=x, ylim=c(0, 1), 
        xlab = "Jumlah pasien sembuh", ylab = "Probabilitas",
        main = "Histogram Binomial p = 0.2, n = 20")

#c
mean <- n * p
variance <- n * p * q

paste("mean: ", mean)
paste("variance: ", variance)

#3
l <- 4.5
e <- exp(1)

#a
x <- 6
chance <- dpois(x = x, lambda = l)
paste("P(X = 6) = ", chance)

#b
n <- 365
set.seed(0)
data <- rpois(n = n, lambda = l)
px <- data [ data %in% c(6)]
hist(px, main = "X = 6")

#c

#Hasil poin b mendekati hasil poin a

#d
mean <- 0
for (i in 0:100)
{
  mean <- mean + i * dpois(x = i, lambda = l)
}
mean

expx2 <- 0
for (i in 0:100)
{
  expx2 <- expx2 + i^2 * dpois(x = i, lambda = l)
}
variance <- expx2 - mean^2
variance

#4
v <- 10
x <- 2
#a

# pdf: 

#b

set.seed(0)
n <- 100
data <- rgamma(n = n, shape = v/2, scale = 2)
hist(data, breaks = max(data), xlim = c(0, max(data)))

#c
mean <- v
variance <- 2 * v

paste("mean: ", mean)
paste("variance: ", variance)

#5

l <- 3

#a

# pdf:

#b
set.seed(1)
n <- 10
data <- rgamma(n = n, shape = 1, rate = l)
hist(data)

set.seed(1)
n <- 100
data <- rgamma(n = n, shape = 1, rate = l)
hist(data)

set.seed(1)
n <- 1000
data <- rgamma(n = n, shape = 1, rate = l)
hist(data)

set.seed(1)
n <- 10000
data <- rgamma(n = n, shape = 1, rate = l)
hist(data)

#c
set.seed(1)
n <- 100
data <- rgamma(n = n, shape = 1, rate = l)
paste("mean: ", mean(data))

#6

n <- 100
mean <- 50
sd <- 8
set.seed(0)
data <- rnorm(n = n, mean = mean, sd = sd)
paste("z1: ", (min(data) - mean) / sd)
paste("z2: ", (max(data) - mean) / sd)

x <- seq(min(data), max(data), length = 50)
y <- seq(0, 0, length = 50)
for (i in data)
{
  for (j in 1:50)
  {
    if (j == 1)
    {
      if (i == x[j])
      {
        y[j] <- y[j] + 1
        break 
      }
    }
    else if (i <= x[j] & i > x[j - 1])
    {
      y[j] <- y[j] + 1
      if (j == 100)
      {
        print(i)
      }
      break
    }
  }
}

plot(x, y, type="l", lty=1, xlab="x value",
     ylab="Density", main="Distribusi Normal")

#b
hist(data, main = "5025201019_Hans Sean Nathanael_B_DNhistogram", breaks = 50)

#c
mean <- mean(data)
t <- 0
for (i in data)
{
  t <- t + ((i - mean) ^ 2)
}
variance = sqrt(t / n)
paste("variance: ", variance)

