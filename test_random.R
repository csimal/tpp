
t <- read.table("~/Documents/fortran/TP/1/rtable.txt", header=FALSE ,sep="\t",dec=".")

mu = mean(as.matrix(t))
sigma = var(t)

v = (t(range(1,99))-mu)*(t(range(2,100))-mu)
A = sum(v)/sigma
