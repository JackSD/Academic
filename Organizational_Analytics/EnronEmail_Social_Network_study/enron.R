
library(network) 
library(igraph)

#http://kateto.net/network-visualization
#http://kateto.net/networks-r-igraph
#https://en.wikipedia.org/wiki/Centrality

setwd("C:/Users/Jdoig/Google drive/Northwestern/PA452/Assignment3")
enron = read.csv("JSddata5.csv")

E_matrix = data.matrix(enron, rownames.force = NA)
#write.table(E_matrix, file="Indexes.csv", row.names=FALSE, col.names=FALSE, sep=",")

nodeInfo <- c(E_matrix[,1],E_matrix[,2]) 

E_matrix_weights <- E_matrix[,3]
E_matrix <- E_matrix[,-3]

net <- graph.data.frame(E_matrix, unique(nodeInfo), directed=T)
plot(net)

#looping animation to see how the network changes as we eliminate the low-degree emails
for(k in seq(25,400,by=20))
    {
      E_matrix_limited <- E_matrix[E_matrix[,3]>k,]
      E_matrix_limited <- E_matrix_limited[,-3]
      nodeInfo <- c(E_matrix_limited[,1],E_matrix_limited[,2]) 
      net <- graph.data.frame(E_matrix_limited, unique(nodeInfo), directed=T)
      plot(net)
      Sys.sleep(2)
    }

#degree
d <- degree(net)
which.max(d)
d_sorted <- d[order(d)] 
tail(d_sorted,10)
hist(d, breaks=50, main="Historgram of distinct one-to-one email connections at Enron Corp")
mean(d)

#degree, including the count of emails
TotalCounts  <- read.csv("Mailtotals.csv")
TotalCounts_sorted <- TotalCounts[order(TotalCounts$count),] 
tail(TotalCounts_sorted,10)
head(TotalCounts_sorted,10)
hist(TotalCounts_sorted$count, breaks=50, main="Total one-to-one email connections at Enron Corp by person")
mean(TotalCounts_sorted$count)

#studing a subnetwork: vince.kaminski@enron.com

E_matrix_limited <- E_matrix[(E_matrix[,2]==5018 | E_matrix[,1]==5018) ,]
nodeInfo_l <- c(E_matrix_limited[,1],E_matrix_limited[,2]) 
E_matrix_limited <- E_matrix_limited[,-3]
net_l <- graph.data.frame(E_matrix_limited, unique(nodeInfo_l), directed=T)
plot(net_l)

#density study 1999-2000-2001
data1999 <- read.csv("1999.csv")
E_matrix1999 = data.matrix(data1999, rownames.force = NA)
nodeInfo1999 <- c(E_matrix1999[,1],E_matrix1999[,2]) 
E_matrix1999 <- E_matrix1999[,-3]
net1999 <- graph.data.frame(E_matrix1999, unique(nodeInfo1999), directed=T)
d1999 <- graph.density(net1999, loops=FALSE)

data2000 <- read.csv("2000.csv")
E_matrix2000 = data.matrix(data2000, rownames.force = NA)
nodeInfo2000 <- c(E_matrix2000[,1],E_matrix2000[,2]) 
E_matrix2000 <- E_matrix2000[,-3]
net2000 <- graph.data.frame(E_matrix2000, unique(nodeInfo2000), directed=T)
d2000 <- graph.density(net2000, loops=FALSE)

data2001 <- read.csv("2001.csv")
E_matrix2001 = data.matrix(data2001, rownames.force = NA)
nodeInfo2001 <- c(E_matrix2001[,1],E_matrix2001[,2]) 
E_matrix2001 <- E_matrix2001[,-3]
net2001 <- graph.data.frame(E_matrix2001, unique(nodeInfo2001), directed=T)
d2001 <- graph.density(net2001, loops=FALSE)

densityMatrix <- matrix(c(1999,2000,2001,d1999,d2000,d2001), nrow=3, ncol=2)

plot(densityMatrix, main="density of Enron one-to-one network over time")
lines(densityMatrix)

#betweenness
b <- betweenness(net)
which.max(b)
b_sorted <- b[order(b)] 
tail(b_sorted,10)
hist(b, breaks=50, main="Histogram of betweenness measures at Enron Corp")
mean(b)

#eigenvector centrality
e <- evcent(net)
which.max(e$vector)
e_sorted <- e$vector[order(e$vector)] 
tail(e_sorted,10)
hist(e_sorted, breaks=50, main="Histogram of eigenvector c measures at Enron Corp")
mean(e_sorted)

#closeness centrality
c <- closeness(net)
which.max(c)
c_sorted <- c[order(c)] 
tail(c_sorted,10)
hist(c, breaks=50, main="Histogram of closeness measures at Enron Corp")
mean(c)


