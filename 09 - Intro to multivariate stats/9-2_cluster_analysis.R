install.packages(c("pacman"))
pacman::p_load(tidyverse, GGally, vegan)

URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ_yUojL2UJzcc68c2Akg1y-vALi9IllQnN_zbMfKJeFOflcV_43EdbWENhzz7JP5HMrmwglgpNQYFu/pub?gid=487538710&single=true&output=csv"
students.d <- read.csv(URL)

# Check out univariate comparisons
  ggpairs(students.d[5:9])
 
# Create Euclidean distance matrix 
  (student.m <- round(vegdist(students.d[5:9], 
                        method="euclidean"),1))

# Cluster analysis
  
# Calculate cluster diagram
  student.clust <- hclust(student.m, method="average") 	
  plot(student.clust, labels=students.d$name, 
       main="Cluster diagram of student traits", 
       xlab="Student", ylab="Euclidean distance", las=1)

# Visualize potential groups
  rect.hclust(student.clust, 2, border="red") 
  rect.hclust(student.clust, 3, border="blue")
  
  plot(student.clust, labels=students.d$name, 
       main="Cluster diagram of student traits", 
       xlab="Student", ylab="Euclidean distance", las=1)
  rect.hclust(student.clust, 5, border="darkgreen")

# How many groups are 'best'?

  # Define range of clusters to test
    try.clusters <- 1:5
    
  # Create empty data.frames 
    # This one stores the cluster assignments for each student
    k.groups <- data.frame(matrix(ncol=length(try.clusters), 
                                  nrow=length(students.d$name)))
      colnames(k.groups) <- paste0("clstrs", 
                                   c(try.clusters))
    
    # This one stores the error reduction per # clusters
    k.ss <-data.frame(matrix(nrow=length(try.clusters), ncol=2))
      colnames(k.ss) <- c("clusters",
                          "diff.SumSquares")

  # K-means clustering across range of # clusters
    for(i in 1:length(try.clusters)) {
      cl <- kmeans(student.m, i)
      k.groups[i] <- cl$cluster
      k.ss[i,] <- cbind(i, with(cl, totss-betweenss))}

  # plot change in Sum of Squares against # of clusters
    plot(diff.SumSquares ~ clusters, k.ss, type="b")
    
# What accounts for clusters in the dataset? 
    
    # Attach cluster assignments to student data
    (student.clusts <- data.frame(students.d[1:4], k.groups))

    # Test for differences between groups
    (tabMF <- with(student.clusts, table(clstrs2, gender)) )
    plot(tabMF, main=" ")
    summary(tabMF)
