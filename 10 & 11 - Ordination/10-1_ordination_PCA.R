install.packages(c('vegan', 'vegan3d','devtools'))

# Fetch student allometry data from Google Sheets
  URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ_yUojL2UJzcc68c2Akg1y-vALi9IllQnN_zbMfKJeFOflcV_43EdbWENhzz7JP5HMrmwglgpNQYFu/pub?gid=487538710&single=true&output=csv"
  students.d <- read.csv(URL)

# Create Euclidean distance matrix 
  library(vegan)
  (student.m <- round(vegdist(students.d[5:9], 
                        method="euclidean"),1))
  
# Perform Principal Components Analysis 
  # Fit and assess
    students.pca <- rda(students.d[5:9], scale=TRUE)
    summary(students.pca)
    screeplot(students.pca, type="lines")
  
  # View ordination 
    plot(students.pca)
    
    biplot(students.pca, display="species", las=1,
           xlim=c(-2,2), ylim=c(-1.75,1.5))
    text(students.pca, display = "sites", labels=students.d$name)
  
  # Compare to cluster diagram
    stud.clust <- hclust(vegdist(students.d[,c(5:9)], 
                              method="euclidean"), 
                      method="average")
  
    x11(12,5.5) ; par(mgp=c(4, 1, 0), mar=c(6, 6, 1, 1), 
                      las=1, cex.lab=1.4, cex.axis=1.4, mfrow=c(1,2)) 
    plot(stud.clust, 
          labels=students.d$name, 
          xlab="Student", ylab="Euclidean distance", las=1)
    biplot(students.pca, display="species", las=1,
           xlim=c(-2,2), ylim=c(-1.75,1.5))
    text(students.pca, display = "sites", labels=students.d$name)
    dev.off()
    
    # Combine the two plots
      biplot(students.pca, type="n", las=1,
             xlim=c(-2,2), ylim=c(-1.75,1.5) )
      text(students.pca, display = "sites", labels=students.d$name)
      ordicluster(students.pca, stud.clust, col="blue")
  
  # Plotting by known groups
    plot(students.pca, type="n", las=1,
         xlim=c(-2,2), ylim=c(-1.75,1.5))
          ordispider(students.pca, students.d$gender, display="sites", 
                   label=T, lwd=2, col=c("blue","orange"))
          text(students.pca, display="sites", labels=students.d$name)
          
  # View in multiple dimensions!
  library(vegan3d)
  {
    ordirgl(students.pca, display = "sites", type = "n",
            ax.col = "blue")
    orgltext(students.pca, students.d$name, display="sites", col=students.d$gender)
    orglspider(students.pca, students.d$gender, display = "sites", col=students.d$gender)
  }
  
  # Testing groups
    envfit(students.pca ~ students.d$gender)
    
  # Ordinations are generally stuck in base graphics. 
  # There are some clunky attempts to auto-ggplot vegan objects. 

    devtools::install_github("jfq3/ggordiplots")
    library(ggordiplots)
    
    gg_ordiplot(students.pca, groups = students.d$gender) 
    gg_ordiplot(students.pca, groups = students.d$gender, 
                spiders=TRUE, ellipse=FALSE) 
    
   # If you want much more control, better off building it on our own: 
    ggplot() + theme_bw(14) +
      labs(x="PC 1", y="PC 2") + 
      geom_segment(data=tidy(students.pca$CA$v), 
                   aes(x=0, y=0, xend=PC1*0.95, yend=PC2*0.95), 
                   color="darkred", 
                   arrow = arrow(length = unit(0.02, "npc")))+
      geom_text(data=tidy(students.pca$CA$v), 
                aes(x=PC1, y=PC2, label=.rownames), 
                fontface="italic", color="darkred")  +
      geom_label(data=tidy(students.pca$CA$u), 
                aes(x=PC1, y=PC2, label=students.d$name)) 
   