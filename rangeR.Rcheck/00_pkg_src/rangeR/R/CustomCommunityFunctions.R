

RankAbundance <- function (x, y = "", factor = "", 
                           level, digits = 1, t = qt(0.975, 
                                             df = n - 1)) 
{ # This is the rankabundance function from package BiodiversityR
  # Kindt, R. & Coe, R. (2005) Tree diversity analysis. A manual and software for common statistical
  # methods for ecological and biodiversity studies. World Agroforestry Centre (ICRAF), Nairobi. ISBN
  # 92-9059-179-X.
  
  if (class(y) == "data.frame" && factor != "") {
    subs <- y[, factor] == level
    for (q in 1:length(subs)) {
      if (is.na(subs[q])) {
        subs[q] <- F
      }
    }
    x <- x[subs, , drop = F]
    freq <- apply(x, 2, sum)
    subs <- freq > 0
    x <- x[, subs, drop = F]
  }
  if (dim(as.matrix(x))[1] == 0) {
    result <- array(NA, dim = c(1, 8))
    colnames(result) <- c("rank", "abundance", "proportion", 
                          "plower", "pupper", "accumfreq", "logabun", "rankfreq")
    rownames(result) <- "none"
    return(result)
  }
  total <- apply(x, 1, sum)
  p <- ncol(x)
  n <- nrow(x)
  mu <- sum(total)/n
  result <- array(dim = c(p, 8))
  colnames(result) <- c("rank", "abundance", "proportion", 
                        "plower", "pupper", "accumfreq", "logabun", "rankfreq")
  rownames(result) <- colnames(x)
  for (j in 1:p) {
    spec <- x[, j]
    pi <- spec/total
    p <- sum(spec)/sum(total)
    sigma2 <- 0
    for (i in 1:n) {
      sigma2 <- sigma2 + (total[i]^2 * (pi[i] - p)^2)
    }
    sigma2 <- sigma2/(n * (n - 1) * mu * mu)
    sigma <- sigma2^0.5
    result[j, 2] <- sum(spec)
    result[j, 3] <- p * 100
    result[j, 4] <- (p - t * sigma) * 100
    result[j, 5] <- (p + t * sigma) * 100
  }
  p <- ncol(x)
  result2 <- result
  seq <- rev(order(result[, 2], -order(rownames(result))))
  result[1:p, ] <- result2[seq, ]
  rownames(result)[1:p] <- rownames(result2)[seq]
  result[, 1] <- c(1:ncol(x))
  result[, 6] <- cumsum(result[, 3])
  result[, 7] <- log(result[, 2], base = 10)
  result[, 8] <- result[, 1]/ncol(x) * 100
  result <- round(result, digits = digits)
  return(result)
}

var.view <- function(ord, axes){round(summary(eigenvals(ord))[1:3,1:axes],2)}

bouncer <- function(data, measure, level, PrintResults) {
  if (measure!="abundance" && measure!="proportion") 
    stop("'measure' must be one of 'abundance' or 'proportion'")
  ab <- as.data.frame(RankAbundance(data))[c(2,6)]
  ab$propfreq <- 100 - ab$accumfreq
  ifelse(measure=="abundance",
         cuts <- subset(ab, abundance < level), 
         cuts <- subset(ab, propfreq < level))
  prop <- dim(cuts)[1] / dim(data)[1]
  if(missing(PrintResults)) {
    return(rownames(cuts))
  } 
  if(PrintResults %in% c("T", "t", "true", "Y", "y", "yes", "YES", "TRUE")) 
         { 
          mess = paste("Yo. ", dim(cuts)[1]," species (",prop*100,"% of total) don't make the cut.", sep="")
          message(mess) 
           return(rownames(cuts)) 
          } }




theme_ord <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="bold"),
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.margin = unit(c(2,10,2,2), "mm"), 
          strip.background = element_rect(fill = "lightgreen", colour = "grey50", size = 0.2))
}