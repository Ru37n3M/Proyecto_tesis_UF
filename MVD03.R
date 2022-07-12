library(MASS)
mvdlist <- list("1" = data.frame(mvrnorm(n = 200, c(2, 2, 2),diag(1,3,3))),
                "2" = data.frame(mvrnorm(n = 200, c(-2, -2, -2),diag(1,3,3))),
                "3" = data.frame(mvrnorm(n = 600, c(0, 0, 0),diag(1,3,3))))


mvdf <- do.call("rbind", mvdlist)

mvdf

library(MVN)
mvn(mvdf, mvnTest = "energy", univariatePlot = "scatter")




