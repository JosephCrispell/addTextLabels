# addTextLabels (DEPRECATED - NOW AVAILABLE MAINTAINED IN `plotteR`)
## Author: Joseph Crispell
## Repository created: 11-08-18
## Licence: GPL-3
An R package for add non-overlapping labels onto existing R plot

Package can be directly installed into R using:
```
install.packages("devtools")
library("devtools")
install_github("JosephCrispell/addTextLabels")
library(addTextLabels)
```

Once installed test it out with the following code:
```
# Create some random points
n <- 50
testLabels <- c("short", "mediummm", "looooonnnnnnngggggg", "0090292002", "9", "A Different label")
coords <- data.frame(X=runif(n), Y=runif(n, min=0, max=100), Name=sample(testLabels, size=n, replace=TRUE),
                     stringsAsFactors = FALSE)

# Plot them without labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")

# With potentially overlapping labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
text(coords$X, coords$Y, labels=coords$Name, xpd=TRUE)

# Plot them with non-overlapping labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
addTextLabels(coords$X, coords$Y, coords$Name, cex=1, col.label="black")

# Plot them with non-overlapping labels
plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
addTextLabels(coords$X, coords$Y, coords$Name, cex=1, col.background=rgb(0,0,0, 0.75), col.label="white")
```

![](Example.gif)
