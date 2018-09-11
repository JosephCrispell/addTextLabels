# Tutorials
#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
#http://r-pkgs.had.co.nz/description.html
#https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html

## Packages to install
#install.packages("devtools")
#install.packages("digest")
#devtools::install_github("klutometis/roxygen")

## Packages to load
#library("devtools")
#library("roxygen2")

## Creating package
#packageDirectory <- "/home/josephcrispell/Desktop/Research/addTextLabels/"
#create(packageDirectory)
#setwd(packageDirectory)

## Documenting changes
#setwd(packageDirectory)
#document()

## Install
#setwd("..")
#install("addTextLabels")

#' Add non-overlapping text labels to plot
#'
#' This function is similar to the \code{text()} function but it will attempt to re-locate labels that will overlap
#' @param xCoords A vector containing the X coordinates for labels
#' @param yCoords A vector containing the Y coordinates for labels
#' @param labels A vector containing the labels to be plotted
#' @param cex A number to scale the size of the plotted labels. Defaults to 1
#' @param col.label The colour of the plotted labels. Defaults to "red"
#' @param col.line The colour of the line to plot from relocated labels to original location. Defaults to "black"
#' @param col.background An optional colour for a background polygon plotted behind labels. Defaults to NULL - won't be plotted
#' @param lty A number detailing the type of line to plot from relocated labels to original location. 0: blank, 1: solid, 2: dashed, 3: dotted, 4: dotdash, 5: longdash, and 6: twodash. Defaults to 1
#' @param lwd A number to scale the size of line from relocated labels to original location. Defaults to 1
#' @param border The colour of the border to be plotted around the polygon. Defaults to NA - won't be plotted
#' @keywords text label plot
#' @export
#' @examples 
#' # Create some random points
#' n <- 50
#' coords <- data.frame(X=runif(n), Y=runif(n), Name="Test Label")
#' 
#' # Plot them without labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#' 
#' # With potentially overlapping labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#' text(coords$X, coords$Y, labels=coords$Name, xpd=TRUE)
#' 
#' # Plot them with non-overlapping labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#' addTextLabels(coords$X, coords$Y, coords$Name, cex=1, col.label="black")
#' 
#' # Plot them with non-overlapping labels
#' plot(x=coords$X, y=coords$Y, pch=19, bty="n", xaxt="n", yaxt="n", col="red", xlab="X", ylab="Y")
#' addTextLabels(coords$X, coords$Y, coords$Name, cex=1, col.background=rgb(0,0,0, 0.75), col.label="white")
addTextLabels <- function(xCoords, yCoords, labels, cex=1, col.label="red", col.line="black", col.background=NULL,
                          lty=1, lwd=1, border=NA){
  
  ###########################################
  # Produce a list of alternative locations #
  ###########################################
  
  # Calculate the label heights and widths
  labelDimensions <- calculateLabelHeightsAndWidths(labels=labels, cex=cex, polygonAdded=is.null(col.background) == FALSE)
  textHeights <- labelDimensions[["Heights"]]
  textWidths <- labelDimensions[["Widths"]]
  
  # Generate the alternative locations
  output <- generateAlternativeLocations(xCoords=xCoords, yCoords=yCoords, textHeights=textHeights,
                                         textWidths=textWidths, cex=cex)
  altX <- output[["AltX"]]
  altY <- output[["AltY"]]
  
  ##############################################################
  # Add labels to plot assigning new locations where necessary #
  ##############################################################
  
  # Plot the point label
  for(i in 1:length(xCoords)){
    
    # Is the current point too close to others?
    if(tooClose(xCoords, yCoords, i, textHeights[i], textWidths[i]) == TRUE && length(altX) != 0){
      
      # Get a new location
      newLocationIndex <- chooseNewLocation(xCoords[i], yCoords[i], altX, altY)
      
      # Add line back to previous location
      addLineBackToOriginalLocation(altX=altX[newLocationIndex], altY=altY[newLocationIndex],
                                    x=xCoords[i], y=yCoords[i], label=labels[i], cex=cex, col=col.line, lty=lty, lwd=lwd)
      
      # Add label
      addLabel(x=altX[newLocationIndex], y=altY[newLocationIndex], label=labels[i], cex=cex, col=col.label, bg=col.background, 
               border=border)
      
      # Remove new location and any locations too close to it
      output <- removeLocationAndThoseCloseToItFromAlternatives(
        altX, altY, newLocationIndex, textHeights[i], textWidths[i])
      altX <- output[["X"]]
      altY <- output[["Y"]]
      
      # Change the X and Y coordinates to the alternate ones
      xCoords[i] <- altX[newLocationIndex]
      yCoords[i] <- altY[newLocationIndex]
      
    }else{
      
      # Add label
      addLabel(x=xCoords[i], y=yCoords[i], label=labels[i], cex=cex, col=col.label, bg=col.background, border=border)
    }
  }
}

#' Plot line from new alternative location back to original
#'
#' Function used by \code{addTextLabels()}
#' @param altX The X coordinate of new location
#' @param altY The Y coordinate of new location
#' @param x The X coordinate of original location
#' @param y The Y coordinate of original location
#' @param label The label to be plotted. Required to work out when line ends
#' @param cex The number used to scale the size of the label. Required to work out when line ends
#' @param col Colour of line to be plotted
#' @param lty A number detailing the type of line to be plotted. 0: blank, 1: solid, 2: dashed, 3: dotted, 4: dotdash, 5: longdash, and 6: twodash. Defaults to 1
#' @param lwd A number to scale the size of plotted line. Defaults to 1
#' @keywords internal
addLineBackToOriginalLocation <- function(altX, altY, x, y, label, cex, col, lty=1, lwd=1){
  
  # Calculate the label width and height
  labelHeight <- strheight(label, cex=cex)
  labelWidth <- strwidth(label, cex=cex)
  
  # Create a set of points marking the boundaries of the label
  xMarkers <- c(seq(from=altX - (labelWidth * 0.52), to=altX + (labelWidth * 0.52), by=0.05*labelWidth), 
                altX + (labelWidth * 0.52))
  yMarkers <- c(seq(from=altY - (labelHeight * 0.6), to=altY + (labelHeight * 0.6), by=0.05*labelHeight),
                altY + (labelHeight * 0.6))
  
  # Calculate the closest pair of X and Y coordinates to the origin
  closestX <- xMarkers[which.min(abs(xMarkers - x))]
  closestY <- yMarkers[which.min(abs(yMarkers - y))]
  
  # Plot the line
  points(x=c(closestX, x), y=c(closestY, y), type="l", col=col, lty=lty, lwd=lwd)
}

#' Calculate the heights and widths of the labels in the current plotting window
#'
#' Function used by \code{addTextLabels()}
#' @param labels A vector of the labels
#' @param cex The number used to scale the size of the label and therefore its height and width
#' @param polygonAdded A logical (TRUE/FALSE) variable to indicate whether polygons are going to be plotted. If they are widths and heights are made slightly larger
#' @keywords internal
#' @return Returns a list containing the heights and widths of the labels provided
calculateLabelHeightsAndWidths <- function(labels, cex, polygonAdded){
  
  # Get the text label heights and lengths
  textHeights <- strheight(labels) * cex
  textWidths <- strwidth(labels) * cex
  
  # If plotting boxes for labels make widths and heights slightly larger
  if(polygonAdded == TRUE){
    textHeights <- textHeights + (0.25 * textHeights)
    textWidths <- textWidths + (0.05 * textWidths)
  }
  
  # Create the output
  output <- list("Heights"=textHeights, "Widths"=textWidths)
  
  return(output)
}

#' Generate a set of alternative locations where labels can be plotted if they overlap with another label
#'
#' Function used by \code{addTextLabels()}
#' @param xCoords A vector containing the X coordinates for labels
#' @param yCoords A vector containing the Y coordinates for labels
#' @param textHeights The heights of the labels in the current plotting window. Calculated by \code{calculateLabelHeightsAndWidths}
#' @param textWidths The widths of the labels in the current plotting window. Calculated by \code{calculateLabelHeightsAndWidths}
#' @param cex The number used to scale the size of the label
#' @keywords internal
#' @return Returns a list containing the coordinates of the alternative locations
generateAlternativeLocations <- function(xCoords, yCoords, textHeights, textWidths, cex){
  
  # Get the axis limits
  axisLimits <- par("usr")
  
  # Define the spacer for each axis
  spacerX <- 0.01 * (axisLimits[2] - axisLimits[1])
  spacerY <- 0.01 * (axisLimits[4] - axisLimits[3])
  
  # Generate the set of points based upon the spacer
  altX <- c()
  altY <- c()
  for(i in seq(axisLimits[1], axisLimits[2], spacerX)){
    for(j in seq(axisLimits[3], axisLimits[4], spacerY)){
      
      altX[length(altX) + 1] <- i
      altY[length(altY) + 1] <- j
    }
  }
  #points(altX, altY, col=rgb(0,0,0, 0.5), pch=20, xpd=TRUE)
  
  # Remove points that are too close to actual values
  remove <- c()
  for(i in 1:length(altX)){
    
    for(j in 1:length(xCoords)){
      
      if(abs(altX[i] - xCoords[j]) < textWidths[j] &&
         abs(altY[i] - yCoords[j]) < textHeights[j]){
        remove[length(remove) + 1] <- i
        break
      }
    }
  }
  #points(altX[remove], altY[remove], col=rgb(1,1,1), pch=20, xpd=TRUE)
  if(length(remove) > 0){
    altX <- altX[-remove]
    altY <- altY[-remove]
  }
  
  # Create an output
  output <- list("AltX"=altX, "AltY"=altY)
  
  return(output)
}

#' Plot a label with optional polygon background
#'
#' Function used by \code{addTextLabels()}
#' @param x The X coordinate at which label is to be plotted
#' @param y The Y coordinate at which label is to be plotted
#' @param label The label to be plotted
#' @param cex The number used to scale the size of the label
#' @param col The colour of the label to be plotted
#' @param bg The colour of the polygon to be plotted. If NULL no polygon plotted
#' @param border The colour of the polygon border. If NA, no border plotted
#' @keywords internal
addLabel <- function(x, y, label, cex, col, bg, border){
  
  # Calculate the height and width of the label
  labelHeight <- strheight(label, cex=cex)
  labelWidth <- strwidth(label, cex=cex)
  
  # Add a background polygon - if requested
  if(is.null(bg) == FALSE){
    polygon(x=c(x - (labelWidth * 0.52),
                x - (labelWidth * 0.52),
                x + (labelWidth * 0.52),
                x + (labelWidth * 0.52)),
            y=c(y - (labelHeight * 0.6),
                y + (labelHeight * 0.6),
                y + (labelHeight * 0.6),
                y - (labelHeight * 0.6)), 
            col=bg,
            border=border, xpd=TRUE)
  }
  
  
  # Add label
  text(x=x, y=y, labels=label, xpd=TRUE, cex=cex, col=col)
}

#' Remove coordinates of alternative locations that are too close to coordinates
#'
#' Function used by \code{addTextLabels()}
#' @param altX A vector of X coordinates for alternative locations
#' @param altY A vector of Y coordinates for alternative locations
#' @param index The index of the point of interest in the coordinate vectors
#' @param textHeight The height of the label to be plotted at the point of interest
#' @param textWidth The width of the label to be plotted at the point of interest
#' @keywords internal
#' @return Returns a list of the coordinates of the alternative locations that weren't too close
removeLocationAndThoseCloseToItFromAlternatives <- function(altX, altY, index, textHeight, textWidth){
  remove <- c(index)
  for(i in 1:length(altX)){
    
    if(i == index){
      next
    }
    
    if(abs(altX[index] - altX[i]) < textWidth &&
       abs(altY[index] - altY[i]) < textHeight){
      remove[length(remove) + 1] <- i
    }
  }
  
  altX <- altX[-remove]
  altY <- altY[-remove]
  
  return(list("X" = altX, "Y" = altY))
}

#' A function to choose (from the alternative locations) a new location for a label to be plotted at
#'
#' Function used by \code{addTextLabels()}
#' @param x The X coordinate at which label was to be plotted
#' @param y The Y coordinate at which label was to be plotted
#' @param altXs A vector of X coordinates for alternative locations
#' @param altYs A vector of Y coordinates for alternative locations
#' @keywords internal
#' @return Returns the index of the chosen alternative location
chooseNewLocation <- function(x, y, altXs, altYs){
  
  # Calculate the distance from point to all alternatives
  distances <- c()
  for(i in 1:length(altXs)){
    distances[i] <- euclideanDistance(x, y, altXs[i], altYs[i])
  }
  
  return(which.min(distances))
}

#' Checks whether a point is too close to any others
#'
#' Function used by \code{addTextLabels()}
#' @param xCoords A vector containing the X coordinates for labels
#' @param yCoords A vector containing the Y coordinates for labels
#' @param index The index of the point of interest
#' @param textHeight The height of the label to be plotted at the point of interest
#' @param textWidth The width of the label to be plotted at the point of interest
#' @keywords internal
#' @return Returns a logical variable to indicate whether the point of interest was too close to any points
tooClose <- function(xCoords, yCoords, index, textHeight, textWidth){
  
  result <- FALSE
  for(i in 1:length(xCoords)){
    
    if(i == index){
      next
    }else if(abs(xCoords[index] - xCoords[i]) < textWidth &&
             abs(yCoords[index] - yCoords[i]) < textHeight){
      result <- TRUE
      break
    }
  }
  
  return(result) 
}

#' Calculate the euclidean distance between two points
#'
#' Function used by \code{addTextLabels()}
#' @param x1 The X coordinate of the first point
#' @param y1 The Y coordinate of the first point
#' @param x2 The X coordinate of the second point
#' @param y2 The Y coordinate of the second point
#' @keywords internal
#' @return Returns the distance between the points provided
euclideanDistance <- function(x1, y1, x2, y2){
  return(sqrt(sum((x1 - x2)^2 + (y1 - y2)^2)))
}