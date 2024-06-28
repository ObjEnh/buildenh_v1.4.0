##name of script: support_plot_results_on_references.R
cat("version_number= ",v_nr,"\n")
##purpose: plotting of results
##instruction: run all scripts in "demo"-mode before using supporting scripts
#author: Joachim Höhle
##GNU General Public License (GPL)

## contents:

# 1.check of classification and enhancement in enlarged orthoimage
# 2.check result in enlarged ground truth (GT) image
# 3.plot building onto graph
# 4.plot outlines with vertices & line-numbers onto enlarged orthoimage
# 5.plot of all processed buildings onto graph, orthoimage and GT
# 6.plot of a selected corner onto orthoimage (small scale) 
# 7.measurement of unmarked check point in zoomed orthoimage
# 8.measurement of check points in zoomed reference map

################################################################################


## 1.check of classification and enhancement in enlarged orthoimage

#input and display
setwd(OrgImgPathname)
img_ref <- readImage(OrgImgFilename)
img_x_max <- dim(img_ref)[1] #image size x
img_y_max <- dim(img_ref)[2] #image size y
display(img_ref,method = "raster")

#input of plot parameters: center (xc, yc), maximum radius (r_max)
setwd(home_dir)
f1 <- paste("./data/",Img_name,"/param_b_",bnr2,sep="")
load(f1)
xc <- plotPar[1]
yc <- plotPar[2]
r_max <- plotPar[3]

#input of adjusted corner point- (vertex-) coordinates
setwd(home_dir)
fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
b_xy_ortho <- read.table(fname12, header=T)
names(b_xy_ortho) <- c("x","y")
b_xy_ortho$y <- (-b_xy_ortho$y)
k1 <- length(b_xy_ortho$x)
#

#display of orthoimage in new (large) window
orig_x <- as.integer(xc-1.2*r_max)
orig_y <- as.integer(yc-1.2*r_max)
wind_x <- as.integer(orig_x+2.4*r_max)
wind_y <- as.integer(orig_y+2.4*r_max)
#

if (orig_x < 0) {
  orig_y = 0 
}

if (orig_y < 0) {
  orig_y = 0 
}

if (wind_x > img_x_max) { #new window border
  wind_x <- img_x_max
}

if (wind_y > img_y_max) { #new window border
  wind_y <- img_y_max
}

img_uds <- img_ref[orig_x:wind_x,orig_y:wind_y,1:3]
display(img_uds, method = "raster")
#display(img_uds, method = "browser")

#input of object (CC)
fname <- paste("./data/",Img_name,"/idxy_LCM_b",bnr2,".csv",sep="")
pc3 <- read.table(fname, header=TRUE)
names(pc3)[1:2] <- c("col","row")

#plotting of center, object (CC), and connections onto enlarged orthoimage
points(xc-orig_x,yc-orig_y,pch=3,asp=1,cex=1.3,col="red")
points(as.integer(pc3$col-orig_x),as.integer(pc3$row-orig_y),pch=20,asp=1,cex=0.3,col="green")
#

#loop
i=0

while(i < k1) {
  i <- i + 1
  lines(b_xy_ortho$x-orig_x, b_xy_ortho$y-orig_y, col="white", asp=1, type="l", lwd=3, lty=1)
}

#end of script #1 (check of classification and enhancement by enlarged orthoimage)

###############################################################################


## 2.check result in enlarged ground truth (GT)

setwd(OrgGtsPathname)
img_ref2 <- readImage(OrgGtsFilename)
img_uds2 <- img_ref2[orig_x:wind_x,orig_y:wind_y,1:3]
display(img_uds2, method = "raster")

#adjusted corner point coordinates
setwd(home_dir)
fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
b_xy_ortho <- read.table(fname12, header=T)
names(b_xy_ortho) <- c("x","y")
b_xy_ortho$y <- (-b_xy_ortho$y)
k2 <- length(b_xy_ortho$x)

#plot of connections on GT_large scale
i <- 0

while(i < k2) {
  i <- i + 1
  lines(b_xy_ortho$x-orig_x, b_xy_ortho$y-orig_y, col="red", 
        asp=1, type="l", lwd=3, lty=1)
}

#end of script #2 (check result in enlarged ground truth (GT) image)
################################################################################


## 3.plot of building onto graph

setwd(home_dir)
x=0
y=0
bnr2 #number of building
plot(x,-y, pch=3, cex=1, cex.axis=1,cex.lab=1,col="red", asp=1, 
     xlim=c(1,img_x_max), ylim=c(-img_y_max,-1), main = paste("b",bnr2," of orthoimage ",
     Img_name,sep = ""))

fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
b <- read.table(fname12,header=T)
k3 <- nrow(b)
names(b) <- c("Points_x","Points_y")
cat("plot of building-outline","\n")
#

#loop
i <- 0

while(i < k3) {
  i <- i+1
  lines(b, col="black", asp=1, type="l", lwd=2, lty=1)
} #end while

## end of script #3 (plot of building onto graph)
###############################################################

#4: plot of outline with vertices & line-numbers onto enlarged orthoimage

display(img_uds,method = "raster")
n_x <- length(PC_nr)
vec_y <- 1 : n_x

#loop
for (i in vec_y) {
  cat("i=",i,"\n")
  #browser()
  points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), 
         pch=20, asp=1, cex=0.3, col="green")
  points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
  points(intsec_linepair_vertex_coord2$x-orig_x,
         (-intsec_linepair_vertex_coord2$y-orig_y),
         pch=20, asp=1, cex=1.5, col="red") 
  
  fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  b <- read.table(fname12,header=T)
  k3 <- nrow(b)
  names(b) <- c("Points_x","Points_y")
  
  #loop
  i <- 0
  while(i < (k3-1)) {
    
    i <- i+1
    lines(b$Points_x-orig_x, (-b$Points_y-orig_y),  col="blue", asp=1, type="l", lwd=2, lty=1)
    text(intsec_linepair_vertex_coord2$x[i]-orig_x,(-intsec_linepair_vertex_coord2$y[i]-orig_y), 
         labels = intsec_linepair_vertex_coord2$vertex_nr[i], 
         pos=2, offset = 0.7, cex = 1, col = "white")
    text(centers_PC[i,2]-orig_x,(-centers_PC[i,3]-orig_y), labels=centers_PC[i,1],
         pos=2, offset = 0.5, cex = 1, col = "red")
  } #end while
  
} #end for-loop

cat("table with line-pairs,vertex/corner-number,coordinates(x,y)","\n")
print(intsec_linepair_vertex_coord2)

## end of script #4 (plot of outline with vertices & line-numbers onto enlarged orthoimage)

################################################################################

## 5.plot of all processed buildings onto graph, orthoimage and ground truth

#example 1: orthoimage #7
#prj_title: ISPRS7_LCM1
#objects/buildings: 4,5,6,8,10,11,13,14,15,16,17,18,20,22,23,24,26,27,28,30,31,32,33,34

#example 2: orthoimage #1
#prj_title: ISPRS1_LCM2
#objects/buildings: 4,5,7,8,9,10,11,13,15,17,18,20,21,22,23,25,271,272,26,28,29,32,35,36,26,341,342,371,372,38,39,41,42,43,45,46
#dimension of orthoimage/GTS: 1919x2569 (WxH)

##plot of all buildings onto graph
setwd(home_dir)
x=0
y=0
plot(x,-y, pch=3, cex=1.3, cex.axis=1.3,cex.lab=1.3,col="red", 
       asp=1, xlim=c(1,img_x_max), ylim=c(-img_y_max,-1))

#input of file with all buildings
fname12 <- paste("./results/",Img_name,"/b_all.txt",sep="")
b_all <- read.table(fname12, header = FALSE) #study contents of file
b_all2 <- b_all

if (length(b_all) >= 2 ) {
  b_all2$V1 <- b_all$V1[order(b_all$V1, decreasing = FALSE)]  
}

b_all2$V1

#remove multiple object-numbers
n8 <- 13 #max number of tests per object (subject of change)
b_all_na <- rep(NA,n8)
b_all_lst <- list(b_all_na)
names(b_all_lst) <- "nr"
b_all_lst$nr[1] <- b_all2$V1[1]
b_all_lst$nr 
n_nr <- length(b_all2$V1)

if (n_nr > 1) { 
  vec_nr <- 1 : (n_nr)
} else {
  vec_nr <- 1
}

k <- 1
#loop
for (i in vec_nr) {
  
  if (b_all2$V1[i] != b_all_lst$nr[k]) { 
    k = k+1
    b_all_lst$nr[k] <- b_all2$V1[i]
  } #end if
  
} #end loop

b_all_lst$nr
b_all_nr <- b_all_lst$nr[!is.na(b_all_lst$nr)]
b_all_nr

#output
fname13 <- paste("./results/",Img_name,"/b_all_nrs.txt",sep="")
write.table(b_all_nr,fname13)

#loop
for (i in b_all_nr)  { 
  bnr2 <- i
  fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  setwd(home_dir)
  b <- read.table(fname12,header=T)
  k1 <- nrow(b)
  names(b) <- c("nr","Points_x","Points_y")
  cat("plot of building-outline","\n")
  lines(b$Points_x, b$Points_y, col="black", asp=1, type="l", lwd=1, lty=1)
} #end loop
#

##plot of all processed buildings onto orthoimage
setwd(OrgImgPathname)
img_ref <- readImage(OrgImgFilename)
display(img_ref, method = "raster")
#display(img_ref, method = "browser") #for measurement of coordinates

#loop
for (k in b_all_nr) {
  cat("k=", k, "\n")
  bnr2 <- k
  fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  setwd(home_dir)
  b <- read.table(fname12, header=T)
  b
  k1 <- nrow(b)
  names(b) <- c("nr","Points_x","Points_y")
  
  cat("plot of building-outline","\n")
  
  lines(b$Points_x, -b$Points_y, col="white", asp=1, 
          type="l", lwd=2, lty=1)
  
} #end loop
#

##plot of all processed buildings on ground truth (GT)
setwd(OrgGtsPathname)
img_GTS <- readImage(OrgGtsFilename)
display(img_GTS, method="raster")
b_all_nr

#loop
for (k in b_all_nr) {
  cat("k=", k, "\n")
  bnr2 <- k
  fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  setwd(home_dir)
  b <- read.table(fname12, header=T)
  b
  k1 <- nrow(b)
  names(b) <- c("nr","Points_x","Points_y")
  lines(b$Points_x, -b$Points_y, col="red", asp=1, type="l", lwd=2, lty=1)
} #end loop
#

##end of script 5 
################################################################################

#6: plot of a selected corner on orthoimage (small scale)
# to be used for checking of geometric accuracy
setwd(OrgImgPathname)
img_ref <- readImage(OrgImgFilename)
display(img_ref, method = "raster")

#loop
for (k in b_all_nr) {
  cat("k=", k, "\n")
  bnr2 <- k
  fname12 <- paste("./results/",Img_name,"/b",bnr2,"_coord_adj_plot.txt",sep="")
  setwd(home_dir)
  b <- read.table(fname12, header=T)
  k1 <- nrow(b)
  names(b) <- c("nr","Points_x","Points_y")
  
  cat("plot of building-outline","\n")
  
  lines(b$Points_x, -b$Points_y, col="white", asp=1, 
        type="l", lwd=2, lty=1)
  
  #plot corner i in all b
  i=2 #to be changed
  points(b$Points_x[i],-b$Points_y[i], pch=20, asp=1, cex=1.5, col="yellow")
  cat("building_nr= ", bnr2, "corner_nr= ",b$nr[i], "x= ", b$Points_x[i], "y= ", -b$Points_y[i], "\n")
} #end loop
#

#measurement of a single point in orthoimage
setwd(OrgImgPathname)
img_ref <- readImage(OrgImgFilename)
display(img_ref, method = "raster")

c10 <- locator(1) #standard function of locator
c10
#

## plot of a selected corner on orthoimage (large scale)
#display enlarged ortho_image and plot of PC of building outline
img_uds <- img_ref[orig_x:wind_x,orig_y:wind_y,1:3]
#

if (orig_x < 0) { #solves problems at edges of orthoimage
  orig_x = 0
}

if (orig_y < 0) {
  orig_y = 0 
}
#

display(img_uds, method = "raster")
points(xc-orig_x,yc-orig_y,pch=3, asp=1, cex=1.3, col="red")
points(as.integer(pc3$col-orig_x), as.integer(pc3$row-orig_y), 
       pch=20, asp=1, cex=0.3, col="green")

##manual measurement of 3 checkpoints (lower, upper, middle)
L1 <- trans_ortho() 

#derive transformation-matrix
D <- matrix(nrow=2, ncol=2)
D[1,1] <- L1[[1]][1,1]
D[1,2] <- L1[[1]][1,2]
D[2,1] <- L1[[1]][2,1]
D[2,2] <- L1[[1]][2,2]
a0 <- L1[[2]][1]
b0 <- L1[[2]][2]
tr_lat <- c(a0,b0)
kf2 <- L1[[3]]
#

## measurement of one new pixel (center of line)
#results: x,y in image-system of orthoimage)
locator2() #measurement and marking of one pixel's position

# plot of one corner i in all buildings (to be used for checking of geometric accuracy)

i=2 #to be adapted
points(b$Points_x[i]-orig_x,-(b$Points_y[i]-orig_y), pch=20, asp=1, cex=1.5, col="blue")
cat("building_nr= ", bnr2, "corner_nr= ",b$nr[i], "x= ", b$Points_x[i], "y= ", -b$Points_y[i], "\n")

#end of #6: plot of a selected corner onto orthoimage (small scale)
#################################################################################

#7: measurement of unmarked check point in zoomed orthoimage
setwd(OrgImgPathname)
img_ref <- readImage(OrgImgFilename)
display(img_ref, method = "browser")
#instruction: zoom-in to 200%
#read coordinates (row, column) of unmarked corner point

#end of #7: measurement of unmarked check point onto enlarged orthoimage
################################################################################

### 8.measurement of check points in zoomed reference map & output of map
setwd(OrgGtsPathname)
map_ref <- readImage(OrgGtsFilename)
display(map_ref, method = "browser")

#instruction: zoom-in to 200%
#read coordinates (row, column) of unmarked corner point

###9: generation of second extract
display(map_ref, method = "raster")
map_ref_extr2 <- map_ref[1:480,1:500,]
display(map_ref_extr2, method = "raster")
display(map_ref_extr2,method = "browser")
f="C:/Users/Joachim/R_programs/tests/LVA_Halle/data/DTK10/4130NW_col_extr2.tif"
writeImage(map_ref_extr2,file=f)

#end of script #8: measurement of check points in zoomed reference map
################################################################################

##end of supplementing software to script 'plot_results_on_references.R'
