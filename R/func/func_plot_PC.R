##name of script: func_plot_PC.R
##purpose: plot of point cluster (PC) of one line segment in orthoimage-extract
##used in: sequence_of_lines.R
##argument(s): 
#PC_number...number of point cluster
##author: Joachim Höhle
##GNU General Public License (GPL)

plot_PC <- function(PC_number) { 
  n <- PC_number #number of PC (to be changed)
  setwd(home_dir)
  fname=paste("./data/",Img_name,"/b",bnr2,"_",n,".txt", sep="")
  P <- read.table(fname, col.names=c("idx","x","y")) #point cloud
  P_red <- reduce_pointset(P)
  points(P_red[,2] - orig_x, P_red[,3] - orig_y, pch=20, asp=3, cex=0.2, col="red")
} #end function plot_PC(PC_number)

##end of script 'func_plot_PC.R'