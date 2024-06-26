##name of script: spObj_line_detection_v1.4.0.R
cat("version_number= ",v_nr,"\n")
##purpose: dealing with special objects in script 'line detection.R' 
##instructions: change of default values may be required
#default values: ref_line=1
#default values: n_pix=15 ("extr_wd"),25 ("4_long"),35 ("100_all"),15("100_all+nonortho")
#small rectangular lines can be detected by:
#changing the default value (n_pix=35) in 'line detection.R' or by
#pointing to one pixel using script 'support_line_detection.R'
##author: Joachim Höhle
##GNU General Public License (GPL)


##orthoimage "ISPRS7"

if (Img_name == "ISPRS7") { 

  ##b4
  #cas="100_all"
  
  if (bnr2 == 4 && p_pos == "cor_det") { 
     B5_6R4 <- B5_6
     n_B5_6 <- nrow(B5_6)
     n10 <- n_B5_6 + 1
     B5_6R4[n10,] <- B5_4_ord[5,] #calculation in 'support_line_detection.R'
     B5_6R4[n10,8] <- 1
     n11 <- n_B5_6 + 2
     B5_6R4[n11,] <- B5_4_ord[30,] #calculation in 'support_line_detection.R'
     B5_6R4[n11,8] <- 1
     n12 <- length(B5_6R4$lnr)
     row.names(B5_6R4) <- 1 : n12
  }
  
  
  ##b5
  #cas="100_all+nonortho"
  
  if (bnr2 == 5 && p_pos == "cor_det") { 
     lnr_det3 <- lnr_det3[-c(2,5)] #exchange of line
     lnr_det5 <- lnr_det3
  } #end b5
  
  
  ##b8
  #cas='100_all'
  
  if (bnr2 == 8 && p_pos == "cor_det") { 
     B5_6R4 <- B5_6
     B5_6R4[8,] <- B5_4[34,]
     B5_6R4[8,8] <- 1
     B5_6R4[9,] <- B5_4[24,]
     B5_6R4[9,8] <- 1
     B5_6R4[10,] <- B5_4[9,]
     B5_6R4[10,8] <- 1
  } #end b8
  
  
  ##b11
  #cas='100_all'
  
  if (bnr2 == 11 && p_pos == "cor_det") { 
     B5_4e_4long2 <- B5_4e_4long
     B5_4e_4long2[4,] <- v_par
     B5_4e_4long2$ortho <- 1
  } #end b11 
  
  
  ##b14
  #cas = "4_long" 
  
  if (bnr2 == 14 && p_pos == "cor_det") { 
     B5_6R4 <- B5_6[-4,] #drop of line
  } #end b14 
  
  
  ##b16
  #cas="100_all"
  
  if (bnr2 == 16 && p_pos == "cor_det") { 
     B5_6R4 <- B5_6[-3,]
     B5_6R4[8,] <- B5_6R4[6,]
  } #end b16 
  
  
  ##b17
  #cas="100_all+nonortho"
  
  if (bnr2 == 17 && p_pos == "cor_det") {
     lnr_det5 <- lnr_det3[-c(2,4)]
     lnr_det5[7] <- 100
  } #end b17  
    
  
  ##b18
  #cas="100_all+nonortho"
  
  if (bnr2 == 18 && p_pos == "cor_det") {
    lnr_det4 <- lnr_det3[-c(2,4,7,10)] 
    lnr_det4[10] <- c(5)
    lnr_det5 <- lnr_det4
  } #end b18
  
  ##b23
  #cas = "100_all"
  
  if (bnr2 == 23 && p_pos == "cor_det") {
     n2 <- nrow(B5_6)
     B5_6[n2+1,] <- B5_4[10,]
     B5_6 <- B5_6[-c(2:3,7:9,11:15),1:8]
     n_B5_6 <- length(B5_6$lnr)
     row.names(B5_6) <- 1 : n_B5_6
     B5_6$ortho <- 1
     B5_6R4 <- B5_6
  } #end b23
  #
  
  
  ##b24
  #cas="100_all"
  
  if (bnr2 == 24 && p_pos == "cor_det") {
    B5_6R4 <- B5_6[-c(2:3,5,7:8),1:8]
    n_B5_6R4 <- length(B5_6R4$lnr)
    n2_B5_6R4 <- n_B5_6R4 + 1
    B5_6R4[n2_B5_6R4,1:7] <- B5_4_ord[40,1:7]
    n3_B5_6R4 <- n2_B5_6R4 + 1
    B5_6R4[n3_B5_6R4,1:7] <- B5_4_ord[30,1:7]
    B5_6R4[n2_B5_6R4,8] <- 1
    B5_6R4[n3_B5_6R4,8] <- 1
  } #end b24
  #
  
  
  ##b27
  #cas="100_all"
  
  if (bnr2 == 271 && p_pos == "cor_det") { #first part of 27
     B5_6R4 <- B5_6[-c(2:3,6:7,9:10,12),]
     B5_6R4[10,] <- B5_4_ord[47,]
     B5_6R4[10,8] <- 1
  } #end b271 
  
  if (bnr2 == 272 && p_pos == "cor_det") { #second part of 27
     B5_6[23,] <- B5_6[10,]
     B5_6[24,] <- B5_4_ord[88,1:7]
     B5_6R4 <- B5_6[-c(2,6,11:18,20,22),]
     B5_6R4[12,8] <- 1
  } #end b272
  
  #end b27
  
  
  ##b28
  #cas="100_all"
  
  if (bnr2 == 28 && p_pos == "cor_det") { 
     B5_6[2,1:7] <- B4[454,1:7]
     B5_6[2,8] <- 1
     B5_6R4 <- B5_6
  } #end b28
  #
  
  
  ##b30
  #cas="100_all+nonortho"
  
  if (bnr2 == 30 && p_pos == "cor_det") { 
     lnr_det5 <- lnr_det3[-c(4:5)]
  } #end b30
  
  #
  
  
  ##b32
  #cas="100_all"
  
  if (bnr2 == 321 && p_pos == "cor_det") { 
    B5_6R4 <- B5_6[-c(2,4,7,9,10),]
    B5_6R4[8,] <- B5_6R4[2,]
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
  } #end b321
  
  #
  
  
  if (bnr2 == 322 && p_pos == "cor_det") { 
    B5_6R4 <- B5_6[-c(2,4,7,9),]
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6 <- B5_6R4
  } #end b322
  
  #end b32
  
  
  ##b34
  #cas="100_all+nonortho"
  
  if (bnr2 == 34 && p_pos == "cor_det") { 
    lnr_det5 <- lnr_det3[-c(2:3,5:11)]
  } #end b34
    
} #end of orthoimage "ISPRS7"

###############################################################################

##ISPRS1

if (Img_name == "ISPRS1") { 

  ##b5
  #cas="4_long"
  
  if (bnr2 == 5 && p_pos == "cor_det") { 
    B5_4e_4long[3,] <- c(1,21,20,302,100,95,95)
    B5_4e_4long2 <- B5_4e_4long
  } #end b5
  
  
  ##b10
  #cas='100_all'
  
  if (bnr2 == 10 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2,3,4,8,11,12,13,14,15,20),]
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b10
  
  
  ##b11
  #cas="100_all+nonortho"
  
  if (bnr2 == 11 && p_pos == "cor_det") {
    B5_6
    B5_6[5,] <- B4[173,] #correction of line
    B5_6R4 <- B5_6
    lnr_det5 <- B5_6R4$lnr
  } #end b11
  
  
  #b13
  #cas='100_all'
  
  if (bnr2 == 13 && p_pos == "cor_det") { 
      B5_6R4 <- B5_6[-c(2,3,7),]
      row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
  } #end b13
  
  
  #b15
  #cas='100_all'
  
  if (bnr2 == 15 && p_pos == "cor_det") { 
    B5_6R4 <- B5_6[-6,]
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
  } #end b15
  
  
  ##b16
  #cas='100_all'
  
  if (bnr2 == 16 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6
    n_B5_6 <- nrow(B5_6)
    index2 <- n_B5_6 + 1
    B5_6R4[index2,1:7] <- B4[625,1:7]
    B5_6R4[index2,8] <- 1
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
    B5_6R4
  } #end b16
  
  
  ##b17
  #cas='100_all'
  
  if (bnr2 == 17 && p_pos == "cor_det") { 
    B5_6R4 <- B5_6[-c(2,3,5,7),]
    row.names(B5_6R4) <- 1 : length(B5_6R4$lnr)
  } #end b17
  
  
  ##b21
  #cas='100_all'
  
  if (bnr2 == 21 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2,5,6,7),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
  } #end b21
  
  
  ##b22
  #ISPRS1
  #cas='100_all'
  
  if (bnr2 == 22 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2,5,6,8,10,11,13,14,16,17),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
  } #end b22
  
  
  ##b25
  #cas='100_all'
  
  if (bnr2 == 25 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2,3,6,7),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
  } #end b25
  
  ##b26
  #cas='100_all'
  if (bnr2 == 26 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2,4,5,7),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
  } #end b25
  
  ##b271
  #cas='100_all+nonortho'
  
  if (bnr2 == 271 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2,3,4,6,7,10,11,12,13),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4[nrow(B5_6R4)+1,] <- c(277,0,0,0,110,1425,55)
    B5_6R4[4,] <- c(742,23,1233,62,110,1420,34)
    lnr_det5 <- B5_6R4$lnr
  } #end b271
  
  
  ##b28
  #cas='100_all'
  
  if (bnr2 == 28 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(2,5),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
  } #end b28
  
  ##b35
  #cas='100_all'
  #ISPRS1
  
  if (bnr2 == 35 && p_pos == "cor_det") { 
    B5_6
    B5_6R4 <- B5_6[-c(4,5,9,11),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
  } #end b35
 
  
  ##b36
  #cas='100_all+nonortholines'
  
  if (bnr2 == 36 && p_pos == "cor_det") { 
    B5_7 <- B5_6
    B5_6R4 <- B5_7[-c(1,3:4,7:8),]
    B5_6R4
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    lnr_det5 <- B5_6R4$lnr
    lnr_det5
  } #end b36
  
  
  ##b372
  #cas='100_all+nonortholines'
  
  if (bnr2 == 372 && p_pos == "cor_det") { 
    B5_6
    B5_7 <- B5_6
    B5_6R4 <- B5_7[-c(1:3,5:9),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    lnr_det5 <- B5_6R4$lnr
  } #end b372
  
  
  ##b38
  #cas='4_long'
  
  if (bnr2 == 38 && p_pos == "cor_det") { 
    B5_4e_4long
    B5_4e_4long2 <- B5_4e_4long
    B5_4_ord2 <- subset(B5_4_ord, B5_4_ord$n_pixel >= wd)
    B5_4e_4long2[3,] <- B5_4_ord2[1,] #new line
    B5_4e_4long2  
  } #end b38
  
  ##b41
  if (bnr2 == 41 && p_pos == "cor_det") { 
    B5_6R4 <- B5_6
    B5_6R4 <- B5_6R4[-c(2),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
  } #end b41
  
  ##b42
  #cas='4_long'
  
  if (bnr2 == 42 && p_pos == "cor_det") { 
    B5_4e_4long
    B5_4e_4long2 <- B5_4e_4long
    wd=50
    B5_4_ord2 <- subset(B5_4_ord, B5_4_ord$n_pixel >= wd)
    B5_4e_4long2[3,] <- B5_4_ord2[3,] #new line
    B5_4e_4long2  
  } #end b42
  
  
  ##b45
  #cas='100_all+nonortho'
  
  if (bnr2 == 45 && p_pos == "cor_det") { 
    B5_6
    B5_7 <- B5_6
    B5_6R4 <- B5_7[-c(1:2,4:17),]
    row.names(B5_6R4) <- 1 : nrow(B5_6R4)
    B5_6R4
  } #end b45
  
} #end of orthoimage "ISPRS1"

##end of script 'spObj_line_detection_v1.4.0.R'


