##name of script: spObj_adjustment_of_line.R
cat("version_number= ",v_nr,"\n")
#author: Joachim Höhle
#GNU General Public License (GPL)


cat("start of spObj_adjustment of line ","\n")


##buildings of orthoimage ISPRS_#7

if (Img_name == "ISPRS7") {  
  
  #b4
  if (bnr2 == 4 && p_pos == "cor_adj_line") {
    B6$theta_adj[3] <- B6$theta_ang[3] #correction of theta
    B6$ro_adj[3] <- B6$ro_pixel[3] #correction of ro
  }

  #b5
  if (bnr2 == 5 && p_pos == "cor_adj_line") {   
    B6$ro_adj[3] <- (-B6$ro_adj[3]) #correction of ro
    B6$theta_adj[3] <- 180 + B6$theta_adj[3] #angle must be positive
  } 

  #b24
  if (bnr2 == 24 && p_pos == "cor_adj_line") {   
    B6$theta_adj[5] <- B6$theta_ang[5] #correction of theta (line 315)
    B6$ro_adj[5] <- B6$ro_pixel[5] #correction of ro (line 315)
  } 

} #end of ISPRS7


##buildings of orthoimage ISPRS1

if (Img_name == "ISPRS1") {
  
  #b16
  if (bnr2 == 16 && p_pos == "cor_adj_line") { 
      B6 <- B6 #(dummy line)
  } 
  
  #b271
  if (bnr2 == 271 && p_pos == "cor_adj_line") { 
    B6 <- B6 #dummy line
  } #end b271

  #b372
  if (bnr2==372 && p_pos == "cor_adj_line") {
   B6$theta_adj[2] <- B6$theta_ang[2]
   B6$ro_adj[2] <- B6$ro_pixel[2]
   B6$theta_adj[3] <- B6$theta_ang[3]
   B6$ro_adj[3] <- B6$ro_pixel[3]
   B6 
  } #end of b372
  
  #b38
  if (bnr2 == 38 && p_pos == "cor_adj_line") {
    B6 <- B6 #dummy line 
  } #end of b38

  #b46
  if (bnr2 == 46 && p_pos == "cor_adj_line") {
    B6 <- B6 #(dummy line) 
  } #end of b46
  
} #end of ISPRS1

##end of script 'spObj_adjustment_of_line_v1.4.0.R' 


