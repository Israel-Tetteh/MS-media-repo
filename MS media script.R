library(roxygen2)
#' @param volume_of_stock This is the total stock volume the user  wishes to prepare for 
#' each constituents. Example desires to prepare 1000 ml for stock 1 to 4 etc.
#' @param volume_of_MS_media This is the total volume of complete MS media the person wishes to prepare
#' @return returns  the MS_media table ( data frame.)

# Writing a function to help in MS media preparations
MS_Media_Assistant <- function(volume_of_stock = "Total volume of stock you wish to prepare",
                               volume_of_MS_media = "Total volume of Ms media for experiment") {
  
 # To check and prompt if a parameter has a missing value.
  if(missing(volume_of_stock)) {
    stop("Error: volume_of_stock parameter is missing")
  }
  
  if(missing(volume_of_MS_media)) {
    stop("Error: volume_of_MS_media parameter is missing")
  }
  
  # To check and prompt if the paramter value is not numeric
  if(!is.numeric(c(volume_of_stock, volume_of_MS_media)) == TRUE) {
    stop("Input parameter values should be numbers only")
  }
  
   
  # Empty dataframe to store the various reagents and their weights.
  MS_media_table <- as.data.frame(matrix(nrow = 31, ncol = 3))
  
  # Assigning dynamic column names to Ms_media_table
  names(MS_media_table) <- c("Constituent of Stock", paste("Quantity (g/",volume_of_stock,"ml)"), 
                             paste("Volume for Media (ml/",volume_of_MS_media,"ml)"))
  
  # Character vector storing values of column 1 for the empty dataframe.
  Vec1 <- c(" ",'MACRONUTRIENT (10x)', "NH₄NO₃"," KNO₃", 'MgSO₄.7H₂0','KH₂PO₄',"CaCl.2H₂0"," ",
            "MICRONUTRIENT (1000x)", "H₃BO₃", "MnSO₄.7H₂0", "ZnSO₄.7H₂0", " KI","Na₂MoO₂.2H₂0",
            "CuSO₄·5H₂O","CoCL₂.2H₂0"," ", "IRON SOURCE","FeSO₄.7H₂0","Na₂EDTA","","VITAMINS (1000x)",
            "Nicotinic acid", "Thiamine","Pyridoxine","Glycine"," ", "OTHERS","Myo-inositol","Sucrose","Phytagel")
  
  # Character (numeric vector) storing values for column 2
  Vec2 <- c(" ",' ', 16.5,19, 3.7,1.7,4.4," ",
            " ", 6.2, 22.3, 8.6, 0.83,0.25,
            0.025,0.025 ," ", " ",27.8,37.3,""," ",
            0.5, 0.1,0.5,2," ", " ","" , "", "")
  
  # Character (numeric vector) storing values for column 3
  Vec3 <- c(" ",100, " ","  ", ' ',' '," "," ",
            1, " ", " ", " ", " "," ",
            " "," "," ", 1," "," ","",1,
            " ", " "," "," "," ", " ",0.1 ,30 ,3.5)
   
  # Using a for loop to fill columns 1 to 3  easily.
  for(i in 1:length(Vec1)) {
    MS_media_table[i,1] <- Vec1[i] # column 1
    
  }
  
  for(i in 1:length(Vec2)) {
    MS_media_table[i,2] <- Vec2[i] # column 2
    
  }
  
  for(i in 1:length(Vec3)) {
    MS_media_table[i,3] <- Vec3[i] # column 3
    
  }
   
  ##Defining important parameters 
  column2_param <- 1000  # Object to hold the default volume of column2 (g/L)  
  
  catalyst1 <- volume_of_stock/column2_param #object to update the weights of column 2 based on user specification
  
  catalyst2 <- volume_of_MS_media/ volume_of_stock #object to update column 3 based on specifications 
  
  # Column 2 adjuster per input value, - convert to numeric
  MS_media_table[c(3,4,5,6,7,10,11,12,13,14,15,16,19,20,
                   23,24,25,26),2] <- as.numeric(MS_media_table[c(3,4,5,6,7,10,11,12,13,14,15,16,19,20,
                                                                  23,24,25,26),2])* catalyst1
  
  # Column 3 adjuster per input value - convert to numeric
  MS_media_table[c(2, 9, 18, 22, 29, 30, 31), 3] <- as.numeric( MS_media_table[c( 2, 9, 18, 22, 29, 30, 31 ), 3]) * catalyst1 * catalyst2
 
  return(MS_media_table = MS_media_table)

}

MS_Media_Assistant(1000,400) 
