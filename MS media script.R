# Writing a function to help in MS media preparations
MS_Media_Assistant <- function(volume_of_stock = "Total volume of stock you wish to prepare",
                               volume_of_MS_media = "Total volume of Ms media for experiment") {
  # Empty dataframe to store the various reagents and their weights.
  MS_media_table <- as.data.frame(matrix(nrow = 31,ncol = 3))
  MS_media_table
  
  # Assigning column names to the dataframe
  names(MS_media_table) <- c("Constituent of Stock", paste("Quantity (g/",volume_of_stock,"ml)"), paste("Volume in Medium (ml/",volume_of_MS_media,"ml)"))
  MS_media_table 
  # Character vector storing values of column 1 for the empty dataframe.
  Vec1 <- c(" ",'MACRONUTRIENT (10x)', "NH₄NO₃"," KNO₃", 'MgSO₄.7H₂0','KH₂PO₄',"CaCl.2H₂0"," ",
            "MICRONUTRIENT (1000x)", "H₃BO₃", "MnSO₄.7H₂0", "ZnSO₄.7H₂0", " KI","Na₂MoO₂.2H₂0",
            "CuSO₄·5H₂O","CoCL₂.2H₂0"," ", "IRON SOURCE","FeSO₄.7H₂0","Na₂EDTA","","VITAMINS (1000x)",
            "Nicotinic acid", "Thiamine","Pyridoxine","Glycine"," ", "OTHERS","Myo-inositol","Sucrose","Phytagel")
  # Character (numeric vector) storing values for column 2
  Vec2 <- c(" ",' ', 16.5,19, 3.7,1.7,4.4," ",
            " ", 6.2, 22.3, 8.6, 0.83,0.25,
            0.025,0.025 ," ", " ",27.8,37.3,""," ",
            0.5, 0.1,0.5,2," ", " ",0.1,30,3.5)
  # Character (numeric vector) storing values for column 3
  Vec3 <- c(" ",100, " ","  ", ' ',' '," "," ",
            1, " ", " ", " ", " "," ",
            " "," "," ", 1," "," ","",1,
            " ", " "," "," "," ", " "," "," "," ")
  
  # Using a for loop to fill columns 1 to 3  easily.
  for(i in 1:length(Vec1)) {
    MS_media_table[i,1] <- Vec1[i]
    
  }
  for(i in 1:length(Vec2)) {
    MS_media_table[i,2] <- Vec2[i]
    
  }
  for(i in 1:length(Vec3)) {
    MS_media_table[i,3] <- Vec3[i]
    
  }
  # Convert the character(numeric) subsets in column 2 to complete numeric data-types
  MS_media_table[c(3,4,5,6,7,10,11,12,13,14,15,16,19,20,
                   23,24,25,26,29,30,31),2] <- as.numeric(MS_media_table[c(3,4,5,6,7,10,11,12,13,14,15,16,19,20,
                                                                           23,24,25,26,29,30,31),2]) 
  MS_media_table
  # Convert the character(numeric) subsets in column 3 to complete numeric data-types
  MS_media_table[c(2,9,18,22),3] <- as.numeric(MS_media_table[c(2,9,18,22),3]) 
  MS_media_table  
  
  # Object to hold the default volume of column2 (g/L)  
  column2_param <- 1000
  catalyst1 <- volume_of_stock/column2_param # object to be used for calculation
  catalyst2 <- volume_of_MS_media/ volume_of_stock
  catalyst3 <- volume_of_MS_media/column2_param
  # Column 2 adjuster per input value
  MS_media_table[c(3,4,5,6,7,10,11,12,13,14,15,16,19,20,
                   23,24,25,26),2] <- as.numeric(MS_media_table[c(3,4,5,6,7,10,11,12,13,14,15,16,19,20,
                                                                  23,24,25,26),2])* catalyst1
  MS_media_table[c(29,30,31),2] <- as.numeric(MS_media_table[c( 29,30,31),2])* catalyst3
  
  
  # Column 3 adjuster per input value
  MS_media_table[c(2,9,18,22),3] <- as.numeric(MS_media_table[c(2,9,18,22),3])* catalyst2
  MS_media_table
  Volumes_to_take <- as.numeric(MS_media_table[c(2,9,18,22),3])
  Volumes_to_decant <- Volumes_to_take * catalyst2 # volume to be dispense
  text_description <- paste0(
    "To prepare ", volume_of_MS_media, " ml of MS Media for your Plant Tissue Culture Experiment, follow these steps:\n",
    "First prepare your stock solutions of volume ",volume_of_stock," ml, Check the MS protocol below to see how to do that.Next is:\n",
    "1. Take ", MS_media_table[2,3], " ml of Macronutient and dispense it into a beaker or a conical flask.\n",
    "2. Pipette ", round(Volumes_to_decant[2], 2), "ml of Micronutrient, ", 
    round(Volumes_to_decant[3], 2), " ml of  Vitamins, and ", 
    round(Volumes_to_decant[4], 2), " ml of Iron source. Use the right micropipette, while changing pipette tips.\n",
    "3. Weigh ",MS_media_table[29,2]," grams of Myo-inositol and ",MS_media_table[30,2]," grams of Sucrose.\n",
    "4. Top these volumes up with distilled water to the ", volume_of_MS_media, " ml mark.\n",
    "Note : Add the calculated weight as indicated in the protocol for phytagel after you have checked the pH of your MS Media.\n\n",
    "Below is the MS Media protocol with the necessary calculations easily done.\n\n"
  )
  
  return(MS_media_table = MS_media_table)
}
MS_Media_Assistant(500, 400)