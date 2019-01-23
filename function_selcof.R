# Create function to calculate selection coefficient
# Based off of code shared by Megan Fritz
# Created on: 7/25/17
# Last edited on: 7/25/17

# Assumes dataframe with structure as 
# > x1534
# year   RR SR  SS    n      freqR       CI_95      RRfreq     SRfreq      SSfreq
# 1  2000    4  7 385  396 0.01893939 0.013425777 0.010101010 0.01767677 0.972222222
# 2  2001    3 11 650  664 0.01280120 0.008550662 0.004518072 0.01656627 0.978915663
# 3  2002  141 23  75  239 0.63807531 0.060925934 0.589958159 0.09623431 0.313807531
# 4  2003  180 50  52  282 0.72695035 0.052000163 0.638297872 0.17730496 0.184397163
# 5  2004   24  5   3   32 0.82812500 0.130717997 0.750000000 0.15625000 0.093750000

selcof <- function(dataframe){
  # Create empty dataframe and name columns
  df <- data.frame()
  
  # Calculate relative survival for each genotype
  for(i in 1:(length(dataframe$RRfreq)-1)){
    relativeSurvival.RR <- dataframe$RRfreq[i+1] / dataframe$RRfreq[i]
    relativeSurvival.SR <- dataframe$SRfreq[i+1] / dataframe$SRfreq[i]
    relativeSurvival.SS <- dataframe$SSfreq[i+1] / dataframe$SSfreq[i]
    
    
    # Calculate relative fitness for each genotype
    relativeFitness.RR <- relativeSurvival.RR / relativeSurvival.RR
    relativeFitness.SR <- relativeSurvival.SR / relativeSurvival.RR
    relativeFitness.SS <- relativeSurvival.SS / relativeSurvival.RR
    
    # Calculate Selection coefficient for each genotype
    selectionCoeff.RR <- 1 - relativeFitness.RR
    selectionCoeff.SR <- 1 - relativeFitness.SR
    selectionCoeff.SS <- 1 - relativeFitness.SS
    
    # Calculate dominance (h) for heterozygote
    dominance <- selectionCoeff.SR / selectionCoeff.SS
    
    # Write output to dataframe
    df <- rbind(df, c(dataframe$year[i], selectionCoeff.SS, dominance))
  }
  # Name columns in dataframe
  colnames(df) <- c("Year", "Selection Coefficient", "Dominance")
  
  # Return dataframe
  return(df)
}
