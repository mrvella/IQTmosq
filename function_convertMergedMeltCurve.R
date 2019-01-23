### These functions are used to convert the melt curve locus data into genotypes

### For the 1016 Locus
# HomozygousLow melt curve results are homozygous for the resistance allele
# Heterozygous melt curve results are heterozygous for the resistance and susceptible allele
# HomozygousHigh melt curve results are homozygous for the susceptible allele



convert1016 <- function(mergedData){
  x <- ifelse(mergedData$V1016I == "HomozygousLow", "RR"
         , ifelse(mergedData$V1016I == "Heterozygous", "SR"
                  , ifelse(mergedData$V1016I == "HomozygousHigh", "SS"
                           , "error")))
  return(x)
}


### For the 1534 Locus
# HomozygousLow melt curve results are homozygous for the susceptible allele
# Heterozygous melt curve results are heterozygous for the resistance and susceptible allele
# HomozygousHigh melt curve results are homozygous for the resistance allele

convert1534 <- function(mergedData){
  x <- ifelse(mergedData$F1534C == "HomozygousLow", "SS"
              , ifelse(mergedData$F1534C == "Heterozygous", "SR"
                       , ifelse(mergedData$F1534C == "HomozygousHigh", "RR"
                                , "error")))
  return(x)
}


### For the 410 Locus
# HomozygousLow melt curve results are homozygous for the resistance allele
# Heterozygous melt curve results are heterozygous for the resistance and susceptible allele
# HomozygousHigh melt curve results are homozygous for the susceptible allele

convert410 <- function(mergedData){
  x <- ifelse(mergedData$V410L == "HomozygousLow", "RR"
              , ifelse(mergedData$V410L == "Heterozygous", "SR"
                       , ifelse(mergedData$V410L == "HomozygousHigh", "SS"
                                , "error")))
  return(x)
}










