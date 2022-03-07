lat.gradient <- function(taxon, time_int) {
      occurrences <- read.csv(paste("http://paleobiodb.org/data1.2/occs/list.txt?base_name=",taxon,"&interval=",time_int,"&show=ident,paleoloc&limit=all",sep=""))
      
      #deletes occurrences not resolved to at least genus level using classified and unclassified species
      cleaned_occs <- subset(occurrences, occurrences$accepted_rank == "genus")
      
      cleaned_occs$primary_name <- factor(cleaned_occs$primary_name)
      
      #summarizes maximum and minimum paleolatitude of each genus
      maxlat <- sapply(split(cleaned_occs$paleolat,cleaned_occs$primary_name),function(x) max(x,na.rm=T))
      minlat <- sapply(split(cleaned_occs$paleolat,cleaned_occs$primary_name),function(y) min(y,na.rm=T))
      
      paleolats <- data.frame(maxlat, minlat)
      
      lats <- seq(-90,89,by=1)
      latdiv <- numeric(0)
      
      #finds genera that occur between given paleolatitudes
      for (i in seq(1:180)) {
            latdiv[i] <- length(which(paleolats$maxlat>lats[i] & paleolats$minlat<lats[i]))
      }
      
      #plots results
      plot(lats,latdiv,type="l",lwd=2,xlab="Paleolatitude",ylab="Genus Diversity")
}
