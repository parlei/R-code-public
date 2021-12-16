library(ISOweek)
library(digest)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(grid)
library(gridExtra)
library(lubridate)
library(stringr)
library(tidyr)
library(xlsx)

#vecka <- 32

idag = Sys.Date()

förraveckan = strftime(as.Date(idag) - 7, format="%V")
förraveckan = isoweek(idag-7)
isoyearvecka <- paste0(isoyear(idag-7), "v", isoweek(idag-7))
ISOvecka <- ISOweek(idag-7)
vecka <- förraveckan

isoyearvecka <- paste0(isoyear(idag-7), "v", isoweek(idag-7))
isoyearvecka <- ISOweek(idag-7)




källfil <- paste0("acov-", isoyearvecka,".csv")
källfil <- paste0("acov-", ISOvecka,".csv")


acov.all <- read.csv2(källfil,
			    fileEncoding = "UTF-8-BOM", na.strings=c("-",
										   "Prov
										   saknas",
										   ""))


#######  Inga ändringar nedaför denna linje (normalt)   ########



# Döp om columnen Ålder
colnames(acov.all)[3] <- "ålder"

# Skapa resultat-kolumn
acov.all$Resultat<- cut(acov.all$Mätvärde, c(-1, 0.79999999, 19.999999, 100000),
			  labels=c("NEG", "GRÄNS", "POS"))



# Städa bort icke-svar

acov.all <- filter(acov.all, Resultat == "POS" | Resultat == "NEG" | Resultat == "GRÄNS" )

acov.all$Datum.tek <- as.Date(acov.all$Tekniskt.godkännande, na.rm = TRUE)
acov.all$Vecka <- strftime(acov.all$Datum.tek, format="%V")
acov.all$Vecka <- isoweek(acov.all$Datum.tek)
acov.all$År <- strftime(acov.all$Datum.tek, format="%Y")
#acov.all$År <- strftime(acov.all$Datum.tek, format="%Y")

# Använder isoyear för att vara kompatibel med ISO 8601 i veckorna!
acov.all$isoyear <- isoyear(acov.all$Datum.tek)
acov.all$år.vecka <- paste0(acov.all$isoyear,"v",acov.all$Vecka)
acov.all$år.vecka <- ISOweek(acov.all$Datum.tek)



# Åldergrupper
#  Sätt okänd ålder till 500
acov.all$ålder[is.na(acov.all$ålder)] <- 500


# Åldergrupper: 500-åringar  blir okända
acov.all$Åldersgrupp<- cut(acov.all$ålder, c(0, 
							   4.999999, 
							   9.999999,
							   14.999999,
							   17.999999,
							   19.999999,
							   29.999999,
							   39.999999,
							   49.999999,
							   59.999999,
							   64.999999,
							   69.999999,
							   74.999999,
							   79.999999,
# 								5, 10, 15, 18, 20, 30,
# 								40, 50, 60, 65, 70, 75, 80,
							     200, 1000),
			  labels=c(
"0-4" , "5-9" , "10-14" , "15-17" , "18-19" , "20-29" , "30-39" ,
"40-49" , "50-59" , "60-64" , "65-69" , "70-74" , "75-79" , "80+",
"Okänd"
				     ))

#  Sätt okänd ålder till NA
acov.all$ålder[acov.all$ålder== 500] <- NA



#replace_na(acov.all$Åldergrupp, "Okänd")

# bara aktuell vecka
acov.all.vecka <- filter(acov.all, år.vecka==isoyearvecka)
n.pnr.vecka <- length(unique(acov.all.vecka$PatientID))

# Skapa tabell med veckans resultat: summera, justera ordning och lägg till
# totalnummor
tab.fhm.vecka <- table(acov.all$år.vecka, acov.all$Resultat)
#tab.fhm.vecka<- tab.fhm.vecka[1:15, 3:1]
tab.fhm.vecka<- tab.fhm.vecka[, 3:1]
tab.fhm.vecka <-cbind(tab.fhm.vecka, Totalt = rowSums(tab.fhm.vecka))
tab.fhm.vecka <-rbind(tab.fhm.vecka, Totalt = colSums(tab.fhm.vecka))

# tab för åldersgrupper: summera, justera ordning och lägg till
# totalnummor

# Testa om vi har minst en i varje resultat-grupp:
vecka.gräns <- nrow(filter(acov.all.vecka, Resultat == "GRÄNS"))
vecka.pos <- nrow(filter(acov.all.vecka, Resultat == "POS"))
vecka.neg <- nrow(filter(acov.all.vecka, Resultat == "NEG"))

if(vecka.gräns == 0 || vecka.pos == 0|| vecka.neg == 0){
tab.fhm.åldersgrupper<- table(acov.all.vecka$Åldersgrupp, acov.all.vecka$Resultat)
tab.fhm.åldersgrupper<- tab.fhm.åldersgrupper[, c("POS", "NEG", "GRÄNS")]
} else {
tab.fhm.åldersgrupper<- table(acov.all.vecka$Åldersgrupp, acov.all.vecka$Resultat)
tab.fhm.åldersgrupper<- tab.fhm.åldersgrupper[, c("POS", "NEG", "GRÄNS")]
}

tab.fhm.åldersgrupper<- cbind(tab.fhm.åldersgrupper, Totalt = rowSums(tab.fhm.åldersgrupper))
tab.fhm.åldersgrupper<- rbind(tab.fhm.åldersgrupper, Totalt = colSums(tab.fhm.åldersgrupper))

cat("\nSummerade resultat per vecka.\n")
print(tab.fhm.vecka)
cat("\n")

cat("\n")

# Finns det flera test på samma patient?
fleratest<-acov.all.vecka$PatientID[duplicated(acov.all.vecka$PatientID)]


# Kolla om det är möjligt att det finns dubletttester

if (any(ftable(acov.all.vecka$Kön, 
		  round(acov.all.vecka$ålder, 1)) >1 )) {
	  cat("\n Det kan finns flera provtagningar på samma patient (kön+ålder). \n")
	  } else {
		    cat("\n Det ser inte ut att finnas flera provtagningar på samma patient (kön+ålder). \n")
				     }

# Skriv ut de rader som härhör till samma PatientID

crosstab<- ftable(acov.all.vecka$Åldersgrupp, #acov.all.vecka$Vecka,
		     acov.all.vecka$Resultat)


#crosstab <-cbind(crosstab, Totalt = rowSums(crosstab))

##   

# Totalt antal för FHM
#

#tot.analyser <- tab.fhm.vecka[1] + tab.fhm.vecka[2] + tab.fhm.vecka[3]

# skapa CSV-fil för egen loggning
tabfilnamn <- paste0("acov-v",vecka, "-fhm2-b.csv")
write.csv2(tab.fhm.vecka, tabfilnamn)

tabfilnamn.xlsx <- paste0("acov-v",vecka, "-fhm2.xlsx")

write.xlsx(crosstab, tabfilnamn.xlsx,
	     col.names=TRUE,  row.names=FALSE, showNA = FALSE,
			 sheetName="Åldersgrupper", append = FALSE)

write.xlsx(tab.fhm.vecka, tabfilnamn.xlsx,
	     col.names=TRUE,  row.names=FALSE, showNA = FALSE,
			 sheetName="Veckor", append = TRUE)

if(vecka.gräns == 0 || vecka.pos == 0|| vecka.neg == 0){
cat("\n*** Det saknas resultat i en grupp: notera ordningen på kolumnerna!***\n")
}
cat("\nSummerade resultat per åldersgrupp för vecka ", vecka, "\n")
print(tab.fhm.åldersgrupper)


cat("\n")
cat("\nAntal utan ålder: ", nrow(filter(acov.all.vecka, is.na(ålder))), "\n")
print(filter(acov.all.vecka, is.na(ålder)))
cat("\n")



### Sammanställning för varje vecka ###

# rensa innevarande vecka

acov.tmp <- filter(acov.all, Vecka != strftime(as.Date(idag), format="%V"))


# Översikt
hela.ålder<- table(acov.tmp$Åldersgrupp, acov.tmp$Resultat)

  	  write.xlsx(hela.ålder, "alla-veckor-ålder.xlsx",
  	     col.names=TRUE,  row.names=TRUE, showNA = FALSE,
  			 sheetName="Hela perioden", append = FALSE)
 
  	  write.xlsx(tab.fhm.vecka, "alla-veckor-ålder.xlsx",
  	     col.names=TRUE,  row.names=TRUE, showNA = FALSE,
  			 sheetName="Alla", append = TRUE)
 

# För varje vecka

cat("Sammanfattande fil, sparar vecka ")
for (vka in unique(acov.tmp$år.vecka)){
	   cat(vka, " ")

 	  veckans <- filter(acov.tmp, år.vecka == vka)


tab.vka<- table(veckans$Åldersgrupp, veckans$Resultat)
tab.vka<- cbind(tab.vka, Totalt = rowSums(tab.vka))
tab.vka<- rbind(tab.vka, Totalt = colSums(tab.vka))


 	  veckoflik <- paste0(vka)

  	  write.xlsx(tab.vka, "alla-veckor-ålder.xlsx",
  	     col.names=TRUE,  row.names=TRUE, showNA = FALSE,
  			 sheetName=veckoflik, append = TRUE)
 
 }



# Städa
 rm(acov.tmp,veckans) 

### Slut på sammanställning för varje vecka
