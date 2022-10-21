# Simplifies the Equalis allmänkemi poolat serum reporting
#
# One of the external QC programs that we subscribe to is a number of
# chemistry tests on a pooled serum sample. The procedure is that each
# analyte is analysed trice, and all three reported with three
# significant digits.
#
# Assumptions:
# 1. existence of a csv file with a systematic name
# (akpoolat_YYYY-MM.csv") and column names

# TODO: fix the ordering
# TODO: Figure out a way to add the eGFR calculation
#       how to pass in sex and age to a eGFR calculation?


library(tidyverse)
library(xlsx)



## Houskeeping
today <- Sys.Date()


ImportFileName <- paste0("akpoolat_", lubridate::year(today), "-", lubridate::month(today), ".csv")
ExportFileName <- paste0("akpoolat_", lubridate::year(today), "-", lubridate::month(today), ".xlsx")

# Ordering vector

ORDER <- data.frame(Analys=c( "ALAT", "Alb", "Alp", "Pamyl", "ASAT",
				     "Bil", "Bilkonj", "Ca", "CK", "Fosf",
				     "GluS", "GT", "HDL", "Järn", "K", "Klor",
				     "Kol", "Krea", "eGFR", "Laktat", "LD",
				     "LDL", "Mg2", "Na", "OsmS", "Tg", "Urat",
				     "Urea")
		    )

## load data
EQDATA <- read.csv2(ImportFileName, fileEncoding = "UTF-8-BOM", na.strings=c("-", ""))


## clean data
# fill down LIDs as needed
EQDATA <- tidyr::fill(EQDATA, LID)


## TheWork

# summary table

eqdata.sum <- EQDATA %>%
    group_by(Analys, LID) %>%
    summarise(
          Resultat = signif(Råvärde, 3), 
          Råvärde = Råvärde
          )

eqdata.sum <- left_join(ORDER, eqdata.sum, by = "Analys")


# TODO: ordering
# TODO: add eGFR

# A summary of summary: Equalis will evaluate our mean result, so lets
# do some calculations just in case

eqdata.report <- EQDATA %>%
	  group_by(Analys) %>%
	  summarise(
			Medel = signif(mean(Råvärde),3), 
			Min = signif(min(Råvärde),3), 
			Max= signif(max(Råvärde),3), 
			"CV%"=signif(100*sd(Råvärde)/mean(Råvärde),2)
	  )

eqdata.report <- left_join(ORDER, eqdata.report, by = "Analys")

eqdata.report.df <- as.data.frame(eqdata.report)

## Export result file
    
# Because the xlsx file becomes useless from a tibble
eqdata.sum.df <- as.data.frame(eqdata.sum)


# Write export file: excel with three tabs
xlsx::write.xlsx2(eqdata.sum.df, file=ExportFileName, sheetName = "TillEqualis")
xlsx::write.xlsx2(EQDATA, file=ExportFileName, sheetName = "Rådata", append = TRUE)
xlsx::write.xlsx2(eqdata.report.df, file=ExportFileName, sheetName =
			"EqualisRapportenJmf", append = TRUE)
