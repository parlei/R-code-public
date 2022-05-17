# Allergi-analyser över tid (äggvita(f1), mjölk(f2), torsk(f3) och
# vete(f4))

library(tidyverse)
library(xlsx)

# importera fyra filer och slå ihop
f1 <- read.xlsx2("f1_2017-2022.xls", sheetIndex = 1)
f2 <- read.xlsx2("f2_2017-2022.xls", sheetIndex = 1)
f3 <- read.xlsx2("f3_2017-2022.xls", sheetIndex = 1)
f4 <- read.xlsx2("f4_2017-2022.xls", sheetIndex = 1)

f1234 <- bind_rows(f1, f2, f3, f4)

# Datum från år, År från datumm, och ta bort 2022 (endast 5 mån in i 2022)
f1234$Datum <- as.Date(f1234$Provtagningstid, na.rm=TRUE)
f1234$År <- strftime(f1234$Datum, format="%Y")
f1234 <- filter(f1234, År !="2022")

# ta bort tomma rader
f1234<- filter(f1234, Analys != "")

# Skapa summerande tabell opch exportera
f1234.summary2 <- f1234 %>%
group_by(Analys, År) %>%
summarize(Antal = length(Resultat))

write.csv2(f1234.summary2, file="sammanfattning_f1-4_2016-2021.csv")

# Graf: 
# * stapelgraf, 
# * ren bakgrund, 
# * rätt etikett på y-axeln, 
# * årtalen på x-axeln är uppenbarligen år, 
# * antalen ovanför staplarna och 
# * uppdelat på en graf per analys.
# Exporterat till fil.

ggplot(f1234, aes(x=År, fill=Analys ))+ 
	  geom_bar( stat="count") + 
	  theme_minimal()+ 
	  labs(x=NULL, y="Antal", fill="Analyser")+
	  geom_text(stat="count", aes(label=..count..), vjust=-1)+ 
	  coord_cartesian(y=c(0,270))+ 
	  facet_wrap(~Analys, ncol = 1)

ggsave("f1+2+3+4_2016-2021.png", dpi=600)

