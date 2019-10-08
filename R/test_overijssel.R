# Code om KRW trends te bepalen voor de provincie Overijssel

# Laad benodigde packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, maps, sf, ggmap, 
               ggspatial, stringi, openxlsx)

# eerst installeren met devtools::install()
library(KRWTrends)
devtools::load_all()

# Laad gevalideerde KMG dataset in
PMG <- read.delim2("/r-schijf/M607402 Grondwater KRW/Trends workshop 2017/Test Overijssel/UNIONStoffenselectieProvincienamenCoordinatenGWL.txt",
                   sep = ";", header = TRUE, stringsAsFactors = FALSE)

# Selecteer alleen de provincie Overijssel
# en selecteer relevante kolommen
ov_pmg <- PMG %>%
  filter(PROV == "OV") %>%
  filter(Parametercode %in% c("Cl", "NO3", "Ptot", "Cd", "Pb", "Ni", "As")) %>%
  mutate(Parametercode = Parametercode %>% tolower())

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Voeg kolom toe met filters
ov_pmg <- ov_pmg %>%
  mutate(diepte = ifelse(substrRight(as.character(Monsteridentificatie), 1) == 1,
                         "ondiep", "diep")) %>%
  mutate(meetnet = ifelse(substrRight(as.character(Namespace), 4) == "RIVM",
                          "LMG", "PMG")) %>%
  select(-PROV, -Namespace, -Identificatie, -Meetpuntidentificatie, 
         -Grootheidcode, -Hoedanigheidcode, -LocatieTypeWaardeBepalingid)

# Voeg grondwaterlichamen per putfilter toe
# maak punt-data van alle putfilters
#pts <- st_as_sf(ov_pmg, coords = c("X", "Y"), crs = 28992) %>% select(Monsteridentificatie)
# Voeg grondwaterlichaamnummer toe aan puntdata
#output <- st_intersection(pts, grondwaterlichamen)
#output2 <- output %>% data.frame() %>% select(Monsteridentificatie, naam)

# Voeg grondwaterlichamen toe aan dataset
ov_pmg$grondwaterlichaam <- output2[match(ov_pmg$Monsteridentificatie, output2$Monsteridentificatie), 2] %>% as.character()

ov_pmg <- ov_pmg %>% 
  mutate(meetjaar = lubridate::year(as.Date(Resultaatdatum, format = "%d-%m-%Y")),
         verwijder = 0, 
         Limietsymbool = ifelse(Limietsymbool == "<", 1, 0))

# Rename de kolomnamen en zet ze in goede dataformat
ov_pmg <- ov_pmg %>%
    rename(putfilter = Monsteridentificatie,
         meetnet = meetnet,
         xcoord = X,
         ycoord = Y,
         diepte = diepte,
         parameter = Parametercode,
         detectielimiet = Limietsymbool,
         waarde = Numeriekewaarde,
         eenheid = Eenheidcode,
         grondwaterlichaam = grondwaterlichaam) %>%
    select(-Resultaatdatum, -Veld11)

ov_pmg <- ov_pmg[, c("putfilter", "meetnet", "xcoord", 
                     "ycoord", "meetjaar", "diepte", 
                     "parameter", "detectielimiet",
                     "waarde", "eenheid", "grondwaterlichaam",
                     "verwijder")]

# Selecteer alle metingen over de periode 1998-2018
ov_pmg <- ov_pmg %>%
  filter(meetjaar > 1997)

# Voeg norm kolom toe
ov_pmg$norm <- mapply(dw, ov_pmg$parameter, ov_pmg$grondwaterlichaam)
 
# Output directory:
output <- "/r-schijf/M607402 Grondwater KRW/Trends workshop 2017/Test Overijssel/output/"
  

# ===========================================================================================
# TABEL 1 - Normoverschrijdende stoffen per grondwaterlichaam
# ===========================================================================================

# Overzicht van normoverschrijdende stoffen per grondwaterlichaam/pompstation
# Waardes onder de detectielimiet worden niet meegenomen
# Selecteer stoffen met 0.75 x normoverschrijding
toetsnorm = 0.75
d <- ov_pmg %>%
  mutate(waarde = ifelse(detectielimiet == 1, 0, waarde)) %>%
  mutate(dwratio = waarde / norm) %>%
  filter(dwratio > toetsnorm & dwratio <  1) %>%
  group_by(grondwaterlichaam) %>%
  summarise("75% normoverschrijding" = paste(sort(unique(parameter)), collapse = ", "))
x <- d %>% as.data.frame() 

# Selecteer stoffen met 1 x normoverschrijding
toetsnorm = 1
d <- ov_pmg %>%
  mutate(waarde = ifelse(detectielimiet == 1, 0, waarde)) %>%
  mutate(dwratio = waarde / norm) %>%
  filter(dwratio >= toetsnorm) %>%
  group_by(grondwaterlichaam) %>%
  summarise("100% normoverschrijding" = paste(sort(unique(parameter)), collapse = ", "))
y <- d %>% as.data.frame()

# Voeg 0.75 en 1 x stof normoverschrijding samen
z <- full_join(x, y) %>%
  arrange(grondwaterlichaam) %>% replace(.== "NULL", NA)

# Verwijder stoffen bij 0.75 x normoverschrijding welke ook voorkomen bij 1 x normoverschrijding
z$`75% normoverschrijding` <- mapply(function(x, y) setdiff(x, y), 
                                     strsplit(as.character(z$`75% normoverschrijding`), ", "), 
                                     strsplit(as.character(z$`100% normoverschrijding`), ", "))
z$`75% normoverschrijding` <- z$`75% normoverschrijding` %>% replace(.== "character(0)", NA)

# Maak verschil kolom weer één string per row
z <- z %>% mutate_at(
  vars(`75% normoverschrijding`),
  function(x){
    stri_paste_list(x, sep = ", ")
  })

# Tel normoverschrijdende stoffen per grondwaterlichaam
z <- z %>%
  group_by(grondwaterlichaam) %>%
  mutate("aantal >75%" = length(unique(unlist(strsplit(`75% normoverschrijding`, ", "))))) %>%
  mutate("aantal >100%" = length(unique(unlist(strsplit(`100% normoverschrijding`, ", "))))) %>%
  ungroup()
z <- z[, c(1, 2, 3)] 
write.xlsx(z, paste(output, "Tabel_1.xlsx"))

# ===========================================================================================
# TABEL 2 - Bemonsteringsfrequenties
# ===========================================================================================

d <- ov_pmg %>%
  filter(parameter == "no3", diepte == "ondiep", grondwaterlichaam == "NLGW0003")



a <- d %>%
  group_by(grondwaterlichaam, diepte) %>%
  count(putfilter) %>%
  mutate(valid = ifelse(n > 10 & n < 20, TRUE, FALSE))

b <- a %>%
  group_by("aantal metingen" = n) %>%
  summarise("aantal putfilters" = length(unique(putfilter))) %>%
  spread()


resultaat <- data.frame()
for(i in a$grondwaterlichaam) {
  
  b <- a %>%
    filter(grondwaterlichaam == i)
  
  for(j in b$diepte) {
    
    c <- b %>%
      count(putfilter)
    e <- c %>% 
      group_by("aantal metingen" = n) %>%
      summarise("aantal putfilters" = length(unique(putfilter))) %>%
      spread()
    
    resultaat <- resultaat %>%
      bind_rows(data.frame(grondwaterlichaam = i, diepte = j, 
                e$`aantal metingen`, e$`aantal putfilters`,
                stringAsfactors = FALSE))
    }
}


# Aantal strengen per aantal meetjaren -> TABEL 2
resultaat <- a %>%
  group_by("aantal metingen" = n) %>%
  summarise("aantal putfilters" = length(unique(putfilter)))
view(resultaat)
write.xlsx(resultaat, "/s-schijf/witm/REWAB_Trend/REWAB_trendanalyse/output/Tabel_2.xlsx")






