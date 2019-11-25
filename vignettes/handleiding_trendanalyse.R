## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# Laad benodigde packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, maps, sf, ggmap, 
               ggspatial)

# Laad KRWTrends pakket
# eerst installeren met devtools::install()
devtools::load_all()


## ------------------------------------------------------------------------
str(grondwaterdata)
head(grondwaterdata)

## ------------------------------------------------------------------------
x <- getgwl()
print(x)

## ------------------------------------------------------------------------
dw(param = "no3", gwl = "NLGW0001")

## ------------------------------------------------------------------------
grondwaterdata$norm <- mapply(dw, grondwaterdata$parameter, grondwaterdata$grondwaterlichaam)
head(grondwaterdata)

## ----fig.width = 6, fig.height = 3, fig.align = "center"-----------------

d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep") 

# Pas RG niet aan
figuur <- mktrends("213f1", d, rpDL = FALSE, dw.plot = FALSE, make.plot = TRUE)
print(figuur)

# Pas RG wel aan
figuur <- mktrends("213f1", d, dw.plot = FALSE, make.plot = TRUE)
print(figuur)


## ------------------------------------------------------------------------
# selecteer no3, vervang rapportagegrens met 0.5 * RG
d <- grondwaterdata %>% 
    filter(grondwaterlichaam == "NLGW0003", 
           parameter == "no3", diepte == "ondiep") 

# voorbeelddata is afkomstig van grondwaterlichaam NLGW0003
resultaat<-toetsNormoverschrijding(d)

knitr::kable(resultaat)

## ------------------------------------------------------------------------
resultaat <- data.frame()
for (i in c("no3", "as")) {

    d <- grondwaterdata %>% 
        filter(grondwaterlichaam == "NLGW0003", parameter == i, diepte == "ondiep") 
    resultaat <- resultaat %>%
        bind_rows(data.frame(param = i, toetsNormoverschrijding(d),
                             stringsAsFactors = FALSE))
}

knitr::kable(resultaat, digits = 2)

## ----fig.width = 6, fig.height = 3, fig.align = "center"-----------------
# Selecteer één parameter om maar één meetronde mee te nemen, 
# voor meerdere parameters worden er extra bemonsteringen geteld

# maak overzicht van de meetfrequentie
d <- monsterFreq(grondwaterdata, param = "no3")
knitr::kable(d)

# Histogram 
d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep") %>%
    droplevels()

a <- d %>% count(putfilter) %>%
  mutate(above = n > 10)

ggplot(a, aes(x = n, fill = above)) +
  geom_histogram(binwidth = 1,
    color = "darkblue") +
  geom_vline(aes(xintercept = mean(n), color = "gemiddelde"),
             linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = 10, color = "kritieke.grens"),
             linetype = "dashed", size = 1) +
  scale_y_continuous(breaks = 0:9) +
  ggtitle("Bemonsteringsfrequenties") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position = "right") +
  labs(x = "Aantal x bemonsterd", y = "Aantal filters") + 
  scale_color_manual(name = "lijn", values = c(gemiddelde = "blue", kritieke.grens = "red"))


## ------------------------------------------------------------------------
d <- grondwaterdata %>%
  filter(parameter == "no3", diepte == "ondiep", putfilter == "5f1") 

resultaat <- meetverdeling(d)

knitr::kable(resultaat)


## ------------------------------------------------------------------------
resultaat <- data.frame()
for (i in unique(grondwaterdata$putfilter)) {
  
  d <- grondwaterdata %>%
  filter(parameter == "no3", diepte == "ondiep", putfilter == i) 
  
  resultaat <- resultaat %>%
    bind_rows(data.frame(put = i, meetverdeling(d),
                         stringsAsFactors = FALSE)) 
}
# Voor een beter overzicht wordt de data van long naar wide format omgezet
resultaat <- resultaat %>% spread(key = tijdsperiode, value = aantal.metingen, fill = 0)
knitr::kable(head(resultaat))


## ------------------------------------------------------------------------
d <- grondwaterdata

resultaat <- meetverdeling2(d, "no3", "ondiep")
knitr::kable(head(resultaat))


## ------------------------------------------------------------------------

d <- grondwaterdata %>% 
    filter(grondwaterlichaam == "NLGW0003", parameter == "no3", diepte == "ondiep")
    
resultaat <- toetsStatistiek(d)
                             
knitr::kable(resultaat, digits = 2)


# Voor meerdere stoffen
resultaat <- data.frame()
for (i in c("no3", "as")) {

    d <- grondwaterdata %>% 
        filter(grondwaterlichaam == "NLGW0003", 
               parameter == i, diepte == "ondiep")
    
    resultaat <- resultaat %>%
        bind_rows(data.frame(param = i, toetsStatistiek(d),
                             stringsAsFactors = FALSE))

}
knitr::kable(resultaat, digits = 2)


## ----fig.width = 6, fig.height = 3, fig.align = "center"-----------------

d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep")

statistiek <- mktrends("5f1", d, make.plot = FALSE)
figuur <- mktrends("5f1", d, make.plot = TRUE)

print(statistiek)
print(figuur)


## ------------------------------------------------------------------------

# selecteer data uit ondiepe filters voor NO3
d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep") 

# Bereken de Mann-Kendall statistieken voor alle put locaties
i <- as.character(unique(d$putfilter))
res2  <- lapply(i, FUN = mktrends, d) %>%
    do.call("rbind", args = .) %>%
    na.omit()

# Bereken de false discovery rate voor alle locaties
fdr <- res2 %>% select(p) %>% bhfdr()

print(fdr)

## ------------------------------------------------------------------------

d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep") 

# Totaal aantal reeksen waarvoor trendanalyse wordt gedaan, nodig om 2 percentages te berekenen
aantal.reeks <- paste(d$putfilter, d$parameter, sep = " - ") %>% unique %>% length()

resultaat <- trendAnalyse(d, n.reeks = aantal.reeks)
knitr::kable(resultaat, digits = 2)

## ------------------------------------------------------------------------
resultaat <- data.frame()
for (i in c("no3", "as")) {

    d <- grondwaterdata %>% 
        filter(parameter == i, diepte == "ondiep") 
    
    aantal.reeks <- paste(d$putfilter, d$parameter, sep = " - ") %>% unique %>% length()
    
    resultaat <- resultaat %>%
        bind_rows(data.frame(param = i, trendAnalyse(d, n.reeks = aantal.reeks),
                             stringsAsFactors = FALSE))

}
knitr::kable(resultaat, digits = 2)


## ----fig.width = 6, fig.height = 3, fig.align = "center"-----------------

# Rapportagegrens aanpassen volgens methode
d <- grondwaterdata %>% 
    filter(parameter == "as", diepte == "ondiep") 

statistiek <- mktrends("17f1", d, rpDL = TRUE, make.plot = FALSE)
figuur <- mktrends("17f1", d, , rpDL = TRUE, dw.plot = FALSE, make.plot = TRUE)

print(statistiek)
print(figuur)

# Rapportagegrens gelijk aan 0.5 RG
d <- grondwaterdata %>% 
    filter(parameter == "as", diepte == "ondiep") %>%
  mutate(waarde = ifelse(detectielimiet == 1, 0.5 * waarde, waarde))

statistiek0.5 <- mktrends("17f1", d, rpDL = FALSE, make.plot = FALSE)
figuur0.5 <- mktrends("17f1", d, rpDL = FALSE, dw.plot = FALSE, make.plot = TRUE)

print(statistiek0.5)
print(figuur0.5)

# Rapportagegrens gelijk aan 0
d <- grondwaterdata %>% 
    filter(parameter == "as", diepte == "ondiep") %>%
  mutate(waarde = ifelse(detectielimiet == 1, 0, waarde))

statistiek0 <- mktrends("17f1", d, rpDL = FALSE, make.plot = FALSE)
figuur0 <- mktrends("17f1", d, rpDL = FALSE, dw.plot = FALSE, make.plot = TRUE)

print(statistiek0)
print(figuur0)


## ----fig.width = 4, fig.height = 4, fig.align = "center"-----------------

# Data selecteren uit grondwaterlichaam
d <- grondwaterdata %>% 
    distinct(putfilter, .keep_all = TRUE)


pts <- st_as_sf(d, coords = c("xcoord", "ycoord"), crs = 28992) %>% select(waarde)

p <- basemap()
p <- p + geom_sf(data = pts) 
p


## ------------------------------------------------------------------------

d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep") 

statistiek <- trendReversal("213f1", d, trim = FALSE, make.plot = FALSE)

knitr::kable(statistiek)


## ----fig.width = 6, fig.height = 3, fig.align = "center"-----------------

d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep") 

statistiek <- trendReversal("213f1", d, trim = FALSE, make.plot = FALSE)
figuur <- trendReversal("213f1", d, dw.plot = FALSE, trim = FALSE, make.plot = TRUE)

print(statistiek)
print(figuur)


## ------------------------------------------------------------------------

d <- grondwaterdata %>% 
    filter(parameter == "no3", diepte == "ondiep")

aantal.reeks <- paste(d$putfilter, d$parameter, sep = " - ") %>% unique %>% length()

resultaat <- trendomkeringAnalyse(d, n.reeks = aantal.reeks)

knitr::kable(resultaat, digits = 2)

## ------------------------------------------------------------------------

resultaat <- data.frame()
for (i in c("no3", "as")) {

    d <- grondwaterdata %>% 
        filter(parameter == i, diepte == "ondiep") 
    aantal.reeks <- paste(d$putfilter, d$parameter, sep = " - ") %>% unique %>% length()
    
    resultaat <- resultaat %>%
        bind_rows(data.frame(param = i, trendomkeringAnalyse(d, n.reeks = aantal.reeks),
                             stringsAsFactors = FALSE))
}

knitr::kable(resultaat, digits = 2)

