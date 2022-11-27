## ----setup, echo=FALSE--------------------------------------------------------------------------------------------------------------------
library(knitr)
library(rmdformats)

## Global options
options(max.print="75")
opts_chunk$set(echo=TRUE,
	             cache=FALSE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)


## ----cargaDatasets------------------------------------------------------------------------------------------------------------------------
library(readxl)
GEOdat <- read_excel("GEOdatasets_updatedNov2022.xls")
GEOdat <- GEOdat[-27,]


## ----seleccionDatos-----------------------------------------------------------------------------------------------------------------------
set.seed (234567)
seleccion<- sample(1:nrow(GEOdat),1)
GEOSerie<- GEOdat[seleccion,2]
GEODataset <- GEOdat[seleccion,1]


## -----------------------------------------------------------------------------------------------------------------------------------------
library(GEOquery)
myGSE <- getGEO(GEOSerie, AnnotGPL=TRUE)
myGDS <- getGEO (GEODataset, AnnotGPL=TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------
Meta(myGDS)
Columns(myGDS)
Table(myGDS)


## -----------------------------------------------------------------------------------------------------------------------------------------
infoDF <- data.frame(matrix(rep(NA, 2*length(Meta(myGDS))), ncol=2))
for (i in 1:length(Meta(myGDS))){
  infoDF[i,1] =names(Meta(myGDS))[i]
  infoDF[i,2] =Meta(myGDS)[i]
}
colnames(infoDF) = c("Campo", "Descripción")
library(dplyr)
infoDF %>% kableExtra::kable() %>% kableExtra::kable_styling()


## -----------------------------------------------------------------------------------------------------------------------------------------
myeSet <- GDS2eSet(myGDS)
pData(myeSet) %>% kableExtra::kable() %>% kableExtra::kable_styling()


## -----------------------------------------------------------------------------------------------------------------------------------------
eSet <- myGSE[[1]]
class(eSet)


## -----------------------------------------------------------------------------------------------------------------------------------------
pData(eSet)[,c(1,8)]


## -----------------------------------------------------------------------------------------------------------------------------------------
shortName<- paste(substr(pData(eSet)$title,3,5),
                  c(rep("untr", 4), rep("treat",4)),
                    substr(pData(eSet)$title,1,1),
                    substr(rownames(pData(eSet)),6,8), sep="_")
colores<- c(rep("red", 4), rep("blue", 4))
pData(eSet)<- data.frame(shortName, colores, pData(eSet)[, c(1,8)])
colnames(exprs(eSet)) <- rownames(pData(eSet) ) <- pData(eSet)$shortName


## -----------------------------------------------------------------------------------------------------------------------------------------
boxplot(exprs(eSet), las=2, col=pData(eSet)$colores, 
#        names=pData(eSet)$shortName, 
        cex.axis=0.8, main="Distribucion de los valores de expresión")


## -----------------------------------------------------------------------------------------------------------------------------------------
boxplot(log(exprs(eSet)), las=2, col=pData(eSet)$colores, 
#        names=pData(eSet)$shortName, 
        cex.axis=0.8, main="Distribucion de los valores de log(expresión)")


## -----------------------------------------------------------------------------------------------------------------------------------------
resPC <- prcomp(t(exprs(eSet)))
summary(resPC)


## -----------------------------------------------------------------------------------------------------------------------------------------
if (!(require(ggfortify))) install.packages("ggfortify", dep=TRUE)
library(ggfortify)
autoplot(resPC, data=t(exprs(eSet)), label=TRUE, label.size=3, colour=factor(pData(eSet)$colores))


## -----------------------------------------------------------------------------------------------------------------------------------------
distMat <- dist(t(log(exprs(eSet))))
hc <-hclust(distMat) 
plot(hclust(distMat))


## ---- echo=FALSE--------------------------------------------------------------------------------------------------------------------------
require(readxl)
library(magrittr)
GEOdatasets_updatedNov2022 <- read_excel("GEOdatasets_updatedNov2022.xls")
GEOdatasets_updatedNov2022 %>% kableExtra::kable() %>% kableExtra::kable_styling()


## ---- file="UOC-MU-AD0-2022-23-S1-PEC1-Solucion.R", eval=FALSE----------------------------------------------------------------------------


