home <- read.csv("/Users/escac/OneDrive - Escola Superior de Cinema i Audiovisuals deCatalunya/ing_DataCience/01_tipologia_cicleDeVida/01_PACs/04_PR2/03_REC/data_housing/house_data.csv", header = TRUE)

head(home[,1:6])

sappy(home, function(x), class(x))

house_analytic <- select(habitatges, LotArea, Neighborhood, OverallQual, OverallCond, YearBuilt, TotalBsmtSF, CentralAir, X1stFlrSF, X2ndFlrSF, GrLivArea, FullBath, HalfBath, Fireplaces, GarageArea, PoolArea, EnclosedPorch, OpenPorchSF, SalePrice  )

sapply(house_analytic, function(x) sum(is.na(x)))


house_analytic$X2ndFlrSF<- kNN( house_analytic )$  X2ndFlrSF

boxplot.stats(house_analytic$OverallQual)$out

boxplot.stats(house_analytic$OverallCond)$out

boxplot.stats(house_analytic$SalePrice)$out

boxplot.stats(house_analytic$LotArea)$out


house_analytic.IDOTRR <- house_analytic[house_analytic$Neighborhood =="IDOTRR",]
house_analytic.NAmes <- house_analytic[house_analytic$Neighborhood =="NAmes",]
house_analytic.Giblert <- house_analytic[house_analytic$Neighborhood =="Giblert",]
house_analytic.StoneBr <- house_analytic[house_analytic$Neighborhood =="StoneBr",]
house_analytic.Blmngtn <- house_analytic[house_analytic$Neighborhood =="Blmngtn",]
house_analytic.Blueste <- house_analytic[house_analytic$Neighborhood =="Blueste",]
house_analytic.BrDale <- house_analytic[house_analytic$Neighborhood =="BrDale",]
house_analytic.BrkSide <- house_analytic[house_analytic$Neighborhood =="BrkSide",]
house_analytic.ClearCr <- house_analytic[house_analytic$Neighborhood =="ClearCr",]
house_analytic.CollgCr <- house_analytic[house_analytic$Neighborhood =="CollgCr",]
house_analytic.Crawfor <- house_analytic[house_analytic$Neighborhood =="Crawfor",]
house_analytic.Edwards <- house_analytic[house_analytic$Neighborhood =="Edwards",]
house_analytic.Gibert <- house_analytic[house_analytic$Neighborhood =="Girbert",]
house_analytic.MeadowV <- house_analytic[house_analytic$Neighborhood =="MeadowV",]
house_analytic.Mitchel <- house_analytic[house_analytic$Neighborhood =="Mitchel",]
house_analytic.NoRidge <- house_analytic[house_analytic$Neighborhood =="NoRidge",]
house_analytic.NPkVill <- house_analytic[house_analytic$Neighborhood =="NPkVill",]
house_analytic.NridgHt <- house_analytic[house_analytic$Neighborhood =="NridgHt",]
house_analytic.NWAmes <- house_analytic[house_analytic$Neighborhood =="NWAmes",]
house_analytic.OldTown <- house_analytic[house_analytic$Neighborhood =="OldTown",]
house_analytic.Sawyer <- house_analytic[house_analytic$Neighborhood =="Sawyer",]
house_analytic.SawyerW <- house_analytic[house_analytic$Neighborhood =="SawyerW",]
house_analytic.Somerst <- house_analytic[house_analytic$Neighborhood =="Somerst",] house_analytic.StoneBr <- house_analytic[house_analytic$Neighborhood =="StoneBr",]
house_analytic.SWISU <- house_analytic[house_analytic$Neighborhood =="SWISU",]
house_analytic.Timber <- house_analytic[house_analytic$Neighborhood =="Timber",]
house_analytic.Veenker <- house_analytic[house_analytic$Neighborhood =="Veenker",]

house_analytic.CentralAir_Y <- house_analytic[house_analytic$CentralAir =="Y",]
house_analytic.CentralAir_N <- house_analytic[house_analytic$CentralAir =="N",]

shapiro.test(house_analytic$PoolArea)
shapiro.test(house_analytic$LotArea)
shapiro.test(house_analytic$OverallQual)
shapiro.test(house_analytic$OverallCond)
shapiro.test(house_analytic$YearBuilt)
shapiro.test(house_analytic$TotalBsmtSF)
shapiro.test(house_analytic$X1stFlrSF)
shapiro.test(house_analytic$X2ndFlrSF)
shapiro.test(house_analytic$GrLivArea)
shapiro.test(house_analytic$FullBath)
shapiro.test(house_analytic$HalfBath)
shapiro.test(house_analytic$Fireplaces)
shapiro.test(house_analytic$GarageArea)
shapiro.test(house_analytic$EnclosedPorch)
shapiro.test(house_analytic$OpenPorchSF)
shapiro.test(house_analytic$SalePrice)

+cor(house_analytic$OverallQual,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$YearBuilt,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$TotalBsmtSF,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$X1stFlrSF,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$X2ndFlrSF,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$GrLivArea,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$FullBath,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$HalfBath,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$Fireplaces,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$GarageArea,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$PoolArea,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$EnclosedPorch,house_analytic$SalePrice, method="pearson" )

cor(house_analytic$OpenPorchSF,house_analytic$SalePrice, method="pearson" )

house_analytic.NoFPlace <- house_analytic[house_analytic$Fireplace ==0,]
cor(house_analytic.NoFPlace$LotArea, house_analytic.NoFPlace$SalePrice, method="pearson" )

cor(house_analytic$LotArea,house_analytic$SalePrice, method="pearson" )

house_analytic.NoFBath <- house_analytic[house_analytic$FullBath ==0,]
cor(house_analytic.NoFBath$LotArea, house_analytic.NoFBath$SalePrice, method="pearson" )

