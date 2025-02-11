install.packages("tidyverse")
library(tidyverse)

Benchmarking2015 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2015_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2016 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2016_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2017 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2017_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2018 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2018_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2019 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2019_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2020 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2020_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2021 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2021_Building_Energy_Benchmarking.csv", header = TRUE)
Benchmarking2022 = read.csv("C:/Users/kalin/Downloads/Capstone_Building_data/2022_Building_Energy_Benchmarking.csv", header = TRUE)

Bench2015 = subset(Benchmarking2015, select = -c(OtherFuelUse.kBtu.,Comment,Outlier,DefaultData) )
Bench2016 = subset(Benchmarking2016, select = -c(Comments,Outlier,DefaultData) )
Bench2017 = subset(Benchmarking2017, select = -c(Outlier,DefaultData) )
Bench2018 = subset(Benchmarking2018, select = -c(Compliance.Issue, EPA.Building.Sub.Type.Name) )
Bench2019 = subset(Benchmarking2019, select = -c(ComplianceIssue) )
Bench2020 = subset(Benchmarking2020, select = -c(ComplianceIssue) )
Bench2021 = subset(Benchmarking2021, select = -c(ComplianceIssue) )
Bench2022 = subset(Benchmarking2022, select = -c(ComplianceIssue) )

Bench2018['TaxParcelIdentificationNumber'] <- NA
Bench2018['NumberofBuildings'] <- NA
Bench2018['YearsENERGYSTARCertified'] <- NA
Bench2018['ListOfAllPropertyUseTypes'] <- NA
Bench2019['YearsENERGYSTARCertified'] <- NA
Bench2019['NumberofBuildings'] <- NA
Bench2019['ListOfAllPropertyUseTypes'] <- NA
Bench2020['YearsENERGYSTARCertified'] <- NA
Bench2020['ListOfAllPropertyUseTypes'] <- NA
Bench2021['YearsENERGYSTARCertified'] <- NA
Bench2021['ListOfAllPropertyUseTypes'] <- NA
Bench2022['YearsENERGYSTARCertified'] <- NA
Bench2022['ListOfAllPropertyUseTypes'] <- NA

names(Bench2018)[1] <- "OSEBuildingID"
names(Bench2018)[2] <- "DataYear"
names(Bench2018)[3] <- "PropertyName"
names(Bench2018)[4] <- "BuildingType"
names(Bench2018)[8] <- "ZipCode"
names(Bench2018)[12] <- "CouncilDistrictCode"
names(Bench2018)[13] <- "YearBuilt"
names(Bench2018)[14] <- "NumberofFloors"
names(Bench2018)[15] <- "PropertyGFATotal"
names(Bench2018)[17] <- "PropertyGFAParking"
names(Bench2018)[18] <- "PrimaryPropertyType"
names(Bench2018)[19] <- "ENERGYSTARScore"
names(Bench2018)[26] <- "LargestPropertyUseType"
names(Bench2018)[27] <- "LargestPropertyUseTypeGFA"
names(Bench2018)[28] <- "SecondLargestPropertyUseType"
names(Bench2018)[29] <- "SecondLargestPropertyUseTypeGFA"
names(Bench2018)[30] <- "ThirdLargestPropertyUseType"
names(Bench2018)[31] <- "ThirdLargestPropertyUseTypeGFA"
names(Bench2018)[35] <- "ComplianceStatus"
names(Bench2018)[38] <- "GHGEmissionsIntensity"
names(Bench2018)[39] <- "TotalGHGEmissions"

names(Bench2019)[3] <- "PropertyName"
names(Bench2020)[3] <- "PropertyName"
names(Bench2020)[27] <- "PrimaryPropertyType"
names(Bench2021)[3] <- "PropertyName"
names(Bench2021)[27] <- "PrimaryPropertyType"
names(Bench2022)[3] <- "PropertyName"
names(Bench2022)[27] <- "PrimaryPropertyType"

col_order <- c("OSEBuildingID", "DataYear", "BuildingType", "PrimaryPropertyType",
               "PropertyName", "Address", "City", "State",
               "ZipCode", "TaxParcelIdentificationNumber", "CouncilDistrictCode", "Neighborhood",
               "Latitude", "Longitude", "YearBuilt", "NumberofBuildings",
               "NumberofFloors", "PropertyGFATotal", "PropertyGFAParking", "PropertyGFABuilding.s.",
               "ListOfAllPropertyUseTypes", "LargestPropertyUseType", "LargestPropertyUseTypeGFA", "SecondLargestPropertyUseType",
               "SecondLargestPropertyUseTypeGFA", "ThirdLargestPropertyUseType", "ThirdLargestPropertyUseTypeGFA", "YearsENERGYSTARCertified",
               "ENERGYSTARScore", "SiteEUI.kBtu.sf.", "SiteEUIWN.kBtu.sf.", "SourceEUI.kBtu.sf.",
               "SourceEUIWN.kBtu.sf.", "SiteEnergyUse.kBtu.", "SiteEnergyUseWN.kBtu.", "SteamUse.kBtu.",
               "Electricity.kWh.", "Electricity.kBtu.", "NaturalGas.therms.", "NaturalGas.kBtu.",
               "ComplianceStatus", "TotalGHGEmissions", "GHGEmissionsIntensity")

Bench2016 <- Bench2016[,col_order]
Bench2017 <- Bench2017[,col_order]
Bench2018 <- Bench2018[,col_order]
Bench2019 <- Bench2019[,col_order]
Bench2020 <- Bench2020[,col_order]
Bench2021 <- Bench2021[,col_order]
Bench2022 <- Bench2022[,col_order]

