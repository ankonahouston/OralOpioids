
#' CanadianBigData: TODO description, e.g., Obtain the lastes information on Morphine administration provided by HealthCanada
#' @docType package
#' @name CanadianBigData
  NULL
  #> NULL
#' @param filelocation The directory on your system where you want the dataset to be downloaded.
#' If "", filelocation will be set to the download path within the CanadianBigData
#' package installation directory.
#'
#' @return The dataset showing all details of all oral opioids available for sale as authorized by HealthCanada

#' @import ggplot2 tidyr readr purrr forcats magrittr plyr readr readxl reshape2 stringr openxlsx rvest xml2 dplyr
#'
#' @examples Canadianload_Big_Data_form()


#' @export
##TODEL: I would change the default path since it will be different when you run it from RStudio and R, Windows or Mac
load_Big_Data_form <- function(filelocation = ""){

  if (filelocation == ""){
    filelocation <- paste0(system.file(package = "CanadianBigData"),"/download")
  }

  ## if the filelocation directory does not exist, create it
  if (!dir.exists(filelocation)){
    dir.create(filelocation, recursive = TRUE)
  }

  ## 1) Get HealthCanada data date and compare with Big_Data_form date
  ## Get HealthCanada data date ------------------------
  content <- xml2::read_html("https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database/what-data-extract-drug-product-database.html")
  tables <- content %>%
        rvest::html_table(fill = TRUE)
  second_table <- tables[[2]]
  second_table_date <- second_table$`Last Updated`[[1]]

  second_table_date <- as.Date(as.character(second_table_date))

  ## Get Big_Data_form date ---------------------
  ## List all files in filelocation
  downloaded_files <- list.files(filelocation)
  ## check if Big_Data_form file is among files
  Big_Data_form_file_indices <- grep("Big_Data_form",downloaded_files)
  if (length(Big_Data_form_file_indices) > 0) {
    list_of_dates <- NULL
    for (i in Big_Data_form_file_indices){
      list_of_dates <- c(list_of_dates,substr(downloaded_files[i],1,10))
      ##TODO test if all files are older
      as.Date(as.character(substr(downloaded_files[i],1,10))) > second_table_date
    }
  }


  temp <- tempfile()
  download.file("https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles.zip",temp)
  schedule <- read.csv(unz(temp, "schedule.txt"), header= F)
  drug <- read.csv(unz(temp, "drug.txt"), header= F)
  ther <- read.csv(unz(temp, "ther.txt"), header= F)
  status <- read.csv(unz(temp, "status.txt"), header= F)
  ingred <- read.csv(unz(temp, "ingred.txt"), header= F)
  route <- read.csv(unz(temp, "route.txt"), header= F)
  form <- read.csv(unz(temp, "form.txt"), header= F)

  unlink(temp)


      download.file("https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_ap.zip",temp)
      schedule_ap <- read.csv(unz(temp, "schedule_ap.txt"), header= F)
      drug_ap <- read.csv(unz(temp, "drug_ap.txt"), header= F)
      ther_ap <- read.csv(unz(temp, "ther_ap.txt"), header= F)
      status_ap <- read.csv(unz(temp, "status_ap.txt"), header= F)
      ingred_ap <- read.csv(unz(temp, "ingred_ap.txt"), header= F)
      route_ap <- read.csv(unz(temp, "route_ap.txt"), header= F)
      form_ap <- read.csv(unz(temp, "form_ap.txt"), header= F)


      unlink(temp)


      download.file("https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_dr.zip",temp)
      schedule_dr <- read.csv(unz(temp, "schedule_dr.txt"), header= F)
      drug_dr <- read.csv(unz(temp, "drug_dr.txt"), header= F)
      ther_dr <- read.csv(unz(temp, "ther_dr.txt"), header= F)
      status_dr <- read.csv(unz(temp, "status_dr.txt"), header= F)
      ingred_dr <- read.csv(unz(temp, "ingred_dr.txt"), header= F)
      route_dr <- read.csv(unz(temp, "route_dr.txt"), header= F)
      form_dr <- read.csv(unz(temp, "form_dr.txt"), header= F)


      unlink(temp)


      download.file("https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_ia.zip",temp)
      schedule_ia <- read.csv(unz(temp, "schedule_ia.txt"), header= F)
      drug_ia <- read.csv(unz(temp, "drug_ia.txt"), header= F)
      ther_ia <- read.csv(unz(temp, "ther_ia.txt"), header= F)
      status_ia <- read.csv(unz(temp, "status_ia.txt"), header= F)
      ingred_ia <- read.csv(unz(temp, "ingred_ia.txt"), header= F)
      route_ia <- read.csv(unz(temp, "route_ia.txt"), header= F)
      form_ia <- read.csv(unz(temp, "form_ia.txt"), header= F)

      unlink(temp)

      schedule <- rbind (schedule, schedule_ap, schedule_dr, schedule_ia)
      rm (schedule_ap, schedule_dr, schedule_ia)


      drug <- rbind (drug, drug_ap, drug_dr, drug_ia)
      rm (drug_ap, drug_dr, drug_ia)

      drug <- drug[,c(1,4,5)]
      colnames(drug) <- c("ID","DIN","Brand")


      ther <- rbind (ther, ther_ap, ther_dr, ther_ia)

      rm (ther_ap, ther_dr, ther_ia)

      ther <- ther [c(1:3)]

      colnames (ther) <- c("ID","ATC_Number","ATC")



      status <- rbind (status,status_ap, status_dr, status_ia)

      rm (status_ap, status_dr, status_ia)

      status <- status [ ,c(1,3,4)]
      colnames (status) <- c("ID","Status","Date")


      ingred <- rbind (ingred,ingred_ap,ingred_dr, ingred_ia)

      rm (ingred_ap,ingred_dr, ingred_ia)

      ingred <- ingred [ ,c(1,2,3,5,6,8,9,10)]
      colnames (ingred) <- c("ID","Drug_Code","Ingred","Dose","Value","Base1","Base2","Base3")


      form <- rbind (form,form_ap,form_dr,form_ia)

      rm (form_ap,form_dr,form_ia)

      form <- form [ c(1,3)]
      colnames(form) <- c("ID", "Form")



      route <- rbind (route, route_ap,route_dr,route_ia)
      route <- route [, c(1,3)]
      colnames (route) <- c("ID","Route")

      rm (route_ap,route_dr,route_ia)


      schedule <- schedule [,-3]

      colnames (schedule) <- c("ID","Schedule")

      drug <- merge (drug,schedule, by= "ID")

      drug$Opioid_Query <- ifelse((grepl("Narcotic", drug$Schedule)), "1", "0")

      Opioid_Query <- subset (drug, Opioid_Query==1)

      Opioid_Query <- Opioid_Query[,c(2,3,4)]
      Opioid_Query <- unique (Opioid_Query)

      Opioid_Query$DIN <- as.numeric(Opioid_Query$DIN)

      drug$keep <- ifelse (((grepl("Narcotic", drug$Schedule))), "1", "0")

      Opioids <- subset (drug,keep=="1")
      Opioids <- merge (Opioids,route, by= "ID")

      Unique_route <- as.data.frame(unique(Opioids$Route))

      #usethis::use_package ("dplyr")
      Opioids <-  Opioids[Opioids$Route %in% c("ORAL","TRANSDERMAL","RECTAL","BUCCAL", "SUBLINGUAL"),]

      Opioids_1 <- Opioids [,c(1:3)]

      Opioids_1 <- unique(Opioids_1)
      Opioids_1 <- merge (Opioids_1,ingred, by= "ID")

      Unique_Ingred <- as.data.frame(unique(Opioids_1$Ingred))

      Opioids_1<- subset (Opioids_1,Ingred!="CHLORZOXAZONE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PHENOBARBITAL")
      Opioids_1 <- subset (Opioids_1,Ingred!="METHOCARBAMOL")
      Opioids_1$Caffeine <- ifelse (grepl("CAFFEINE", Opioids_1$Ingred), "1", "0")
      Opioids_1 <- subset (Opioids_1,Ingred!="BUTALBITAL")
      Opioids_1 <- subset (Opioids_1,Ingred!="CAFFEINE")
      Opioids_1 <- subset (Opioids_1,Ingred!="ACETYLSALICYLIC ACID")
      Opioids_1 <- subset (Opioids_1,Ingred!="ACETAMINOPHEN")
      Opioids_1 <- subset (Opioids_1,Ingred!="DROPERIDOL")
      Opioids_1 <- subset (Opioids_1,Ingred!="CAMPHOR")
      Opioids_1 <- subset (Opioids_1,Ingred!="BENZOIC ACID")
      Opioids_1 <- subset (Opioids_1,Ingred!="ANTIPYRINE")
      Opioids_1 <- subset (Opioids_1,Ingred!="SODIUM SALICYLATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="MAGNESIUM HYDROXIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="ALUMINUM HYDROXIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="DIPHENYLPYRALINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PHENYLPROPANOLAMINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="BELLADONNA")
      Opioids_1 <- subset (Opioids_1,Ingred!="DOXYLAMINE SUCCINATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="HYOSCYAMINE SULFATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PECTIN")
      Opioids_1 <- subset (Opioids_1,Ingred!="ATTAPULGITE (ACTIVATED)")
      Opioids_1 <- subset (Opioids_1,Ingred!="ATROPINE SULFATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="SCOPOLAMINE HYDROBROMIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="IBUPROFEN")
      Opioids_1 <- subset (Opioids_1, Caffeine=="0")
      Opioids_1 <- subset (Opioids_1,Ingred!="SQUILL")
      Opioids_1 <- subset (Opioids_1,Ingred!="POTASSIUM CITRATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="IPECAC")
      Opioids_1 <- subset (Opioids_1,Ingred!="TERPIN HYDRATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="P-HYDROXYEPHEDRINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="EMETINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="DIPHENOXYLATE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PSEUDOEPHEDRINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="TRIPROLIDINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="GUAIFENESIN")
      Opioids_1 <- subset (Opioids_1,Ingred!="MEPROBAMATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="AMMONIUM CHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="BROMODIPHENHYDRAMINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="DIPHENHYDRAMINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="POTASSIUM GUAIACOL SULPHONATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="CHLORPHENIRAMINE MALEATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="ETAFEDRIN HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="SODIUM CITRATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PHENIRAMINE MALEATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PYRILAMINE MALEATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="NABILONE")
      Opioids_1 <- subset (Opioids_1,Ingred!="ALCOHOL ANHYDROUS")
      Opioids_1 <- subset (Opioids_1,Ingred!="COCILLANA")
      Opioids_1 <- subset (Opioids_1,Ingred!="WILD LETTUCE")
      Opioids_1 <- subset (Opioids_1,Ingred!="SENEGA")
      Opioids_1 <- subset (Opioids_1,Ingred!="EUPHORBIA")
      Opioids_1 <- subset (Opioids_1,Ingred!="PHENYLEPHRINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="MENTHOL")
      Opioids_1 <- subset (Opioids_1,Ingred!="TANNIC ACID")
      Opioids_1 <- subset (Opioids_1,Ingred!="GUMWEED")
      Opioids_1 <- subset (Opioids_1,Ingred!="AMMONIUM ACETATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="EPHEDRINE AS RESIN COMPLEX")
      Opioids_1 <- subset (Opioids_1,Ingred!="CHLORPHENIRAMINE")
      Opioids_1 <- subset (Opioids_1,Ingred!="GUAIACOL CARBONATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PHENYLTOLOXAMINE")
      Opioids_1 <- subset (Opioids_1,Ingred!="BROMPHENIRAMINE MALEATE")
      Opioids_1 <- subset (Opioids_1,Ingred!="PROMETHAZINE HYDROCHLORIDE")
      Opioids_1 <- subset (Opioids_1,Ingred!="KAOLIN")
      Opioids_1 <- subset (Opioids_1,Ingred!="DRONABINOL")

      #str (status)
      status$Date <- as.Date(status$Date,"%d-%b-%Y")



      status <- status%>%
        dplyr::arrange(ID,desc(Date))%>%
        dplyr::group_by(ID)%>%
        dplyr::mutate(ranks=order(ID))

      #status <- status %>% dplyr::mutate(ranks=row_number())


      #use_package ("reshape2")

      status <- reshape2::dcast (status,ID~ ranks, value.var= "Status")

      x <- paste ("Status",1:(ncol(status)-1), sep= "_")

      colnames (status) <- c("ID",x)

      Opioids_1 <- merge (Opioids_1,status, by= "ID")

      #colnames (Opioids_1)

      Opioids_1 <- merge (Opioids_1, form, by= "ID")
      Opioids_1 <- merge (Opioids_1,route, by= "ID")

      Opioids_1$DIN <- as.numeric(Opioids_1$DIN)

      #usethis::use_package ("stringr")

      Opioids_1$Opioid_1 <- stringr::word(Opioids_1$Ingred,1)
      Opioids_1$Opioid <- paste (Opioids_1$Opioid_1, Opioids_1$Dose, Opioids_1$Value)

      #colnames (Opioids_1)

      Opioids_1$Brand <- as.character(Opioids_1$Brand)
      Opioids_1$Form <- as.character(Opioids_1$Form)
      Opioids_1$Route <- as.character(Opioids_1$Route)

      #str(Opioids_1)
      Opioids_2 <- as.data.frame (cbind(Opioids_1$DIN, (Opioids_1$Brand), Opioids_1$Base1,Opioids_1$Base2, Opioids_1$Base3, Opioids_1$Opioid_1,
                                        Opioids_1$Form, Opioids_1$Route, Opioids_1$Opioid))

      colnames(Opioids_2) <- c("DIN", "Brand","Base1","Base2","Base3","Opioid_1","Form","Route","Opioid")

      Unique_Form <- as.data.frame(unique(Opioids_2$Form))
      Opioids_2$Form <- as.character(Opioids_2$Form)
      Opioids_2$Form_1 <- ifelse (((grepl("TABLET", Opioids_2$Form))|(grepl("CAPSULE", Opioids_2$Form))), "CAPTAB", Opioids_2$Form)
      Opioids_2$Form_1 <- ifelse ((grepl("SYRUP|TINCTURE|ELIXIR|DROPS|SOLUTION|LIQUID|SUSPENSION",Opioids_2$Form)),"LIQUID", Opioids_2$Form_1)

      files <- lapply(list.files(system.file('extdata', package = 'CanadianBigData'), full.names = TRUE), read.csv)

      #use_package("readr")
      #suppressMessages(Big_Opioids_Previous <- readr::read_csv("C:/Users/u243494/Downloads/Big_Opioids_New_Keep.csv",show_col_types = F))
      #colnames (Big_Opioids_Previous)
      Big_1 <- as.data.frame(files)

      Big_1 <- Big_1[,c(2,8)]

      Big_1 <- unique (Big_1)



      Big_1 <- Big_1%>%
        dplyr::arrange(DIN)%>%
        dplyr::group_by(DIN)%>%
        dplyr::mutate(ranks=order(DIN))


      Big_2 <- reshape2::dcast (Big_1,DIN~ ranks, value.var= "MED_per_dispensing_unit")


      Big_2$MED_per_dispensing_unit <- ifelse (Big_2$`1`=="Couldn't be calculated","Couldn't be calculated",Big_2$`1`)


      Big_2$MED_per_dispensing_unit <- ifelse (is.na (Big_2$MED_per_dispensing_unit),Big_2$`1`,Big_2$MED_per_dispensing_unit)


      Big_2 <- Big_2 [,c(1,4)]

      Big_1 <- Big_2


      #colnames (Opioids_2)
      Complete <- merge (Big_1,Opioids_2,by= c("DIN"), all.y= T)


      Complete_a <- subset (Complete, !is.na(Complete$MED_per_dispensing_unit))

      #write.csv(Complete_a,"R:/Medical_Consultants_Counts/Opioids/Big_Opioids_New1.csv")

      Incomplete <- subset (Complete, is.na(Complete$MED_per_dispensing_unit))
      Complete1 <- subset (Complete, !is.na(Complete$MED_per_dispensing_unit))

      #colnames (Complete1)
      Complete1 <- Complete1 [,c(2,4:7,9,10,11)]

      Complete2 <- unique(Complete1)

      #colnames (Incomplete)
      Incomplete1 <- Incomplete[,c(1,3,4:7,9,10,11)]

      Incomplete1 <- merge (Incomplete1, Complete2, by= c("Base1","Base2","Base3","Opioid","Route","Form_1"), all.x= T)

      Incomplete_Done <- subset (Incomplete1,!is.na(Incomplete1$MED_per_dispensing_unit))
      Incomplete2 <- subset (Incomplete1,is.na(Incomplete1$MED_per_dispensing_unit))
      Incomplete2$Base1 <- ifelse ((is.na(Incomplete2$Base1)),1,Incomplete2$Base1)

      #colnames (Incomplete2)

      Incomplete3 <- Incomplete2 [ ,c(1:7)]
      Incomplete3 <- unique(Incomplete3)

      Incomplete3$Opioid_1 <- stringr::word(Incomplete3$Opioid,1)
      Incomplete3$Opioid_2 <- stringr::word(Incomplete3$Opioid,2)

      Incomplete3$MED_per_dispensing_unit <- 0


      #str(Incomplete3)

      Incomplete3$Opioid_2 <- as.numeric(Incomplete3$Opioid_2)

      Incomplete3$MED_per_dispensing_unit<- ifelse (Incomplete3$Opioid_1 %in% c("BUPRENORPHINE","NALOXONE"),
                                                    "Couldn't be calculated",Incomplete3$MED_per_dispensing_unit)

      #str(Incomplete3)

      Incomplete3$Base1 <- as.numeric(Incomplete3$Base1)

      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route %in% c("BUCCAL","SUBLINGUAL") & Incomplete3$Opioid_1=="FENTANYL"),
                                                     ((Incomplete3$Opioid_2*0.13)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)



      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route== "ORAL" & Incomplete3$Opioid_1=="CODEINE"),
                                                     ((Incomplete3$Opioid_2*0.15)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route== "ORAL" & Incomplete3$Opioid_1=="HYDROCODONE"),
                                                     ((Incomplete3$Opioid_2*1.5)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route== "ORAL" & Incomplete3$Opioid_1=="OXYCODONE"),
                                                     ((Incomplete3$Opioid_2*1.5)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route== "ORAL" & Incomplete3$Opioid_1=="HYDROMORPHONE"),
                                                     ((Incomplete3$Opioid_2*5)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route== "RECTAL" & Incomplete3$Opioid_1=="MORPHINE"),
                                                     ((Incomplete3$Opioid_2*3)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route== "ORAL" & Incomplete3$Opioid_1=="MORPHINE"),
                                                     ((Incomplete3$Opioid_2*1)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- ifelse ((Incomplete3$Route== "ORAL" & Incomplete3$Opioid_1=="OXYMORPHONE"),
                                                     ((Incomplete3$Opioid_2*3)/Incomplete3$Base1),Incomplete3$MED_per_dispensing_unit)
      #str(Incomplete3)

      Incomplete3$MED_per_dispensing_unit <- as.numeric(Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- round (Incomplete3$MED_per_dispensing_unit,1)

      Incomplete3$MED_per_dispensing_unit <- as.character(Incomplete3$MED_per_dispensing_unit)

      Incomplete3$MED_per_dispensing_unit <- ifelse (Incomplete3$MED_per_dispensing_unit== "0","Couldn't be calculated",Incomplete3$MED_per_dispensing_unit)

      #colnames (Incomplete2)

      Incomplete2 <- Incomplete2 [,c(1:10)]
      names (Incomplete2)[9] <- "Opioid_1"

      Incomplete2 <- Incomplete2[,c(-10)]

      Incomplete_Done <- Incomplete_Done[ ,c(1:8,10)]

      Incomplete4 <- merge (Incomplete2, Incomplete3,by= c("Base1","Base2","Base3","Opioid","Route","Form_1","DIN"), all.x= T)

      Incomplete4 <- unique (Incomplete4)

      #colnames (Incomplete4)

      Incomplete4 <- Incomplete4 [ ,c(1:8,12)]

      names(Incomplete4)[7] <- "DIN"

      #colnames (Incomplete_Done)
      #colnames(Incomplete4)

      Total_Incomplete <- rbind (Incomplete4, Incomplete_Done)

      Total_Incomplete$Base1 <- ifelse ((is.na(Total_Incomplete$Base1)),1,Total_Incomplete$Base1)

      Total_Incomplete$MED_per_dispensing_unit <- ifelse (Total_Incomplete$Opioid=="OXYCODONE 5 MG" & Total_Incomplete$Form_1=="CAPTAB" ,7.5,Total_Incomplete$MED_per_dispensing_unit )
      Total_Incomplete$MED_per_dispensing_unit <- ifelse (Total_Incomplete$Opioid=="OXYCODONE 10 MG" & Total_Incomplete$Form_1=="CAPTAB" ,15,Total_Incomplete$MED_per_dispensing_unit )
      Total_Incomplete$MED_per_dispensing_unit <- ifelse (Total_Incomplete$Opioid=="OXYCODONE 20 MG" & Total_Incomplete$Form_1=="CAPTAB" ,30,Total_Incomplete$MED_per_dispensing_unit )
      Total_Incomplete$MED_per_dispensing_unit <- ifelse (Total_Incomplete$Opioid=="OXYCODONE 40 MG" & Total_Incomplete$Form_1=="CAPTAB" ,60,Total_Incomplete$MED_per_dispensing_unit )


      Total_Incomplete$MED_per_dispensing_unit <- as.numeric (Total_Incomplete$MED_per_dispensing_unit)

      Total_Incomplete$MED_50_day <- 50/(Total_Incomplete$MED_per_dispensing_unit)
      Total_Incomplete$MED_50_day <- round(Total_Incomplete$MED_50_day, digits=0)
      Total_Incomplete$MED_90_day <- 90/(Total_Incomplete$MED_per_dispensing_unit)
      Total_Incomplete$MED_90_day <- round(Total_Incomplete$MED_90_day, digits=0)
      Total_Incomplete$Threshold_7days <- 7*Total_Incomplete$MED_50_day
      Total_Incomplete$Threshold_14days <- 14*Total_Incomplete$MED_50_day
      Total_Incomplete$Threshold_30days <- 30*Total_Incomplete$MED_50_day

      #colnames (Total_Incomplete)


      names (Total_Incomplete) [10] <- "No_tabs/ml assuming 50 MED limit per day"
      names (Total_Incomplete) [11] <- "No_tabs/ml assuming 90 MED limit per day"
      names (Total_Incomplete) [12] <- "Maximum No_tabs/ml assuming 50 MED limit for 7 days"
      names (Total_Incomplete) [13] <- "Maximum No_tabs/ml assuming 50 MED limit for 14 days"
      names (Total_Incomplete) [14] <- "Maximum No_tabs/ml assuming 50 MED limit for 30 days"

      drug$DIN <- as.numeric(drug$DIN)
      status <- merge (status, drug, by= "ID")



      #colnames(status)

      status1 <- as.data.frame(cbind(status$ID,status$Status_1,status$Status_2,status$Status_3,status$DIN))

      status1 <- as.data.frame(status1)

      colnames(status1) <- c("ID","Status_1","Status_2","Status_3","DIN")


      #colnames(status1)

      Total_Incomplete <- merge (Total_Incomplete,status1, by= "DIN")

      Complete_a <- merge (Complete_a,status1, by= "DIN")
      Complete_a <- unique (Complete_a)
      #colnames (Complete_a)
      #colnames (Total_Incomplete)

      Complete_a$MED_per_dispensing_unit <- as.numeric(Complete_a$MED_per_dispensing_unit)

      Complete_a$MED_50_day <- 50/(Complete_a$MED_per_dispensing_unit)
      Complete_a$MED_50_day <- round(Complete_a$MED_50_day, digits=0)
      Complete_a$MED_90_day <- 90/(Complete_a$MED_per_dispensing_unit)
      Complete_a$MED_90_day <- round(Complete_a$MED_90_day, digits=0)
      Complete_a$Threshold_7days <- 7*Complete_a$MED_50_day
      Complete_a$Threshold_14days <- 14*Complete_a$MED_50_day
      Complete_a$Threshold_30days <- 30*Complete_a$MED_50_day

      #colnames (Complete_a)


      names (Complete_a) [16] <- "No_tabs/ml assuming 50 MED limit per day"
      names (Complete_a) [17] <- "No_tabs/ml assuming 90 MED limit per day"
      names (Complete_a) [18] <- "Maximum No_tabs/ml assuming 50 MED limit for 7 days"
      names (Complete_a) [19] <- "Maximum No_tabs/ml assuming 50 MED limit for 14 days"
      names (Complete_a) [20] <- "Maximum No_tabs/ml assuming 50 MED limit for 30 days"


      Complete_a <- Complete_a[,c(1:6,9:20)]

      Big_Data <- rbind (Total_Incomplete,Complete_a)



      Big_Data <- unique(Big_Data)

      Big_Data <- Big_Data%>%
        dplyr::arrange(DIN)%>%
        dplyr::group_by(DIN)%>%
        dplyr::mutate(ranks=order(DIN))

      Previous_DIN <- as.data.frame(Big_1[,2])
      Previous_DIN <- unique(Previous_DIN)
      colnames(Previous_DIN) <- "DIN"

      Previous_DIN$Month <- "Previous"

      Big_Data_DIN <- Big_Data[,1]
      Big_Data_DIN <- unique(Big_Data_DIN)

      Big_Data_DIN$Month <- "Recent"

      a <- merge (Big_Data_DIN, Previous_DIN,by= "DIN", all.x= T, all.y= T)



      #Big_Data <- Big_Data[,c(1:11)]

      DIN_count <- Big_Data %>%
        dplyr::group_by(DIN,MED_per_dispensing_unit) %>%
        dplyr::tally()


      Big_Data <- merge(Big_Data, DIN_count, by= c("DIN","MED_per_dispensing_unit"))


      #Big_Data <- Big_Data[,c(1:3,7,8,11,14:18)]


      Big_Data1 <- as.data.frame(cbind(Big_Data$DIN, as.character(Big_Data$Opioid))) #I want to keep just the DIN and name of the Opioid

      Big_Data1 <- unique(Big_Data1)

      colnames(Big_Data1) <- c("DIN","Opioid")

      Big_Data1 <- Big_Data1 %>%
        dplyr::arrange (DIN,Opioid)%>%
        dplyr::group_by(DIN)%>%
        dplyr::mutate(rank=order(DIN))


      Big_Data2 <- reshape2::dcast(Big_Data1, DIN ~ rank, value.var="Opioid")

      colnames(Big_Data2) <- c("DIN","Opioid1","Opioid2")

      Big_Data2$Opioid <- ifelse(is.na(Big_Data2$Opioid2),paste(Big_Data2$Opioid1),
                                 paste (Big_Data2$Opioid1, "/",Big_Data2$Opioid2))



      Big_Data1 <- as.data.frame(cbind(Big_Data$DIN, as.character(Big_Data$Route))) #I want to keep just the DIN and route

      Big_Data1 <- unique(Big_Data1)

      colnames(Big_Data1) <- c("DIN","Route")

      Big_Data1 <- Big_Data1 %>%
        dplyr::arrange (DIN,Route)%>%
        dplyr::group_by(DIN)%>%
        dplyr::mutate(rank=order(DIN))



      Big_Data3 <- reshape2::dcast(Big_Data1, DIN ~ rank, value.var="Route")

      colnames(Big_Data3) <- c("DIN","Route1","Route2")

      Big_Data3$Route <- ifelse(is.na(Big_Data3$Route2),paste(Big_Data3$Route1),
                                paste (Big_Data3$Route1, "/",Big_Data3$Route2))


      Big_Data2 <- Big_Data2[,c(1,ncol(Big_Data2))]
      Big_Data3 <- Big_Data3[,c(1,ncol(Big_Data3))]

      Big_Data <- merge(Big_Data, Big_Data2, by= "DIN")
      Big_Data <- merge(Big_Data, Big_Data3, by= "DIN")


      Big_Data$MED_per_dispensing_unit <- ifelse (is.na(Big_Data$MED_per_dispensing_unit),"Couldn't be calculated",Big_Data$MED_per_dispensing_unit)
      Big_Data$`No_tabs/ml assuming 50 MED limit per day` <- ifelse (is.na(Big_Data$`No_tabs/ml assuming 50 MED limit per day`),"Couldn't be calculated",Big_Data$`No_tabs/ml assuming 50 MED limit per day`)
      Big_Data$`No_tabs/ml assuming 90 MED limit per day` <- ifelse (is.na(Big_Data$`No_tabs/ml assuming 90 MED limit per day`),"Couldn't be calculated",Big_Data$`No_tabs/ml assuming 90 MED limit per day`)
      Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 7 days` <- ifelse (is.na(Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 7 days`),"Couldn't be calculated",Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 7 days`)
      Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 14 days` <- ifelse (is.na(Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 14 days`),"Couldn't be calculated",Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 14 days`)
      Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 30 days` <- ifelse (is.na(Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 30 days`),"Couldn't be calculated",Big_Data$`Maximum No_tabs/ml assuming 50 MED limit for 30 days`)



      Previous_Data <- colnames(Big_1)


      Big_Data <- Big_Data[,c(1:18)]


      #suppressWarnings(use_package ("plyr"))

      Big_Data <- plyr::rename(Big_Data, c("Opioid.x" = "Opioid"))
      Big_Data <- plyr::rename(Big_Data, c("Route.x" = "Route"))


      Big_Data <- unique(Big_Data)
      Big_Data_form <- merge(Big_Data,form,by= "ID")

openxlsx::write.xlsx(Big_Data_form[,c(2:8,19,10:16)],paste0(filelocation,"/",second_table_date,"_Big_Data_form.xlsx"))
return(Big_Data_form)
}

Big_Data_form <- load_Big_Data_form()

#' @param Drug_ID A numeric value for the DIN.Exclude all zeros in front
#'@param Big_Data_form Name of the dataset that is output from previous function
#' @return MED: Morphine Equivalent Dose

#' @import dplyr
#'
#' @examples MED(108316)


#' @export

MED <- function(Drug_ID,Big_Data_form){
  if (Drug_ID %in% Big_Data_form$DIN)
  {a <- subset(Big_Data_form,DIN== Drug_ID)
  return(a$MED_per_dispensing_unit)}
  else return(0)
}

MED(108316, Big_Data_form)

Opioid <- function(Drug_ID,Big_Data_form){
  if (Drug_ID %in% Big_Data_form$DIN)
  {a <- subset(Big_Data_form,DIN== Drug_ID)
  return(a$Opioid)}
  else print("Could not be calculated")
}

Opioid(108316, Big_Data_form)

Brand <- function(Drug_ID,Big_Data_form){
  if (Drug_ID %in% Big_Data_form$DIN)
  {a <- subset(Big_Data_form,DIN== Drug_ID)
  return(a$Brand)}
  else print("Could not be calculated")
}

Brand(108316, Big_Data_form)

MED_50 <- function(Drug_ID,Big_Data_form){
  if (Drug_ID %in% Big_Data_form$DIN)
  {a <- subset(Big_Data_form,DIN== Drug_ID)
  return(a$`No_tabs/ml assuming 50 MED limit per day`)}
  else print("Could not be calculated")
}

MED_50(108316, Big_Data_form)

MED_90 <- function(Drug_ID,Big_Data_form){
  if (Drug_ID %in% Big_Data_form$DIN)
  {a <- subset(Big_Data_form,DIN== Drug_ID)
  return(a$`No_tabs/ml assuming 90 MED limit per day`)}
  else print("Could not be calculated")
}

MED_90(108316, Big_Data_form)




