#Here is the mock R code
library(readr)

#Access prescription database. I am using DIN but NDC could also be used. We need the number of tablets or ml of opioid containing liquid dispensed 
colnames (Database) <- c("Patient_ID","prescription_date","paid_quantity",
                           "DIN","Prescription_no")

Database <- unique(Database) # Making sure my data is clean and that there are no repeats

library (OralOpioids)

b <- load_HealthCanada_Opioid_Table() #you can use load_FDA_Opioid_Table if using US data

# Merge the information output by load_HealthCanada_Opioid_Table or load_Opioid_Table(country="usa")

Opioid_Prescriptions <- merge(Database,b,by.x="DIN",by.y= "Drug_ID")

Opioid_Prescriptions$prescription_date <- (as.character(as.Date (Opioid_Prescriptions$prescription_date)))

#If I want to identify the MED for say August 2024. 
Opioid_Prescriptions <- subset (Opioid_Prescriptions,prescription_date >as.Date("2024-08-01") & prescription_date >as.Date("2024-08-31"))

# Cleaning the data
Opioid_Prescriptions$MED_per_dispensing_unit <- as.numeric(Opioid_Prescriptions$MED_per_dispensing_unit)
Opioid_Prescriptions$paid_quantity <- as.numeric (Opioid_Prescriptions$paid_quantity)
Opioid_Prescriptions<- subset (Opioid_Prescriptions, paid_quantity >0)

# Calculate the total MED per prescription
Opioid_Prescriptions$Total_MED_Prescription <- Opioid_Prescriptions$paid_quantity* Opioid_Prescriptions$MED_per_dispending_unit

# Calculate the MED per person
Monthly_MED <- aggregate(Opioid_Prescriptions$Total_MED_Prescription,list(Opioid_Prescriptions$Patient_ID),FUN= "sum")

colnames(Monthly_MED) <- c("Patient_ID","Total_MED_Prescription")

#Calculate the daily MED per person
Monthly_MED$Daily_MED <- Monthly_MED$Total_MED_Prescription/31
