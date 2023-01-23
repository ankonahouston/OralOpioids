-   <a href="#description" id="toc-description">Description</a>
-   <a href="#functions" id="toc-functions">Functions</a>
    -   <a href="#loadingupdating-the-health-canada-dataset"
        id="toc-loadingupdating-the-health-canada-dataset">Loading/updating the
        Health Canada dataset</a>
    -   <a
        href="#get-the-morphine-equivalent-dose-med-from-health-canada-by-using-the-din"
        id="toc-get-the-morphine-equivalent-dose-med-from-health-canada-by-using-the-din">Get
        the Morphine Equivalent Dose (MED) from Health Canada by using the
        DIN</a>
    -   <a href="#get-the-brand-name-from-health-canada-by-using-the-din"
        id="toc-get-the-brand-name-from-health-canada-by-using-the-din">Get the
        Brand name from Health Canada by using the DIN</a>
    -   <a href="#get-the-opioid-content-from-health-canada-by-using-the-din"
        id="toc-get-the-opioid-content-from-health-canada-by-using-the-din">Get
        the Opioid content from Health Canada by using the DIN</a>
    -   <a
        href="#get-the-maximum-number-of-unitsmillilitres-of-oral-opioids-allowed-per-day-assuming-a-daily-limit-of-50-medday-for-a-din-from-the-healthcanada-opioid-table-by-using-the-din"
        id="toc-get-the-maximum-number-of-unitsmillilitres-of-oral-opioids-allowed-per-day-assuming-a-daily-limit-of-50-medday-for-a-din-from-the-healthcanada-opioid-table-by-using-the-din">Get
        the Maximum number of units/millilitres of oral opioids allowed per day
        assuming a daily limit of 50 MED/day for a DIN from the HealthCanada
        Opioid Table by using the DIN</a>
    -   <a
        href="#get-the-maximum-number-of-unitsmillilitres-of-oral-opioids-allowed-per-day-assuming-a-daily-limit-of-90-medday-for-a-din-from-the-healthcanada-opioid-table-by-using-the-din"
        id="toc-get-the-maximum-number-of-unitsmillilitres-of-oral-opioids-allowed-per-day-assuming-a-daily-limit-of-90-medday-for-a-din-from-the-healthcanada-opioid-table-by-using-the-din">Get
        the Maximum number of units/millilitres of oral opioids allowed per day
        assuming a daily limit of 90 MED/day for a DIN from the HealthCanada
        Opioid Table by using the DIN</a>

------------------------------------------------------------------------

## Description

This package provides details of all oral opioids authorized for sale by
Health Canada. Please note no way should output from this package be a
substitute for medical advise. All medications should only be consumed
on prescription from a licensed healthcare provider.

------------------------------------------------------------------------

## Functions

### Loading/updating the Health Canada dataset

    library(OralOpioids)
    HealthCanada_Opioid_Table <- load_HealthCanada_Opioid_Table()

    ## Your HealthCanada_Opioid_Table is outdated. Do you want to download  the latest data from Health Canada? (y/n) 
    ## 
    ## 1: Y
    ## 2: N
    ## 
    ## Selection: Y
    ## trying URL 'https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles.zip'
    ## Content type 'application/zip' length unknown
    ## downloaded 1.4 MB
    ## 
    ## trying URL 'https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_ap.zip'
    ## Content type 'application/zip' length unknown
    ## downloaded 407 KB
    ## 
    ## trying URL 'https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_dr.zip'
    ## Content type 'application/zip' length unknown
    ## downloaded 317 KB
    ## 
    ## trying URL 'https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/allfiles_ia.zip'
    ## Content type 'application/zip' length unknown
    ## downloaded 4.9 MB
    ## 
    ## The HealthCanada_Opioid_Table was successfully updated to 2023-01-03.
    ## DISCLAIMER: Not a substitute for medical advise. Please note that the output generated by the package should not be substituted for clinical advise and any medication ## should be only consumed at the advise of a licensed healthcare provider.
    ## 
    ## Source url of the data: https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database/what-data-extract-drug-product-database.html
    ## Source url used for dosing: https://www.cihi.ca/sites/default/files/document/opioid-prescribing-canada-trends-en-web.pdf

    head(HealthCanada_Opioid_Table)

    ##       DIN MED_per_dispensing_unit Base1 Base2 Base3             Opioid      Route   Form                       Brand No_tabs/ml assuming 50 MED limit per day
    ## 1 2517175  Couldn't be calculated     1     N       BUPRENORPHINE 2 MG SUBLINGUAL TABLET TARO-BUPRENORPHINE/NALOXONE                   Couldn't be calculated
    ## 2 2517175  Couldn't be calculated     1     N          NALOXONE 0.5 MG SUBLINGUAL TABLET TARO-BUPRENORPHINE/NALOXONE                   Couldn't be calculated
    ## 3 2517183  Couldn't be calculated     1     N       BUPRENORPHINE 8 MG SUBLINGUAL TABLET TARO-BUPRENORPHINE/NALOXONE                   Couldn't be calculated
    ## 4 2517183  Couldn't be calculated     1     N            NALOXONE 2 MG SUBLINGUAL TABLET TARO-BUPRENORPHINE/NALOXONE                   Couldn't be calculated
    ## 5 2518759                       5     1     N           TRAMADOL 50 MG       ORAL TABLET           JAMP TRAMADOL HCL                                       10
    ## 6  763527                     1.2     1     N             CODEINE 8 MG       ORAL TABLET            ORADRINE TABLETS                                       42
    ##   No_tabs/ml assuming 90 MED limit per day Maximum No_tabs/ml assuming 50 MED limit for 7 days Maximum No_tabs/ml assuming 50 MED limit for 14 days
    ## 1                   Couldn't be calculated                              Couldn't be calculated                               Couldn't be calculated
    ## 2                   Couldn't be calculated                              Couldn't be calculated                               Couldn't be calculated
    ## 3                   Couldn't be calculated                              Couldn't be calculated                               Couldn't be calculated
    ## 4                   Couldn't be calculated                              Couldn't be calculated                               Couldn't be calculated
    ## 5                                       18                                                  70                                                  140
    ## 6                                       75                                                 294                                                  588
    ##   Maximum No_tabs/ml assuming 50 MED limit for 30 days              Status_1 last_updated
    ## 1                               Couldn't be calculated              APPROVED   2023-01-03
    ## 2                               Couldn't be calculated              APPROVED   2023-01-03
    ## 3                               Couldn't be calculated              APPROVED   2023-01-03
    ## 4                               Couldn't be calculated              APPROVED   2023-01-03
    ## 5                                                  300              APPROVED   2023-01-03
    ## 6                                                 1260 CANCELLED POST MARKET   2023-01-03

### Get the Morphine Equivalent Dose (MED) from Health Canada by using the DIN

    MED(108316,HealthCanada_Opioid_Table)

    ## [1] 2.25

### Get the Brand name from Health Canada by using the DIN

    Brand(108316, HealthCanada_Opioid_Table)

    ## [1] "282 MEP TAB"

### Get the Opioid content from Health Canada by using the DIN

    Opioid(108316,HealthCanada_Opioid_Table)

    ## [1] "CODEINE 15 MG"

### Get the Maximum number of units/millilitres of oral opioids allowed per day assuming a daily limit of 50 MED/day for a DIN from the HealthCanada Opioid Table by using the DIN

    MED_50(108316,HealthCanada_Opioid_Table)

    [1] 22

### Get the Maximum number of units/millilitres of oral opioids allowed per day assuming a daily limit of 90 MED/day for a DIN from the HealthCanada Opioid Table by using the DIN

    MED_90(108316,HealthCanada_Opioid_Table)

    [1] 40