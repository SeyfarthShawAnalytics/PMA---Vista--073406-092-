#rstudioapi::restartSession()
rm(list =ls())

# Housekeeping ------------------------------------------------------------
par(mar=c(1,1,1,1))           #1 plot per plot window
options(scipen = 999)         #Eliminate auto rounding
options(width = 80)           #Code width
options(max.print = 200)      #Limited for quicker run time
options(pkgType = "source")   #
memory.size(max=TRUE)         #Devote maximum RAM amount to Rs
Sys.setenv(TZ = "GMT")        #Setting GMT as default
TZ <- "GMT"                   #enabling future use of global timezone call
set.seed(1776)                #


# ** CM & Version ------------------------------------------------------------------------------------------
Client.Matter.Info<-"PMA - Vista (073406-092)"
Version.Number<-"Quick Analysis v2"
Client.Matter.Info.AND.Version.Number<-paste(Client.Matter.Info, "-", Version.Number)


stop("Nothing has been changed from v1 as of yet!")


#** Working Directory --------------------------------------------------------------------------------------
WD<-file.path("A:/Cases_Github", gsub("\\s|[[:punct:]]", "-", Client.Matter.Info))



# Packages Enablement -----------------------------------------------------
require(checkpoint)
setSnapshot("2020-12-10")
#checkpoint("2020-04-30", project=WD, forceProject = TRUE) ##NOTE: must be YYYY-MM-DD

pkgs<-rev(c("dplyr",        ## for data handling
            "stringr",      ## for strings
            "stringdist",   ## for string distance/fuzzy match
            "tidyr",        ## for data structures
            "lubridate",    ## for date handling
            "data.table",   ## for data structures and reading/writing
            "ggplot2",      ## for internal visualizations
            "plotly",       ## for interactive visualizations
            "ggrepel",      ## for labeling within ggplot2
            "readxl",       ## for ingesting xlsx files
            "openxlsx",     ## for exporting objects to a finalized excel template
            "pdftools",     ## for PDF extraction
            #"caret",       ## for machine learning modeling
            "shiny",        ###### for web app development
            "svDialogs",    ## for prompting user for mistype inputs
            "pryr",         ## for faster object size print outs & memory management!
            "devtools",     ## for certain functions lacking in base
            "httr",         ### Direct access to websites/URLs
            "tictoc",       ## for tracking calculation times
            "beepr"))      ## for placing audio chimes to signal completed calculations


#as.data.frame(lapply(pkgs, install.packages, dependencies = TRUE))
#install.packages(pkgs, dependencies=TRUE)
as.data.frame(lapply(pkgs, require, character.only=TRUE)) #Reversed for 1st package priority
sessionInfo()




# * --------------------------------------------------------------------------------------------------------
# File Checks ----------------------------------------------------------------------------------------------

# ** Custom Functions --------------------------------------------------------

#source_url("https://raw.githubusercontent.com/PGundy/Useful-Custom-R-Functions/master/Custom%20Functions.R")

WD.Code<-file.path(WD, "Code")
dir.create(WD.Code, showWarnings = F)

if ("Custom Functions from Master Branch of PGundy's Github.R" %in% list.files(WD.Code) ){
  source(file.path(WD.Code, "Custom Functions from Master Branch of PGundy's Github.R"))
}else{
  
  download.file("https://raw.githubusercontent.com/PGundy/Useful-Custom-R-Functions/master/Custom%20Functions.R",
                file.path(WD.Code, "Custom Functions from Master Branch of PGundy's Github.R"))
  
  source(file.path(WD.Code, "Custom Functions from Master Branch of PGundy's Github.R"))
}





# ** File Name ---------------------------------------------------------------------------------------------
File.Name<-paste0(Client.Matter.Info.AND.Version.Number, ".R")                     ## File.Name used in backup sys
File.Name2<-str_remove(File.Name, "\\.R$")                                   ## File.Name used in code versions
WC.FILE <- (function() { attr(body(sys.function()), "srcfile")})()$filename  ## Pulls the file that is open

##Below checks to see if the current WD & file are the same as the file being backed up.
### The comparison is done on all non-punct characters to avoid Regex issues
ifelse( str_remove(WC.FILE, ".*\\/")==File.Name,
        #File names match!
        "Versioning code will work normally",
        #Fix the definition of File.Name OR change the name of the active file!
        stop("Please update the variable 'File.Name' to correctly match the FILE variable"))





# * --------------------------------------------------------------------------------------------------------















# Directory Builder ----------------------------------------------------------------------------------------
# * Directory Creation ----------------------------------------------------------------------------#


# ** dir: Version ---------------------------------------------------------------------------------#
# Analysis Version
WD.Analysis<-file.path(WD, "Analysis")
WD.Analysis.Version<-file.path(WD, "Analysis", Version.Number)
dir.create(file.path(WD.Analysis.Version), recursive = T, showWarnings = F)




# ** dir: Code ------------------------------------------------------------------------------------#
#Code
#Any and all versions of code saved here
WD.Code<-file.path(WD, "Code")
dir.create(WD.Code, showWarnings = F)

# Code file within the versioned folder
file.copy(file.path(WD.Code, File.Name), 
          file.path(WD.Analysis.Version, paste(Version.Number, "of", File.Name)),
          overwrite = T)





# ** dir: Raw Data -------------------------------------------------------------------------------#
#Raw Data
#Data from Client
WD.Raw.Data<-(file.path(WD, "Raw Data"))
dir.create(WD.Raw.Data, showWarnings = F)


WD.Raw.Data.pr<-(file.path(WD.Raw.Data, "Payroll"))
dir.create(WD.Raw.Data.pr, showWarnings = F)

WD.Raw.Data.tc<-(file.path(WD.Raw.Data, "Timecard"))
dir.create(WD.Raw.Data.tc, showWarnings = F)

WD.Raw.Data.cen<-(file.path(WD.Raw.Data, "Census"))
dir.create(WD.Raw.Data.cen, showWarnings = F)


dir.create(file.path(WD.Raw.Data, "Zips"), showWarnings = F)
dir.create(file.path(WD.Raw.Data, "To Be Sorted"), showWarnings = F)


# ** dir: Compiled -------------------------------------------------------------------------------#
#Compiled Data
# For use across ALL versions
WD.Compiled.Data.AGNOSTIC<-(file.path(WD.Analysis, "~ Agnostic Compiled Data"))
dir.create(WD.Compiled.Data.AGNOSTIC, recursive = T, showWarnings = F)


#Data prepared for analysis
WD.Compiled.Data<-(file.path(WD.Analysis.Version, "Compiled Data"))
dir.create(WD.Compiled.Data, recursive = T, showWarnings = F)

WD.Compiled.Data.pr<-(file.path(WD.Analysis.Version, "Compiled Data", "Payroll"))
dir.create(WD.Compiled.Data.pr, recursive = T, showWarnings = F)

WD.Compiled.Data.tc<-(file.path(WD.Analysis.Version, "Compiled Data", "Timecard"))
dir.create(WD.Compiled.Data.tc, recursive = T, showWarnings = F)

WD.Compiled.Data.cen<-(file.path(WD.Analysis.Version, "Compiled Data", "Census"))
dir.create(WD.Compiled.Data.cen, recursive = T, showWarnings = F)




# ** dir: Prod Data -----------------------------------------------------------------------------#

#Production Data
#Data to be produced
WD.Production.Data<-(file.path(WD.Analysis.Version, "Production Data"))
dir.create(WD.Production.Data, recursive = T, showWarnings = F)




# **Other Dirs -----------------------------------------------------------------------------------#

#Emails
#Any emails containing data, analysis requests, case info, ect.
dir.create(file.path(WD, "Emails"), showWarnings = F)


#Legal Docs
#Any legal documentation saved during case
dir.create(file.path(WD, "Legal Docs"), showWarnings = F)


#Visualization Data
#Data to be visualized in tableau
WD.Visualization.Data<-(file.path(WD.Analysis.Version, "Visualization Data"))
dir.create(WD.Visualization.Data, recursive = T, showWarnings = F)


#For tableau PDFs
dir.create(file.path(WD.Visualization.Data, "Exports"), recursive = T, showWarnings = F)


#Tables and Summaries
#Data containing exposure calculations
WD.Tables.and.Summaries.Data<-(file.path(WD.Analysis.Version, "Tables and Summaries Data"))
dir.create(WD.Tables.and.Summaries.Data,recursive = T, showWarnings = F)


#QC Data
#Data specifically intended for QC purposes
WD.QC.Data<-(file.path(WD.Analysis.Version, "QC Data"))
dir.create(WD.QC.Data,recursive = T, showWarnings = F)


# * --------------------------------------------------------------------------------------------------------




# ******************* -------------------------------------------------------------------------------------











# Loading Data --------------------------------------------------------------------------------------------------
list.files(WD.Raw.Data, recursive = TRUE)

# Cleaned Data --------------------------------------------------------------------------------------------------


DF<-read_excel(file.path(WD.Raw.Data,
                         "Initial Data/Vista - All Procedure List (4.18.2019) names removed.xlsx"))

names(DF)<-colNamesCleaner(DF)

DF<-DF %>% rename(Provider.or.Facility=Provider.Facility)


glimpse(DF)


DF_Patient_Agg<-DF %>% 
  arrange(Date.of.Service.From) %>% 
  group_by(Patient) %>% 
  summarize(Has.Cosmetic.Claim=str_detect(list.collapse(Type.of.Claim), "Cosmetic"),
            
            COUNTD_Provider.Office=n_distinct(Provider.Office, na.rm=TRUE),
            LIST_Provider.Office=list.collapse(na.omit(Provider.Office), Clean = TRUE),
            LIST_C_Provider.Office=list.collapse(na.omit(Provider.Office), Sort = TRUE, Clean = TRUE),
            
            COUNTD_Procedure.Codes=n_distinct(Procedure.Code),
            COUNTD_Provider_Procedure_Code=n_distinct(paste(na.omit(Provider.Office), Procedure.Code)),
            List_Procedure.Codes=list.collapse(na.omit(Procedure.Code)),
            
            COUNTD_Provider.Category.of.Surgery=n_distinct(paste(Provider.Office, Category.of.Surgery)),
            COUNTD_Category.of.Surgery=n_distinct(Category.of.Surgery),
            List_Category.of.Surgery=list.collapse(Category.of.Surgery, Clean = TRUE, Sort = TRUE) ) %>% 
  mutate(Mid.Priority=(str_detect(LIST_C_Provider.Office, "Dahiya") & str_detect(LIST_C_Provider.Office, "Patel") |
                         str_detect(LIST_C_Provider.Office, "Dahiya") & str_detect(LIST_C_Provider.Office, "Gomez Garcia") |
                         str_detect(LIST_C_Provider.Office, "Patel") & str_detect(LIST_C_Provider.Office, "Gomez Garcia")),
         
         High.Priority=(str_detect(LIST_C_Provider.Office, "Dahiya") &
                          str_detect(LIST_C_Provider.Office, "Patel") & 
                          str_detect(LIST_C_Provider.Office, "Gomez Garcia")),
         
         Evaluate.These=(Mid.Priority+High.Priority)>0 )

View(DF_Patient_Agg)

DF_Patient_Agg %>%   
  group_by(LIST_C_Provider.Office) %>% 
  summarize(COUNT_Patient=n_distinct(Patient),
            COUNT_Patient_ProcedureCodes=sum(COUNTD_Provider_Procedure_Code),
            COUNT_Patient_Provider_Procedure_Code=sum(COUNTD_Provider_Procedure_Code),
            LIST_Pateint=list.collapse(Patient, Sort = TRUE, Clean = TRUE),
            
            sum(COUNTD_Category.of.Surgery) ) %>% 
  mutate(Row.Of.Interest=str_detect(LIST_C_Provider.Office, "Dahiya|Patel|Gomez Garcia"),
         Mid.Priority=(str_detect(LIST_C_Provider.Office, "Dahiya") & str_detect(LIST_C_Provider.Office, "Patel") |
                         str_detect(LIST_C_Provider.Office, "Dahiya") & str_detect(LIST_C_Provider.Office, "Gomez Garcia") |
                         str_detect(LIST_C_Provider.Office, "Patel") & str_detect(LIST_C_Provider.Office, "Gomez Garcia")),
         
         High.Priority=(str_detect(LIST_C_Provider.Office, "Dahiya") &
                          str_detect(LIST_C_Provider.Office, "Patel") & 
                          str_detect(LIST_C_Provider.Office, "Gomez Garcia")),
         
         Evaluate.These=(Mid.Priority+High.Priority)>0 ) %>% 
  View(., "LIST_C_Provider.Office")



stop()



fwrite.DF.to.csv.as.char(DF,
                         file.path(WD.Compiled.Data,
                                   paste(Client.Matter.Info.AND.Version.Number, 
                                         "DF Exported.csv")))


fwrite.DF.to.csv.as.char(DF_Patient_Agg,
                         file.path(WD.Compiled.Data,
                                   paste(Client.Matter.Info.AND.Version.Number, 
                                         "DF_Patient_Agg Exported.csv")))






















