###########################################################
# AqMaD inlees functionaliteiten voor RShiny applicatie   #
#                                                         #
# Auteurs: Marc Weeber                                    #
#          Lilith Kramer                                  #
#                                                         #
#                                                         #
# Datum : 2019-04-23                                      #
# Bedrijf: Deltares                                       #
# Licentie: GNU General Public License                    #
#                                                         #           
# Contact : Gerben van Geest                              #
# Email : gerben.vangeest@deltares.nl                     #
#                                                         #
########################################################### 

c0To0 <- function(x){
  #Change output character(0) or integer(0) to a workable output
  if(length(x) == 0){
    x = 0
  }
  return(x)
}

inlezenExcelOfCsv <- function(datafile_path){
  #Inlezen van Excel of csv 
  #Benodigd voor Ecobase, Ecobase QBW en Ecolims
  
  library("tidyverse")
  library("readxl")
  #library(XLConnect)
  library(tools)
  
  if(file_ext(datafile_path) == "csv"){
    dfr <- read.table(datafile_path,stringsAsFactors = FALSE, sep = ";", header = FALSE, quote = '"')
    log = "Status: Bestand ingelezen als csv-bestand."
  }else if(file_ext(datafile_path) == "xls" | file_ext(datafile_path) =="xlsx"){
    #wb <- loadWorkbook(datafile_path)
    #dfr <- readWorksheet(wb,sheet = getSheets(wb)[1], header = FALSE)
    dfr <- read_excel(datafile_path, sheet = 1, guess_max = 999999)
    ??read_excel
    log = "Status: Bestand ingelezen als Excel-bestand."
  }else{
    dfr <- 0
    log = paste("Bestand extensie is verkeerd: '",file_ext(datafile_path), "'. Verwacht wordt CSV of Excel.",sep ="")
  }
  return(list(dfr,log))
}

inlezenCsvOfTxt <- function(datafile_path){
  #Inlezen van csv of text 
  #Benodigd voor inlezenDoelsoorten
  
  library("tidyverse")
  library("readxl")
  library(tools)
  
  if(file_ext(datafile_path) == "txt"){
    dfr <- read.table(datafile_path, stringsAsFactors = FALSE, sep = "\t", header = TRUE, quote = '"')
    log = "Status: Bestand ingelezen als txt-bestand."
  }else if(file_ext(datafile_path) == "csv"){
    dfr <- read.table(datafile_path,stringsAsFactors = FALSE, sep = ";", header = TRUE, quote = '"')
    log = "Status: Bestand ingelezen als csv-bestand."
  }else{
    dfr <- 0
    log = paste("Bestand extensie is verkeerd: '", file_ext(datafile_path), "'. Verwacht wordt csv of (tab-gescheiden) txt.", sep ="")
  }
  return(list(dfr,log))
}


inlezenEcobaseQBW <- function(datafile_path, soortgroep, soortgroepnaam){
  # inlezen van Ecobase QBW data naar Matlab voorbeeld Witteveen+Bos
  # datafile_path = paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
  #"Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Ecolims_vb1.xls", sep = "")
  # soortgroep = "Macrofyten"
  # soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
  #
  
  #make log
  fid = c("")
  
  #open de excel file en lees de eerste sheet
  resultaat = inlezenExcelOfCsv(datafile_path)
  
  dfr = data.frame(resultaat[[1]], stringsAsFactors = FALSE)
  fid = c(fid,resultaat[[2]])
  
  # controleer excel file
  columns = dfr[1,]
  dfr = dfr[2:length(dfr[,1]),]
  condition = c0To0(which(columns == "locatie")) == 10 & c0To0(which(columns == "sample")) == 11 & c0To0(which(columns == "jaar")) == 12 &
              c0To0(which(columns == "keyword")) == 13 & c0To0(which(columns == "ext")) == 14 & c0To0(which(columns == "type")) == 15
  
  if(condition){
    colnaam = columns
    #txt = dfr
    #raw = dfr
    
    colextref = 1
    colsmpcode = c0To0(which(columns == "sample"))
    colpartype = 8
    #colparcode = ...
    colparname =  c0To0(which(columns == "keyword"))
    colreftype = c0To0(which(columns == "type"))
    collocname = 2
    colloccode = 7
    collocX = 3
    collocY = 4
    coldatesmp = 5
    coltimeobs = 6
    
    partype = unlist(lapply(dfr[,colpartype, drop = FALSE], as.character))
    upt = unique(partype)

    imf = 0
    for(i in 1:length(soortgroepnaam)){
      if(upt %in% soortgroepnaam[i]){
        if(imf == 0){
          imf = i
        }else{
          fid = c(fid,paste("WAARSCHUWING: meer dan een code voor ", soortgroep,": ",
                            soortgroepnaam[imf]," en ", soortgroepnaam[i], ".", sep = ""))  
        }
      }
    }
    sel = c0To0(which(partype == soortgroepnaam[imf]))
    extref = as.character(dfr[sel,colextref])
    smpcode = as.character(dfr[sel,colsmpcode])
    parname = as.character(dfr[sel,colparname])
    # parcode = as.character(dfr[sel,colparcode])
    reftype = as.character(dfr[sel,colreftype])
    locname = as.character(dfr[sel,collocname])
    loccode = as.character(dfr[sel,colloccode])
    for(i in 1:length(sel)){
      if(is.numeric(dfr[sel[i],colloccode])){
        loccode[i] = as.character(dfr[sel[i],colloccode])
      }
    }
    locX = dfr[sel,collocX]
    locY = dfr[sel,collocY]
    datesmp = dfr[sel,coldatesmp]
    timeobs = dfr[sel,coltimeobs]
    timeobs[is.na(timeobs)] = 0
    
    if(length(colreftype) == 0){
      noreftype = 1
      reftype = c("")
    }else{
      reftype = as.character(dfr[sel,colreftype])
      noreftype = 0
    }
    
    fullsample = list()
    
    usmp = unique(smpcode)
    for(i in 1:length(usmp)){
      fid = c(fid,paste("Status: inlezen monster ",i," van ",length(usmp), "."))
      #Hier wordt een interactieve voortgang overzicht mogelijk gemaakt in het MatLab script
      
      tempsamp = list()
      tempsamp[["smpcode"]] <- usmp[i]
      sel = c0To0(which(smpcode %in% usmp[i]))
      if(length(reftype) == 0){
        tempsamp[["reftype"]] <- "M01"
      }else{
        tempsamp[["reftype"]] <- unique(reftype[sel])
      }
      tempsamp[["extref"]] <- unique(extref[sel])
      tempsamp[["locname"]] <- unique(locname[sel])
      tempsamp[["loccode"]] <- unique(loccode[sel])
      datumtime <- strptime(unique(paste(datesmp[sel],timeobs[sel])), format = "%d-%m-%y%y %H:%M")
      if(TRUE %in% is.na(datumtime)){
        datumtime <- strptime(unique(datesmp[sel]), format = "%d-%m-%y%y")
      }
      tempsamp[["datesmp"]] <- datumtime
      tempsamp[["locX"]] <- as.numeric(unique(locX[sel]))
      tempsamp[["locY"]] <- as.numeric(unique(locY[sel]))
      tempsamp[["speccode"]] = parname[sel]
      tempsamp[["specname"]] = parname[sel]

      fullsample[[i]]<- tempsamp
    }
  }else{
    fullsample = list()   
    noreftype = 0
  }
  return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
}

inlezenEcobase <- function(datafile_path, soortgroep, soortgroepnaam){
  # inlezen van Ecobase data naar Matlab voorbeeld Witteveen+Bos
  # soortgroep = "Macrofyten"
  # soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
  #
  
  #make log
  fid = c("")
  
  #open de excel file en lees de eerste sheet
  resultaat = inlezenExcelOfCsv(datafile_path)
  
  dfr = data.frame(resultaat[[1]], stringsAsFactors = FALSE)
  fid = c(fid,resultaat[[2]])
  
  # controleer excel file
  columns = dfr[1,]
  dfr = dfr[2:length(dfr[,1]),]
  row_header = c0To0(which(columns == 'HEADER GW_MWA BIO '))
  row_data = c0To0(which(dfr[,row_header] == ' //GW_MWA BIO//'))
  
  if(all(c(row_header,row_data)) & length(row_data) > 0){

    #txt = dfr
    #raw = dfr
    
    # colextref = c0To0(which(columns == "ewa.extmcd0"))
    # colsmpcode = c0To0(which(columns == "eco_extwrs"))
    colpartype = c0To0(which(columns == "wrs.eco_groep_ident"))
    colparcode = c0To0(which(columns == "wns.wnsident"))
    # colparname = c0To0(which(columns == "PAR_NAME"))
    colreftype = c0To0(which(columns == "REF_TYPE"))
    #collocname = c0To0(which(columns == "LOC_NAME"))
    colloccode = c0To0(which(columns == "mpnident"))
    coldatesmp = c0To0(which(columns == "ewa.mdat"))
    coltimeobs = c0To0(which(columns == "ewa.mtijd"))

    #Check if all required columns are found in file
    if(0 %in% c(colpartype,colparcode,
                colloccode,coldatesmp,coltimeobs)){
      fid = c(fid,paste("ERROR: er wordt niet aan het benodigde file formaat voldaan. ",
                        "De import is beeindigd.", sep = ""))
      fullsample = list()   
      noreftype = 0
      return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
    } 
    
    
    partype = unlist(lapply(dfr[,colpartype, drop = FALSE], as.character))
    upt = unique(partype)
    
    imf = 0
    for(i in 1:length(soortgroepnaam)){
      if(upt %in% soortgroepnaam[i]){
        if(imf == 0){
          imf = i
        }else{
          fid = c(fid,paste("WAARSCHUWING: meer dan een code voor ", soortgroep, ": ",
                            soortgroepnaam[imf]," en ", soortgroepnaam[i], sep = ""))  
        }
      }
    }
    sel = c0To0(which(partype == soortgroepnaam[imf]))
    parcode = as.character(dfr[sel,colparcode])
    loccode = as.character(dfr[sel,colloccode])
    datesmp = as.character(dfr[sel,coldatesmp])
    timeobs = as.character(dfr[sel,coltimeobs])
    timeobs[is.na(timeobs)] = 0
    
    smpcode = loccode
    for(i in 1:length(sel)){
      if(is.numeric(dfr[sel[i],colloccode])){
        loccode[i] = as.character(dfr[sel[i],colloccode])
      }
      smpcode[i] = paste(loccode[i],"_",datesmp[i],sep = "")
    }
    extref = smpcode
    
    if(colreftype == 0){
      noreftype = 1
      reftype = c()
    }else{
      reftype = as.character(dfr[sel,colreftype])
      noreftype = 0
    }

    fullsample = list()
    
    uref = unique(extref)
    for(i in 1:length(uref)){
      fid = c(fid,paste("Status: inlezen monster ",i," van ",length(uref), "."))
      # Hier wordt een interactieve voortgang overzicht mogelijk gemaakt in het MatLab script
      
      tempsamp = list()
      tempsamp[["extref"]] <- uref[i]
      sel = c0To0(which(extref %in% uref[i]))
      if(length(reftype) == 0){
        tempsamp[["reftype"]] <- "M01"
      }else{
        tempsamp[["reftype"]] <- unique(reftype[sel])
      }
      tempsamp[["smpcode"]] <- unique(smpcode[sel])
      tempsamp[["locname"]] <- ""
      tempsamp[["loccode"]] <- unique(loccode[sel])
      tempsamp[["locX"]] <- -999
      tempsamp[["locY"]] <- -999
      datumtime <- strptime(unique(paste(datesmp[sel],timeobs[sel])), format = "%d-%m-%y%y %H:%M")
      if(TRUE %in% is.na(datumtime)){
        datumtime <- strptime(unique(datesmp[sel]), format = "%d-%m-%y%y")
      }
      tempsamp[["datesmp"]] <- datumtime
      tempsamp[["speccode"]] = parcode[sel]
      tempsamp[["specname"]] = parcode[sel]
      
      fullsample[[i]]<- tempsamp
    }
  }else{
    fullsample = list()   
    noreftype = 0
  }
  return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
}    

inlezenEcolims <- function(datafile_path, soortgroep, soortgroepnaam){
  # inlezen van Ecolims data naar Matlab voorbeeld Witteveen+Bos
  # soortgroep = "Macrofyten"
  # soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
  
  # datafile_path <- paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
  # "Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Ecolims_vb2.xls", sep = "")
  # dfr <- read_excel(datafile_path, sheet = 1)
  
  #make log
  fid = c("")
  
  #open de excel file en lees de eerste sheet
  resultaat = inlezenExcelOfCsv(datafile_path)
  
  dfr = data.frame(resultaat[[1]], stringsAsFactors = FALSE)
  fid = c(fid,resultaat[[2]])
  
  # controleer excel file
  columns = colnames(dfr)
  
  while(length(c0To0(which(columns == "EXT_REF"))) == 0){
    # verwijder redundante headers
    dfr = dfr[2:length(dfr[,1]),]
    if(length(dfr[,1]) == 0){
      fid = c(fid,paste("ERROR: geen header gevonden.", sep =""))
    }else{
      columns = dfr[1,] 
    }
  }
  dfr = dfr[2:length(dfr[,1]),]
  
  if(length(columns) > 0){
    colsmpcode = c0To0(which(columns == "SMP_CODE"))
    colextref = c0To0(which(columns == "EXT_REF"))
    colpartype = c0To0(which(columns == "PAR_TYPE"))
    colparcode = c0To0(which(columns == "PAR_CODE"))
    colparname = c0To0(which(columns == "PAR_NAME"))
    colreftype = c0To0(which(columns == "REF_TYPE"))
    collocname = c0To0(which(columns == "LOC_NAME"))
    colloccode = c0To0(which(columns == "LOC_CODE"))
    coldatesmp = c0To0(which(columns == "DATE_SMP"))
    coltimeobs = c0To0(which(columns == "TIME_OBS"))
    collocX = c0To0(which(columns == "COORD_X_S"))
    collocY = c0To0(which(columns == "COORD_Y_S"))
    
    #Check if all required columns are found in file
    if(0 %in% c(colsmpcode,colextref,colpartype,colparcode,colparname,
                collocname,colloccode,coldatesmp,coltimeobs,collocX,collocY)){
      fid = c(fid,paste("ERROR: er wordt niet aan het benodigde file formaat voldaan. ",
                        "De import is beeindigd.", sep = ""))
      fullsample = list()   
      noreftype = 0
      return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" =noreftype))
    } 
    
    partype = unlist(lapply(dfr[,colpartype, drop = FALSE], as.character))
    upt = unique(partype)
    
    imf = 0
    for(i in 1:length(soortgroepnaam)){
      if(upt %in% soortgroepnaam[i]){
        if(imf == 0){
          imf = i
        }else{
          fid = c(fid,paste("WAARSCHUWING: meer dan een code voor", soortgroep, ": ",
                            soortgroepnaam[imf]," en ", soortgroepnaam[i], ".", sep = ""))  
        }
      }
    }
    sel = c0To0(which(partype %in% soortgroepnaam[imf]))
    smpcode = dfr[sel,colsmpcode]
    for(i in 1:length(smpcode)){
      if(is.numeric(smpcode[i])){
        smpcode[i] = as.character(smpcode[i])
      }
    }
    extref = dfr[sel,colextref]
    for(i in 1:length(extref)){
      if(is.numeric(extref[i])){
        extref[i] = as.character(extref[i])
      }
    }
    parcode = dfr[sel,colparcode]
    for(i in 1:length(parcode)){
      if(is.numeric(parcode[i])){
        parcode[i] = as.character(parcode[i])
      }
    }
    parname = dfr[sel,colparname]
    locname = dfr[sel,collocname]
    loccode = dfr[sel,colloccode]
    for(i in 1:length(loccode)){
      if(is.numeric(loccode[i])){
        loccode[i] = as.character(loccode[i])
      }
    }
    datesmp = dfr[sel,coldatesmp]
    
    timeobs = as.numeric(unlist(dfr[sel,coltimeobs]))
    HH = floor(timeobs/100)
    MM = timeobs - HH*100
    timeobs = paste(as.character(HH),":",as.character(MM),sep = "")
    locX = dfr[sel,collocX]
    locY = dfr[sel,collocY]
    
    if(colreftype == 0){
      fid = c(fid,paste("WAARSCHUWING: Geen referentie type geselecteerd.", sep = ""))  
      reftype = c()
      noreftype = 1
    }else{
      reftype = dfr[sel,colreftype]
      noreftype = 0
    }
    fullsample = list()      
    
    usmp = unique(smpcode)
    for(i in 1:length(usmp)){
      fid = c(fid,paste("Status: inlezen monster ",i," van ",length(usmp), "."))
      #Hier wordt een interactieve voortgang overzicht mogelijk gemaakt in het MatLab script
      
      tempsamp = list()
      tempsamp[["smpcode"]] <- usmp[i]
      sel = c0To0(which(smpcode %in% usmp[i]))
      if(length(reftype) == 0){
        tempsamp[["reftype"]] <- "M01"
      }else{
        tempsamp[["reftype"]] <- unique(reftype[sel])
      }
      myextref = unique(extref[sel])
      if(length(myextref)>1){
        fid = c(fid,paste("WAARSCHUWING: ",as.character(usmp[i])," heeft geen uniek REF_TYPE. ",
                          "Alleen het eerste referentie type wordt gebruikt.", sep = ""))
        myextref = myextref[1]
      }
      tempsamp[["extref"]] <- myextref
      
      mylocname = unique(locname[sel])
      if(length(mylocname)>1){
        fid = c(fid,paste("WAARSCHUWING: ",as.character(usmp[i])," heeft geen unieke LOC_NAME. ",
                          "Alleen de eerste locatienaam wordt gebruikt.", sep = ""))
        mylocname = mylocname[1]
      }
      tempsamp[["locname"]] <- mylocname
      
      myloccode = unique(loccode[sel])
      if(length(myloccode)>1){
        fid = c(fid,paste("WAARSCHUWING: ",as.character(usmp[i])," heeft geen unieke LOC_CODE. ",
                          "Alleen de eerste locatiecode wordt gebruikt.", sep = ""))
        myloccode = myloccode[1]
      }
      tempsamp[["loccode"]] <- myloccode

      mylocX = unique(locX[sel])
      if(length(mylocX)>1){
        fid = c(fid,paste("WAARSCHUWING: ",as.character(usmp[i])," heeft geen unieke LOC_X. ",
                          "Alleen het eerste x-coordinaat wordt gebruikt.", sep = ""))
        mylocX = mylocX[1]
      }
      tempsamp[["locX"]] <- mylocX      
            
      mylocY = unique(locY[sel])
      if(length(mylocY)>1){
        fid = c(fid,paste("WAARSCHUWING: ",as.character(usmp[i])," heeft geen unieke LOC_Y. ",
                          "Alleen het eerste y-coordinaat wordt gebruikt.", sep = ""))
        mylocY = mylocY[1]
      }
      tempsamp[["locY"]] <- mylocY
      
      mydatesmp = unique(datesmp[sel])
      if(length(mydatesmp)>1){
        fid = c(fid,paste("WAARSCHUWING: ",as.character(usmp[i])," heeft geen unieke DATE_SMP. ",
                          "Alleen de eerste datum wordt gebruikt.", sep = ""))
        mydatesmp = mydatesmp[1]
      }
      tempsamp[["datesmp"]] <- mydatesmp
      tempsamp[["speccode"]] = parcode[sel]
      tempsamp[["specname"]] = parname[sel]
      
      fullsample[[i]]<- tempsamp
    }
  }else{
    fullsample = list()   
    noreftype = 0
  }
  return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
}

inlezenDawaco <- function(datafile_path, soortgroep, soortgroepnaam){
  # inlezen van Ecolims data naar Matlab voorbeeld Witteveen+Bos
  # soortgroep = "Macrofyten"
  # soortgroep = "Macrofauna"
  # soortgroepnaam =  c('MAFY','MACFT','MACFY','mafy','macft','macfy','MFYT')
  # datafile_path = "testdata/Dawaco_export.txt"
  # datafile_path = paste("testdata/Turboveg_export.XML", sep = "")
  # datafile_path <- paste("testdata/Dawaco WSRL Diatom alles tm 2018.txt", sep = "") ## test hoe inlezenDawaco omgaat met andere soortgroepen 
  # datafile_path <- paste("testdata/Dawaco WSRL Mafau alles tm 2018.txt", sep = "") ## test hoe inlezenDawaco omgaat met andere soortgroepen 
  # soortgroep = "Macrofauna"
  # soortgroepnaam = c("MACEV", "MFA$", "MFAT", "Macrofauna")
  # soortgroep = "Diatomeeen"
  # soortgroepnaam = c("DIATM", "DIAB", "Diatomeeen")
  # getwd()
  
  library("readr")
  
  #make log
  # fid = c("")
  fid = character(0)
  
  #get the number of lines
  f <- file(datafile_path, open="rb")
  nlines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
    nlines <- nlines + sum(chunk == as.raw(10L))
  }
  fid = c(fid, paste("Dit bestand heeft ", as.character(nlines), " regels.", sep = ""))
  close(f)
  
  #Check rows until "Mp" is found
  Mp = -999
  row = 0
  con <- file(datafile_path,"r")
  while(row < nlines){
    row = row + 1
    data_line <- readLines(con,n=1)
    vector_line = unlist(strsplit(data_line,"\t"))
    if(length(vector_line) > 0){
      if(vector_line[1] == "Mp"){
        Mp = row
      }
    }else{
      if(row > nlines){
        Mp = "stop"
      }
    }
  }
  close(con)  
  
  #open de file
  resultaat <- data.frame(read_tsv(datafile_path, col_names = F ,
                      skip = Mp - 1, col_types = cols(), guess_max = 999999),
                      stringsAsFactors = FALSE, row.names = NULL)
  Nheader = c0To0(which(resultaat[,1] == "Mp"))
  dfr = resultaat
  
  columns = as.character(dfr[Nheader,])
  
  if(length(columns) > 0){
    
    dfr = dfr[(Nheader + 1):length(dfr[,1]),]
    dfr = dfr[rowSums(is.na(dfr))!=ncol(dfr), ]
    
    colsmpcode = c0To0(which(columns == "Mp"))
    colextref = c0To0(which(columns == "MEPAN"))
    colpartype = c0To0(which(columns == "Methode"))
    #colparcode = c0To0(which(columns == "PAR_CODE"))
    colparname = c0To0(which(columns == "Taxon"))
    colreftype = c0To0(which(columns == "REF_TYPE"))
    colloccode1 = c0To0(which(columns == "Mp"))
    #colloccode2 = c0To0(which(columns == "MEPAN"))
    #collocname1 = c0To0(which(columns == "Locatie"))
    #collocname2 = c0To0(which(columns == "Omschrijving"))
    collocX = c0To0(which(columns == "X-coor"))
    collocY = c0To0(which(columns == "Y-coor"))
    coldatesmp = c0To0(which(columns == "Datum"))
    colwaarde = c0To0(which(columns == "Waarde"))
    
    #Check if all required columns are found in file
    if(0 %in% c(colsmpcode,colextref,colpartype,colparname,colloccode1,
                coldatesmp,colwaarde)){
      fid = c(fid, paste("Kolom mist: ",
                         c("Mp","MEPAN","Methode","Taxon","Mp",
                         "Datum","Waarde")[(c(colsmpcode, colextref,colpartype,colparname,colloccode1,
                         coldatesmp,colwaarde) %in% 0)], sep = ""))
      fid = c(fid,paste("ERROR: er wordt niet aan het benodigde file formaat voldaan. ",
                        "De import is beeindigd.", collapse = "", sep = ""))
      fullsample = list()   
      noreftype = 0
      return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
    }  
    
    partype = as.character(dfr[,colpartype])[!(is.na(dfr[,colpartype]))]
    upt = unique(partype)
    imf = 0
    
    if(!(TRUE %in% grepl(paste(soortgroepnaam, collapse = '|'), upt))){
      fid = c(fid, paste("ERROR: de gekozen soortgroepnaam komt niet voor in bestand. De import is beeindigd. Gegeven soortgroepnamen zijn: '",
                         paste(upt,collapse=', '),"'.", sep = ""))
      fullsample = list()   
      noreftype = 0
      return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
    }
    
    #Verkrijg de data met de juiste soortgroepnaam
    for(i in 1:length(soortgroepnaam)){
      if(TRUE %in% grepl(soortgroepnaam[i],upt, fixed = T)){
        if(imf == 0){
          imf = i
        }else{
          fid = c(fid,paste("WAARSCHUWING: meer dan een code voor ", soortgroep,": ",
                            soortgroepnaam[imf]," en ", soortgroepnaam[i], ".", sep = ""))
          imf = c(imf, i)
        }
      }
    }
    if(imf == 0){
      #Geen soortgroepnaam gevonden die matcht
      fid = c(fid,paste("ERROR: de soortgroepnamen in dit bestand zijn niet bekend voor ", soortgroep,". ",
                        "De import is beeindigd.", collapse = "", sep = ""))
      fullsample = list()   
      noreftype = 0
      return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
    }
    
    sel1 = grepl(paste(soortgroepnaam[imf], collapse = "|"),partype) 
    smpcode = paste(dfr[sel1,colsmpcode],"_",dfr[sel1,coldatesmp], sep = "")
    extref = dfr[sel1,colextref]
    #parcode = dfr[sel1,colparcode]
    parname = dfr[sel1,colparname][!(is.na(dfr[sel1,colparname]))]
    locname = paste(dfr[sel1,colsmpcode]) 
    loccode = dfr[sel1,colloccode1]
    waarde = gsub(",",".",dfr[sel1,colwaarde])   #The comma delimiter is replaced for a dot
    if(length(dfr[sel1,collocX]) > 0){
      locX = dfr[sel1,collocX]
    }else{
      locX = as.character(rep(-999,length(dfr[sel1,1])))
    }
    if(length(dfr[sel1,collocY]) > 0){
      locY = dfr[sel1,collocY]
    }else{
      locY = as.character(rep(-999,length(dfr[sel1,1])))
    }
    datesmp = dfr[sel1,coldatesmp]
    if(length(colreftype) == 0){
      fid = c(fid,"WAARSCHUWING: geen referentietype bepaald in de data.")
      reftype = c()
      noreftype = 1
    }else{
      reftype = dfr[sel1,colreftype]
      noreftype = 0
    }
    fullsample = list()      
    usmp = unique(smpcode)
    usmp = usmp[!(usmp == "_")]
    
    for(i in 1:length(usmp)){
      fid = c(fid,paste("Status: inlezen monster ",i," van ",length(usmp), ".", sep = ""))
      #Hier wordt een interactieve voortgang overzicht mogelijk gemaakt in het MatLab script
      
      tempsamp = list()
      tempsamp[["smpcode"]] <- usmp[i]
      sel2 = c0To0(which(smpcode %in% usmp[i]))
      if(length(reftype) == 0){
        tempsamp[["reftype"]] <- "M01"
      }else{
        tempsamp[["reftype"]] <- unique(reftype[sel2])
      }
      
      #Verkrijg het totale waarde per soort (LET OP: Bij macrofyten is dit vaak percentage bedekking)
      tempsamp[["extref"]] <- unique(extref[sel2])
      tempsamp[["locname"]] = unique(locname[sel2])
      tempsamp[["loccode"]] = unique(loccode[sel2])
      tempsamp[["locX"]] = unique(locX[sel2])
      tempsamp[["locY"]] = unique(locY[sel2])
      tempsamp[["datesmp"]] = unique(strptime(datesmp[sel2], format = "%d-%m-%y%y"))
      if(is.na(tempsamp[["datesmp"]])){
        #probeer nog een keer met een ander datum format
        tempsamp[["datesmp"]] = unique(strptime(datesmp[sel2], format = "%d/%m/%y%y"))
      }
      tempsamp[["speccode"]] = parname[sel2]
      tempsamp[["specname"]] = parname[sel2]
      tempsamp[["waarde"]] = waarde[sel2]
      
      fullsample[[i]]<- tempsamp
    }
  }else{
    fullsample = list()   
    noreftype = 0
    fid <- c(fid, paste("ERROR: dit bestand kan niet goed ingelezen worden, controleer het format."))
  }
  return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
}

inlezenTurboveg <-function(datafile_path){
  # inlezen van Ecolims data naar Matlab voorbeeld Witteveen+Bos
  # soortgroep = "Macrofyten"
  # soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
  #
  
  require(XML)
  require(tidyverse)
  
  #make log
  fid = c("")
  
  data <- xmlParse(datafile_path)
  xml_data <- xmlToList(data)

  Plotdata = xml_data[["Plots"]]
  Lookup_tables = xml_data[["Lookup_tables"]]
  Template = xml_data[["Template"]]
  
  i = 0
  fullsample = list() 
  for(cur_plot in Plotdata){
    i = i + 1
    
    tempsamp = list()
    tempsamp[["smpcode"]] <- cur_plot[["header_data"]][["standard_record"]][["releve_nr"]] 
    if("date" %in% names(cur_plot[["header_data"]][["standard_record"]])){
      mydatesmp = strptime(cur_plot[["header_data"]][["standard_record"]][["date"]], format = "%y%y%m%d")
    }else{
      mydatesmp = strptime(19000101, format = "%y%y%m%d")
    }
    tempsamp[["datesmp"]] <- mydatesmp
    tempsamp[["extref"]] <- cur_plot[["header_data"]][["standard_record"]][["releve_nr"]]
    tempsamp[["locname"]] <- ""
    if("km_hok_x" %in% names(cur_plot[["header_data"]][["standard_record"]])){
      mylocX = 1000 * as.numeric(cur_plot[["header_data"]][["standard_record"]][["km_hok_x"]])
    }else{
      mylocX = -999
    }
    if("km_hok_y" %in% names(cur_plot[["header_data"]][["standard_record"]])){
      mylocY = 1000 * as.numeric(cur_plot[["header_data"]][["standard_record"]][["km_hok_y"]])
    }else{
      mylocY = -999
    }
    tempsamp[["locX"]]<- mylocX
    tempsamp[["locY"]]<- mylocY
    tempsamp[["loccode"]] <- paste("X=",as.character(mylocX),"_Y=",as.character(mylocY), sep = "")
    tempsamp[["reftype"]] <- "M01"
    tempsamp[["specnum"]] <- c()

    ll <- lapply(cur_plot[["header_data"]], as.character)
    value_position1 = -999
    position = 0
    for(element in ll){
      position = position + 1
      index = which(sapply(element, FUN=function(X) "nat2000geb" %in% X))
      if(length(index) > 0){
        value_position1 = position
        break
      }
    }
    if(value_position1 != -999){
      tempsamp[["nat2000code"]] <- cur_plot[["header_data"]][[value_position1]][["value"]]
    }
    for(species_found in cur_plot[["species_data"]]){
      tempsamp[["specnum"]] <- c(tempsamp[["specnum"]],as.numeric(species_found[["standard_record"]][["nr"]]))
    }
    fullsample[[i]]<- tempsamp
  }

  #Natura2000 gebiedsnamen  
  gebcode = c()
  gebname = c()
  if("Nat2000geb_list" %in% names(Lookup_tables)){
    for(nat2000codes in Lookup_tables[["Nat2000geb_list"]]){
      gebcode = c(gebcode,nat2000codes[["code"]])
      gebname = c(gebname,nat2000codes[["description"]])
    }
  }  
  for(i in 1:length(fullsample)){
    if("nat2000code" %in% names(fullsample[[i]]) & length(gebcode) > 0){
      fullsample[[i]][["locname"]] = gebname[which(gebcode == fullsample[[i]][["nat2000code"]])]
    }else{
      fullsample[[i]][["locname"]] = fullsample[[i]][["loccode"]]
    }
  }

  #Soortennamen
  specnum = c()
  speccode = c()
  specname = c()
  if("Species_list" %in% names(Lookup_tables)){
    for(species_tabel in Lookup_tables[["Species_list"]]){
      specnum = c(specnum,species_tabel[["nr"]])
      speccode = c(speccode,species_tabel[["code"]])
      specname = c(specname,species_tabel[["name"]])
    }
  }     
  for(i in 1:length(fullsample)){
    if("specnum" %in% names(fullsample[[i]]) & length(specnum) > 0){
      fullsample[[i]][["speccode"]] = speccode[match(fullsample[[i]][["specnum"]],specnum)]
      fullsample[[i]][["specname"]] = specname[match(fullsample[[i]][["specnum"]],specnum)]
    }else{
      fullsample[[i]][["speccode"]] <- NULL
      fullsample[[i]][["specname"]] <- NULL
    }
  }
  noreftype = 1
  return(list("Sample_data" = fullsample, "Comments" = fid, "Reference_present" = noreftype))
}

inlezenDoelsoorten <-function(datafile_path, soortgroep, soortgroepnaam){
  # inlezen van doelsoorten data 
  # soortgroep = "Macrofyten"
  # soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
  #
  # Format for this file should be a place csv consisting of for example:
  # "reftype","species_group", "species_latin"
  # "R8"    ,"Macrofyten"   , "Phragmites australis
  # etc.
  # datafile_path = paste("testdata/test_doelsoorten_diatomeeen.xlsx", sep = "")
  
  # datafile_path = paste("testdata/test_doelsoorten_waterplanten_new_DAWACO.xlsx", sep = "")
  # resultaat <- inlezenExcelOfCsv("d:/krame_lh/Documents/_Werk/Software/Subversion/Aqmad/rAqMaD/testdata/test_doelsoorten_waterplanten_new.csv")
  # resultaat <- inlezenExcelOfCsv("d:/krame_lh/Documents/_Werk/Software/Subversion/Aqmad/rAqMaD/testdata/test_doelsoorten_waterplanten_new.txt")
  
  aantal_soortgroepen = c('Diatomeeen', 'Macrofauna', 'Macrofauna_V2')
  
  #make log
  #fid = c("")
  fid = character(0)
  fatal_error = 0
  
  # open de excel file en lees de eerste sheet
  # resultaat = inlezenExcelOfCsv(datafile_path)
  resultaat = inlezenCsvOfTxt(datafile_path)
  dfr = data.frame(resultaat[[1]], stringsAsFactors = FALSE)
  fid = c(fid, resultaat[[2]])

  # controleer excel file
  columns = colnames(dfr)
  
  if(length(columns) > 0){
    colpartype = c0To0(which(columns == "PAR_TYPE"))
    collocname = c0To0(which(columns == "LOC_NAME"))
    colspecname = c0To0(which(columns == "SPEC_NAME"))
    colvaluename = c0To0(which(columns == "AANTAL"))
    
    #Check if all required columns are found in file
    if(soortgroep %in% aantal_soortgroepen){
      if(0 %in% c(colpartype, collocname, colspecname, colvaluename)){
        fid = c(fid,paste("ERROR: er wordt niet aan het benodigde file formaat voldaan. Controleer de kolomnamen. ",
                          "De import is beeindigd.", sep = ""))
        fullsample = list()   
        fatal_error = 1
        return(list("Ref_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
    }}
    if(!(soortgroep %in% aantal_soortgroepen)){
      if(0 %in% c(colpartype, collocname, colspecname)){
      fid = c(fid,paste("ERROR: er wordt niet aan het benodigde file formaat voldaan. Controleer de kolomnamen. ",
                        "De import is beeindigd.", sep = ""))
      fullsample = list()   
      fatal_error = 1
      return(list("Ref_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
      }
    }
    
    partype = unlist(lapply(dfr[,colpartype, drop = FALSE], as.character))
    upt = unique(partype)
    
    imf = 0
    for(i in 1:length(soortgroepnaam)){
      if(upt %in% soortgroepnaam[i]){
        if(imf == 0){
          imf = i
        }else{
          fid = c(fid,paste("WAARSCHUWING: meer dan een code voor", soortgroep,": ",
                            soortgroepnaam[imf]," en ", soortgroepnaam[i], ".", sep = ""))  
        }
      }
    }
    sel = c0To0(which(partype %in% soortgroepnaam[imf]))
    locname = dfr[sel,collocname]
    specname = dfr[sel,colspecname]
    
    fullsample = list()      
    
    ulocname = unique(locname)
    
    #Test if soortgroep is present
    if(length(ulocname) == 0){
      fid = c(fid,paste("ERROR: Geen doelsoorten van soortgroep ", soortgroep," in ",
                        "het inleesbestand aanwezig.", sep = ""))
      fullsample = list()   
      fatal_error = 1
      return(list("Ref_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
    }
    
    for(i in 1:length(ulocname)){ 
      
      fid = c(fid,paste("Status: inlezen monster ",i," van ",length(ulocname), ".", sep = ""))
      #Hier wordt een interactieve voortgang overzicht mogelijk gemaakt in het MatLab script
      
      tempsamp = list()
      sel = c0To0(which(locname %in% ulocname[i]))
      
      tempsamp[["locname"]] = ulocname[i]
      tempsamp[["partype"]] = soortgroep
      tempsamp[["specname"]] = specname[sel]
      if(soortgroep %in% aantal_soortgroepen){
        tempsamp[["waarde"]] = dfr[sel,colvaluename]
      }
      
      fullsample[[i]]<- tempsamp
    }
  }else{
    fullsample = list()   
    fatal_error = 1
  }
  return(list("Ref_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
}

inlezenLocatieReferentie <- function(datafile_path, soortgroep, soortgroepnaam){
  # inlezen van locatiereferentie data 
  # soortgroep = "Macrofyten"
  # soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
  #
  # Format for this file should be a place csv consisting of for example: 
  # "locname"  ,"reftype"
  # "Meetpunt1", "R8"
  # etc.
  
  #make log
  #fid = c("")
  fid = character(0)
  fatal_error = 0  
  
  #open de excel file en lees de eerste sheet
  resultaat = inlezenExcelOfCsv(datafile_path)
  dfr = data.frame(resultaat[[1]], stringsAsFactors = FALSE)
  fid = c(fid,resultaat[[2]])
  
  # controleer excel file
  columns = colnames(dfr)
  
  if(length(columns) > 0){
    collocname = c0To0(which(columns == "LOC_NAME"))
    colreftype = c0To0(which(columns == "REF_TYPE"))
    
    #Check if all required columns are found in file
    if(0 %in% c(collocname,colreftype)){
      fid = c(fid,paste("ERROR: er wordt niet aan het benodigde file formaat voldaan. Controleer de kolomnamen. ",
                        "De import is beeindigd.", sep = ""))
      fullsample = list()   
      fatal_error = 1
      return(list("LocRef_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
    }
    
    locname = dfr[,collocname]
    reftype = dfr[,colreftype]
    
    fullsample = list()      
    
    ulocname = unique(locname)
    for(i in 1:length(ulocname)){ 
      
      fid = c(fid,paste("Status: inlezen monster ",i," van ",length(ulocname), "."))
      #Hier wordt een interactieve voortgang overzicht mogelijk gemaakt in het MatLab script
      
      tempsamp = list()
      sel = c0To0(which(locname %in% ulocname[i]))
      
      tempsamp[["locname"]] = ulocname[i]
      if(length(reftype[sel])>1){
        fid = c(fid,paste("WAARSCHUWING: meer dan een referentie voor locatie '",ulocname[i],"'. ",
                          "Alleen de eerste wordt gebruikt.", sep = "")) 
        tempsamp[["reftype"]] = reftype[sel][1]
      }else{
        tempsamp[["reftype"]] = reftype[sel]
      }

      fullsample[[i]]<- tempsamp
    }
  }else{
    fullsample = list()   
    fatal_error = 1
  }
  return(list("LocRef_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
}


inlezenKRWrefDoelsoorten <-function(datafile_path, reftypes, soortgroep, soortgroepnaam){
  # inlezen van doelsoorten data 
  # soortgroep = "Macrofyten"
  # soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
  #
  # Format for this file should be a place csv consisting of for example:
  # "reftype","species_group", "species_latin"
  # "R8"    ,"Macrofyten"   , "Phragmites australis
  # etc.
  
  # datafile_path <- "data/AqMaD referentietypes.csv"
  # reftypes <- c("R17", "R18")
  # reftypes <- unique(groepPlotData$reftype)
  library(tidyr)
  
  #make log
  # fid = c("")
  fid = character(0)
  fatal_error = 0
  
  #open de excel file en lees de eerste sheet
  resultaat = inlezenExcelOfCsv(datafile_path)
  dfr = data.frame(resultaat[[1]], stringsAsFactors = FALSE)
  fid = c(fid,resultaat[[2]])
  
  ## reshape dfr
  colnames(dfr) <- dfr[1,]
  dfr <- dfr[-1,]
  dfr_g <- gather(dfr, key = REF_TYPE, value = SPEC_NAME)
  dfr_sel <- dfr_g[dfr_g$REF_TYPE %in% reftypes,]
  dfr_sel$PAR_TYPE <- "MACFT"
  dfr_selc <- dfr_sel[dfr_sel$SPEC_NAME != "",]
  dfr <- dfr_selc
  
  # controleer excel file
  columns = colnames(dfr)
  
  if(length(columns) > 0){
    colpartype = c0To0(which(columns == "PAR_TYPE"))
    colreftype = c0To0(which(columns == "REF_TYPE"))
    colspecname = c0To0(which(columns == "SPEC_NAME"))
    
    #Check if all required columns are found in file
    if(0 %in% c(colpartype,colreftype,colspecname)){
      fid = c(fid,paste("ERROR: er wordt niet aan het benodigde file formaat voldaan. Controleer de kolomnamen. ",
                        "De import is beeindigd.", sep = ""))
      fullsample = list()   
      fatal_error = 1
      return(list("Ref_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
    }  
    
    partype = unlist(lapply(dfr[,colpartype, drop = FALSE], as.character))
    upt = unique(partype)
    
    imf = 0
    for(i in 1:length(soortgroepnaam)){
      if(upt %in% soortgroepnaam[i]){
        if(imf == 0){
          imf = i
        }else{
          fid = c(fid,paste("WAARSCHUWING: meer dan een code voor", soortgroep,": ",
                            soortgroepnaam[imf]," en ", soortgroepnaam[i], ".", sep = ""))  
        }
      }
    }
    sel = c0To0(which(partype %in% soortgroepnaam[imf]))
    reftype = dfr[sel,colreftype]
    specname = dfr[sel,colspecname]
    
    fullsample = list()      
    
    ureftype = unique(reftype)
    
    #Test if soortgroep is present
    if(length(ureftype) == 0){
      fid = c(fid,paste("ERROR: geen doelsoorten van soortgroep ", soortgroep," in ",
                        "het inleesbestand aanwezig", sep = ""))
      fullsample = list()   
      fatal_error = 1
      return(list("Ref_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
    }
    
    for(i in 1:length(ureftype)){ 
      
      fid = c(fid,paste("Status: inlezen monster ",i," van ",length(ureftype), "."))
      #Hier wordt een interactieve voortgang overzicht mogelijk gemaakt in het MatLab script
      
      tempsamp = list()
      sel = c0To0(which(reftype %in% ureftype[i]))
      
      tempsamp[["reftype"]] = ureftype[i]
      tempsamp[["partype"]] = soortgroep
      tempsamp[["specname"]] = specname[sel]
      
      fullsample[[i]]<- tempsamp
    }
  }else{
    fullsample = list()   
    fatal_error = 1
  }
  return(list("Ref_data" = fullsample, "Comments" = fid, "Fatal_error" = fatal_error))
}


###TEST SCRIPTS

#Lees EcoBase_QBW
# path_ECOBASE_QBW = paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
# "Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Ecobase_export.csv", sep = "")
# soortgroep = "Macrofyten"
# soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
# testlist = inlezenEcobaseQBW(path_ECOBASE_QBW, soortgroep, soortgroepnaam)
# testlist[[1]]

# #Lees EcoBase
# path_ECOBASE = paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
#                          "Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Ecobase.csv", sep = "")
# soortgroep = "Macrofyten"
# soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
# testlist = inlezenEcobase(path_ECOBASE, soortgroep, soortgroepnaam)
# testlist[[1]]

# #Lees Ecolims
# path_ECOLIMS1 = paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
#                           "Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Ecolims_vb1.xls", sep = "") 
# path_ECOLIMS2 = paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
#                       "Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Ecolims_vb2.xls", sep = "") 
# 
# soortgroep = "Macrofyten"
# soortgroepnaam = c("MAFY","MACFT","mafy","macft","macfy","Vegetatie")
# testlist1 = inlezenEcolims(path_ECOLIMS1, soortgroep, soortgroepnaam)
# testlist1[[1]]
# testlist2 = inlezenEcolims(path_ECOLIMS2, soortgroep, soortgroepnaam)
# testlist2[[1]]

# #Lees DAWACO
# path_DAWACO = paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
#                       "Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Dawaco_export.txt", sep = "")
# soortgroep = "Macrofyten"
# soortgroepnaam = c('MAFY','MACFT','MACFY','mafy','macft','macfy','MFYT')
# testlist1 = inlezenDawaco(path_DAWACO, soortgroep, soortgroepnaam)
# testlist1[[1]]

# #Lees Turboveg
# path_Turboveg = paste("n:\\Projects\\11202500\\11202634\\F. Other information\\achtergrond\\",
#                       "Versie 2.1.5. - Beschikbaar via STOWA (inclusief Matlab)\\Testdata\\Turboveg_export.XML", sep = "")
# testlist1 = inlezenTurboveg(path_Turboveg)
# testlist1[[1]]


# # #Lees Doelsoorten
# path_doelsoorten = paste("..\\testdata\\test_doelsoorten.xlsx", sep = "")
# soortgroep = "Macrofyten"
# soortgroepnaam = c("Macrofyten")
# testlist1 = inlezenDoelsoorten(path_doelsoorten, soortgroep, soortgroepnaam)
# testlist1[[1]] 