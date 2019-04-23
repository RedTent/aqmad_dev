################################################################
# AqMaD rekenkern functionaliteiten voor RShiny applicatie     #
#                                                              #
# Auteurs: Marc Weeber                                         #
#          Willem Stolte                                       #
#          Lilith Kramer                                       #
#                                                              #
#                                                              #
# Datum : 2019-04-23                                           #
# Bedrijf: Deltares                                            #
# Licentie: GNU General Public License                         #
#                                                              #           
# Contact : Gerben van Geest                                   #
# Email : gerben.vangeest@deltares.nl                          #
#                                                              #
################################################################ 

source("AqMaD_inleesfuncties.R")

VertaalSoorten <-function(monsters, type_data , path_databases){
  
  #monsters <- controlelist[[1]]
  #type_data <- soortgroep
  #path_databases <- "data\\"
  
  #Deze functie vervangd wanneer mogelijk de soortennamen, soort afkortingen
  #, nederlandse namen voor de latijnse namen.
  fid = c()
  fatal_error = 0
  
  #Get database name
  if(type_data == "Macrofyten"){
    TWN_list_path = paste(path_databases,"TWN_Macrophytes_20180619.csv", sep = "")

  }else if(type_data == "Diatomeeen"){
    TWN_list_path = paste(path_databases,"TWN_Diatoms_20180619.csv", sep = "")
    
  }else if(type_data == "Vissen"){
    TWN_list_path = paste(path_databases,"TWN_Fish_20180619.csv", sep = "")
    
  }else if(type_data == "Macrofauna" | type_data == "Macrofauna_V2"){
    TWN_list_path = paste(path_databases,"TWN_Macroinvertebrates_20180619.csv", sep = "")
    
  }else{
    fid = c(fid,paste("ERROR: onbekende soortgroep ", as.character(type_data), ".", sep =""))
    fatal_error = 1
    fullsample = list()
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  #Read database of type_data
  TWN_list = inlezenExcelOfCsv(TWN_list_path)
  TWN = TWN_list[[1]] 
  
  header_column = TWN[1,]
  data_TWN = TWN[(2:length(TWN[,1])),]
  colnames(data_TWN) <- as.vector(header_column)
  
  #Correct species names
  if(length(monsters) > 0){
    #Check if the monsters is filled
    for(nr in 1:length(monsters)){

      #Check specname
      if("specname" %in% names(monsters[[nr]])){
        #Check if reftype is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["specname"]])) & length(monsters[[nr]][["specname"]]) > 0){
          
          #check if specname exists in taxonname TWN list
          link1 = match(monsters[[nr]][["specname"]],data_TWN$taxonname)
          
          #If not check if specname exists in taxoncode
          link2 = match(monsters[[nr]][["specname"]][is.na(link1)],data_TWN$taxoncode)
          
          #If not check if specname exists in localname
          link3 = match(monsters[[nr]][["specname"]][is.na(link1)][is.na(link2)],data_TWN$localname)
          
          if(TRUE %in% !(is.na(link2))){
            
            fid = c(fid,paste("Melding: in monster ",nr," zijn soortennamen vervangen ",
                    "van 'taxoncode' naar 'taxonname' in kolom specname.", sep = ""))
            
            #Add link2
            monsters[[nr]][["specname"]][is.na(link1)][!(is.na(link2))] = data_TWN$taxonname[link2][!(is.na(link2))]
          }
          
          if(TRUE %in% !(is.na(link3))){
            
            fid = c(fid,paste("Melding: in monster ",nr," zijn soortennamen vervangen ",
                              "van 'localname' naar 'taxonname' in kolom specname.", sep = ""))
            
            #Add link3
            monsters[[nr]][["specname"]][is.na(link1)][is.na(link2)][!(is.na(link3))] = data_TWN$taxonname[link3][!(is.na(link3))]  
          }
          if(TRUE %in% is.na(link3)){
            fid = c(fid,paste("WAARSCHUWING: in monster ",nr," zijn soortennamen aanwezig ",
                              "die niet voorkomen in de TWN lijst onder taxonnaam, localname of taxoncode.",
                              " Namen zijn: ",paste(monsters[[nr]][["specname"]][is.na(link1)][is.na(link2)][is.na(link3)],
                              collapse = ", ", ".", sep = "")))
          }
        }
      }else{
        fid = c(fid,paste("ERROR: geen specname positie aangemaakt bij monster nr ", nr, ".", sep = ""))
        fatal_error = 1
      }     
    }
  }else{
    fid = c(fid,paste("ERROR: geen relevante monsters gevonden in aangeleverde monitoringsdata.", sep = ""))
    fatal_error = 1
  }
  return(list("Data" = monsters, "Comments" = fid, "Fatal_error" = fatal_error))
}


ControleMonsterData <- function(monsters){
  #Controleer de monsterdata afkomstig uit de inleesfuncties en
  # corrigeer deze waar nodig. Deze monsterdata moet minstens per monster te beschikken
  # over een smpcode, locname, loccode, locX, locY, reftype, speccode, specname en datesmp.
  # Alle data dient in een list met alle monsters aangeboden te worden.
  # Deze data dient ingelezen te worden met de bestandspecifieke functies 
  #(EcobaseQBW, Ecobase, Ecolims, Dawaco, Turboveg) in de AqMaD_inleesfuncties.
  
  fid = c()
  fatal_error = 0
  temp_error = 0
  
  #Loop over monsters and check the data
  store_smpcode = c()
  store_locname = c()
  
  if(length(monsters) > 0){
    #Check if the monsters is filled
    for(nr in 1:length(monsters)){
      
      #Check of smpcode exists in list
      if("smpcode" %in% names(monsters[[nr]])){
        #Check if smpcode is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["smpcode"]])) & length(monsters[[nr]][["smpcode"]]) > 0 &
           monsters[[nr]][["smpcode"]] != ""){
          if(length(monsters[[nr]][["smpcode"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: meer dan een smpcode bij monster nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            monsters[[nr]][["smpcode"]] = monsters[[nr]][["smpcode"]][1]
          }
          monsters[[nr]][["smpcode"]] = as.character(monsters[[nr]][["smpcode"]])
          store_smpcode = c(store_smpcode,monsters[[nr]][["smpcode"]])
        }else{
          fid = c(fid,paste("ERROR: geen smpcode bij monster nr ", nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen smpcode positie aangemaakt bij monster nr ", nr, ".", sep = ""))
        fatal_error = 1
      }
      
      #Check locname
      if("locname" %in% names(monsters[[nr]])){
        #Check if locname is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["locname"]])) & length(monsters[[nr]][["locname"]]) > 0 &
           monsters[[nr]][["locname"]] != ""){
          if(length(monsters[[nr]][["locname"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: meer dan een locname bij monster nr ", nr,". De eerste wordt gebruikt.", sep = ""))
            monsters[[nr]][["locname"]] = monsters[[nr]][["locname"]][1]
          }
          monsters[[nr]][["locname"]] = as.character(monsters[[nr]][["locname"]])
          store_locname = c(store_locname,monsters[[nr]][["locname"]])
        }else{
          fid = c(fid,paste("WAARSCHUWING: geen locname bij monster nr ", nr, ".", sep = ""))
          temp_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen locname positie aangemaakt bij monster nr ",
                          as.character(nr), ".", sep = ""))
        fatal_error = 1
      }   

      #Check loccode
      if("loccode" %in% names(monsters[[nr]])){
        #Check if loccode is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["loccode"]])) & length(monsters[[nr]][["loccode"]]) > 0 &
           monsters[[nr]][["loccode"]] != ""){
          if(length(monsters[[nr]][["loccode"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: meer dan een locname bij monster nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            monsters[[nr]][["loccode"]] = monsters[[nr]][["locname"]][1]
          }
          monsters[[nr]][["loccode"]] = as.character(monsters[[nr]][["loccode"]])
          store_locname = c(store_locname,monsters[[nr]][["loccode"]])
        }else{
          fid = c(fid,paste("WAARSCHUWING: geen loccode bij monster nr ", nr, ".", sep = ""))
          temp_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen locname positie aangemaakt bij monster nr ",
                          as.character(nr), ".", sep = ""))
        fatal_error = 1
      }   
      
      #Check if locname and loccode can be interchanged
      if(temp_error == 1 & "locname" %in% names(monsters[[nr]]) & "loccode" %in% names(monsters[[nr]])){
        fid = c(fid,paste("WAARSCHUWING: locname en loccode gelijk gemaakt aan elkaar bij monster nr ",
                          as.character(nr), ".", sep = ""))
        if(monsters[[nr]][["locname"]] != "" & !(is.na(monsters[[nr]][["locname"]]))){
          monsters[[nr]][["locname"]] = monsters[[nr]][["loccode"]]
        }
        if(monsters[[nr]][["loccode"]] != "" & !(is.na(monsters[[nr]][["loccode"]]))){
          monsters[[nr]][["loccode"]] = monsters[[nr]][["locname"]]  
        }
        #Check replacement
        if(monsters[[nr]][["loccode"]] == "" | is.na(monsters[[nr]][["loccode"]])){
          fid = c(fid,paste("WAARSCHUWING: locname en loccode waren beide leeg en zijn gelijk gemaakt aan smpcode bij monster nr ",
                            as.character(nr), ".", sep = ""))
          monsters[[nr]][["locname"]] = monsters[[nr]][["smpcode"]]
          monsters[[nr]][["loccode"]] = monsters[[nr]][["smpcode"]]
        }
      }
 
      #Check locX
      if("locX" %in% names(monsters[[nr]])){
        #Check if locX is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["locX"]])) & length(monsters[[nr]][["locX"]]) > 0){
          if(length(monsters[[nr]][["locX"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: meer dan een locX bij monster nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            monsters[[nr]][["locX"]] = monsters[[nr]][["locX"]][1]
          }
          monsters[[nr]][["locX"]] = as.numeric(monsters[[nr]][["locX"]])
          if(monsters[[nr]][["locX"]] == -999){
            monsters[[nr]][["locX"]] = 0
            fid = c(fid,paste("WAARSCHUWING: locX omgezet van -999 naar ", as.character(monsters[[nr]][["locX"]]), " voor monster nr = ",
                              as.character(nr), ".", sep = ""))
          }
        }else{
          fid = c(fid,paste("ERROR: geen locX bij monster nr ",nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen locX positie aangemaakt bij monster nr ",nr, ".", sep = ""))
        fatal_error = 1
      }
      
      #Check locY
      if("locY" %in% names(monsters[[nr]])){
        #Check if locY is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["locY"]])) & length(monsters[[nr]][["locY"]]) > 0){
          if(length(monsters[[nr]][["locY"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: meer dan een locY bij monster nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            monsters[[nr]][["locY"]] = monsters[[nr]][["locY"]][1]
          }
          monsters[[nr]][["locY"]] = as.numeric(monsters[[nr]][["locY"]])
          if(monsters[[nr]][["locY"]] == -999){
            monsters[[nr]][["locY"]] = 0
            fid = c(fid,paste("WAARSCHUWING: locY omgezet van -999 naar ", as.character(monsters[[nr]][["locX"]]), " voor monster nr = ",
                              as.character(nr), ".", sep = ""))
          }
        }else{
          fid = c(fid,paste("ERROR: geen locY bij monster nr ", nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen locY positie aangemaakt bij monster nr ",nr, ".", sep = ""))
        fatal_error = 1
      }
      
      #Check reftype
      if("reftype" %in% names(monsters[[nr]])){
        #Check if reftype is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["reftype"]])) & length(monsters[[nr]][["reftype"]]) > 0){
          if(length(monsters[[nr]][["reftype"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: Meer dan een reftype bij monster nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            monsters[[nr]][["reftype"]] = as.character(monsters[[nr]][["reftype"]][1])
          }
          monsters[[nr]][["reftype"]] = as.character(monsters[[nr]][["reftype"]])
        }else{
          fid = c(fid,paste("WAARSCHUWING: geen reftype bij monster nr ",nr, ".", sep = ""))
        }
      }else{
        fid = c(fid,paste("ERROR: geen reftype positie aangemaakt bij monster nr ",nr, ".", sep = ""))
        fatal_error = 1
      }      
      
      #Check speccode
      if("speccode" %in% names(monsters[[nr]])){
        #Check if speccode is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["speccode"]])) & length(monsters[[nr]][["speccode"]]) > 0){
          monsters[[nr]][["speccode"]] = as.character(monsters[[nr]][["speccode"]])
        }else{
          fid = c(fid,paste("WAARSCHUWING: geen speccode bij monster nr ",nr, ".", sep = ""))
        }
      }else{
        fid = c(fid,paste("ERROR: geen speccode positie aangemaakt bij monster nr ",nr, ".", sep = ""))
        fatal_error = 1
      }            
      
      #Check specname
      if("specname" %in% names(monsters[[nr]])){
        #Check if specname is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["specname"]])) & length(monsters[[nr]][["specname"]]) > 0){
          monsters[[nr]][["specname"]] = as.character(monsters[[nr]][["specname"]])
        }else{
          fid = c(fid,paste("WAARSCHUWING: geen specname bij monster nr ",nr, ".", sep = ""))
        }
      }else{
        fid = c(fid,paste("ERROR: geen specname positie aangemaakt bij monster nr ",nr, ".", sep = ""))
        fatal_error = 1
      }  
      
      #Check waarde
      if("waarde" %in% names(monsters[[nr]])){
        #Check if waarde is filled
        if(TRUE %in% !(is.na(monsters[[nr]][["waarde"]])) & length(monsters[[nr]][["waarde"]]) > 0){
          monsters[[nr]][["waarde"]] = as.numeric(monsters[[nr]][["waarde"]])
        }else{
          fid = c(fid,paste("WAARSCHUWING: geen waarde bij monster nr ",nr, ".", sep = ""))
        }
      }else{
        fid = c(fid,paste("WAARSCHUWING: geen waarde positie aangemaakt bij monster nr ",nr, ".", sep = ""))
        #momenteel importeren niet alle functies waardes, alleen DAWACO
        fatal_error = 0
      } 

      #Check datesmp
      if("datesmp" %in% names(monsters[[nr]])){
        #Check if datesmp is filled
        if(!(is.na(monsters[[nr]][["datesmp"]])) & length(monsters[[nr]][["datesmp"]]) > 0){
          if(length(monsters[[nr]][["datesmp"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: Meer dan een datesmp bij monster nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            monsters[[nr]][["datesmp"]] = monsters[[nr]][["datesmp"]][1]
          }
          datum = as.character(monsters[[nr]][["datesmp"]])
          monsters[[nr]][["datesmp"]] = strptime(datum, format = "%y%y-%m-%d")
          if(is.na(monsters[[nr]][["datesmp"]])){
            monsters[[nr]][["datesmp"]] = strptime(datum, format = "%y%y-%m-%d %H:%M:%S")
            if(is.na(monsters[[nr]][["datesmp"]])){
              fid = c(fid,paste("ERROR: Verkeerd datesmp format bij  monster nr = ",nr," format = ", datum, sep = ""))
              fatal_error = 1
            }
          }
        }else{
          fid = c(fid,paste("ERROR: geen datesmp bij monster nr ",nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen datesmp positie aangemaakt bij monster nr ",nr, ".", sep = ""))
        fatal_error = 1
      }

    }
  }else{
    fid = c(fid,paste("ERROR: geen relevante monsters gevonden in aangeleverde monitoringsdata.", sep = ""))
    fatal_error = 1
  }
  
  #Go over samples and remove doubles of samplecodes
  #TODO
  
  return(list("Data" = monsters, "Comments" = fid, "Fatal_error" = fatal_error))
}

ControleDoelsoortData <- function(doelsoortendata){
  #Controleer de doelsoortdata afkomstig uit de inleesfuncties. 
  # Deze doelsoortdata moet minstens beschikken over een referentie type,
  # soortgroep en soorten.
  # Alle data dient in een list met de referentie aangeboden te worden.
  # Deze data dient ingelezen te worden met de functie InlezenDoelsoorten()
  # in de AqMaD_inleesfuncties.
  
  # doelsoortendata <- krwDoelsoorten
  
  fid = c()
  fatal_error = 0
  temp_error = 0
  
  #Loop over monsters and check the data
  store_locname = c()
  
  if(length(doelsoortendata) > 0){
    #Check if the doelsoortendata is filled
    for(nr in 1:length(doelsoortendata)){
      
      # nr <- 1
      
      #Check locname
      if("locname" %in% names(doelsoortendata[[nr]])){
        #Check if locname is filled
        if(TRUE %in% !(is.na(doelsoortendata[[nr]][["locname"]])) & length(doelsoortendata[[nr]][["locname"]]) > 0){
          if(length(doelsoortendata[[nr]][["locname"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: Meer dan een locname bij referentie nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            doelsoortendata[[nr]][["locname"]] = as.character(doelsoortendata[[nr]][["locname"]][1])
          }
          doelsoortendata[[nr]][["locname"]] = as.character(doelsoortendata[[nr]][["locname"]])
          store_locname = c(store_locname,doelsoortendata[[nr]][["locname"]])
        }else{
          fid = c(fid,paste("ERROR: geen locname bij referentie nr ",nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen locname positie aangemaakt bij referentie nr ",nr, ".", sep = ""))
        fatal_error = 1
      }      

      #Check specname
      if("specname" %in% names(doelsoortendata[[nr]])){
        #Check if loccode is filled
        if(TRUE %in% !(is.na(doelsoortendata[[nr]][["specname"]])) & length(doelsoortendata[[nr]][["specname"]]) > 0){
          doelsoortendata[[nr]][["specname"]] = as.character(doelsoortendata[[nr]][["specname"]])
        }else{
          fid = c(fid,paste("ERROR: geen specname bij referentie nr ",nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen specname positie aangemaakt bij referentie nr ",nr, ".", sep = ""))
        fatal_error = 1
      }
    }
  }else{
    fid = c(fid,paste("ERROR: geen relevante referenties gevonden in aangeleverde doelsoortendata", sep = ""))
    fatal_error = 1
  }
  
  #Go over loccodes and remove doubles of samplecodes
  #TODO

  return(list("Data" = doelsoortendata, "Comments" = fid, "Fatal_error" = fatal_error))
}      

ControleKRWData <- function(doelsoortendata){
  #Controleer de doelsoortdata afkomstig uit de inleesfuncties. 
  # Deze doelsoortdata moet minstens beschikken over een referentie type,
  # soortgroep en soorten.
  # Alle data dient in een list met de referentie aangeboden te worden.
  # Deze data dient ingelezen te worden met de functie InlezenDoelsoorten()
  # in de AqMaD_inleesfuncties.
  
  # doelsoortendata <- krwDoelsoorten
  
  fid = c()
  fatal_error = 0
  temp_error = 0
  
  #Loop over monsters and check the data
  store_reftype = c()
  
  if(length(doelsoortendata) > 0){
    #Check if the doelsoortendata is filled
    for(nr in 1:length(doelsoortendata)){
      
      # nr <- 1
      
      #Check reftype
      if("reftype" %in% names(doelsoortendata[[nr]])){
        #Check if reftype is filled
        if(TRUE %in% !(is.na(doelsoortendata[[nr]][["reftype"]])) & length(doelsoortendata[[nr]][["reftype"]]) > 0){
          if(length(doelsoortendata[[nr]][["reftype"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: meer dan een reftype bij referentie nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            doelsoortendata[[nr]][["reftype"]] = as.character(doelsoortendata[[nr]][["reftype"]][1])
          }
          doelsoortendata[[nr]][["reftype"]] = as.character(doelsoortendata[[nr]][["reftype"]])
          store_reftype = c(store_reftype,doelsoortendata[[nr]][["reftype"]])
        }else{
          fid = c(fid,paste("ERROR: geen reftype bij referentie nr ",nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen reftype positie aangemaakt bij referentie nr ",nr, ".", sep = ""))
        fatal_error = 1
      }      
      
      #Check specname
      if("specname" %in% names(doelsoortendata[[nr]])){
        #Check if loccode is filled
        if(TRUE %in% !(is.na(doelsoortendata[[nr]][["specname"]])) & length(doelsoortendata[[nr]][["specname"]]) > 0){
          doelsoortendata[[nr]][["specname"]] = as.character(doelsoortendata[[nr]][["specname"]])
        }else{
          fid = c(fid,paste("ERROR: geen specname bij referentie nr ",nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen specname positie aangemaakt bij referentie nr ",nr, ".", sep = ""))
        fatal_error = 1
      }
    }
  }else{
    fid = c(fid,paste("ERROR: geen relevante referenties gevonden in aangeleverde doelsoortendata", sep = ""))
    fatal_error = 1
  }
  
  #Go over loccodes and remove doubles of samplecodes
  #TODO
  
  return(list("Data" = doelsoortendata, "Comments" = fid, "Fatal_error" = fatal_error))
}

ControleLocatieReferentie <- function(locatiereferentie){
  #Controleer de locatiereferentie afkomstig uit de inleesfuncties. 
  # Deze locatiereferentie moet minstens beschikken over een locatiecode.
  # Alle data dient in een list met de monsterlocaties aangeboden te worden.
  # Deze data dient ingelezen te worden met de functie InlezenDoelsoorten()
  # in de AqMaD_inleesfuncties.
  
  fid = c()
  fatal_error = 0
  temp_error = 0
  
  #Loop over monsters and check the data
  store_locname = c()
  
  if(length(locatiereferentie) > 0){
    #Check if the locatiereferentie is filled
    for(nr in 1:length(locatiereferentie)){
      
      #Check locname
      if("locname" %in% names(locatiereferentie[[nr]])){
        #Check if reftype is filled
        if(TRUE %in% !(is.na(locatiereferentie[[nr]][["locname"]])) & length(locatiereferentie[[nr]][["locname"]]) > 0){
          if(length(locatiereferentie[[nr]][["locname"]]) > 1){
            fid = c(fid,paste("WAARSCHUWING: Meer dan een locname bij locatiereferentie nr ",nr,". De eerste wordt gebruikt.", sep = ""))
            locatiereferentie[[nr]][["locname"]] = as.character(locatiereferentie[[nr]][["locname"]][1])
          }
          locatiereferentie[[nr]][["locname"]] = as.character(locatiereferentie[[nr]][["locname"]])
          store_locname = c(store_locname,locatiereferentie[[nr]][["locname"]])
        }else{
          fid = c(fid,paste("ERROR: geen locname bij locatiereferentie nr ",nr, ".", sep = ""))
          fatal_error = 1
        }
      }else{
        fid = c(fid,paste("ERROR: geen locname positie aangemaakt bij locatiereferentie nr ",nr, ".", sep = ""))
        fatal_error = 1
      }      
    }
  }else{
    fid = c(fid,paste("ERROR: geen relevante monsters gevonden in aangeleverde monitoringsdata", sep = ""))
    fatal_error = 1    
  }
  
  return(list("Data" = locatiereferentie, "Comments" = fid, "Fatal_error" = fatal_error))
}


MatchReferentieDoel <- function(doelabiotiekdata, locatiereferentie){
  
  # doelabiotiekdata <- statistieklist[[1]]
  # locatiereferentie <- controlelist2[[1]]
  
  fid = c()
  fatal_error = 0
  
  store_doelsoortreferenties = c()
  store_locatiereferenties = c()
  
  #Get doelabiotiek and abiotische parameters
  doelabiotiek = doelabiotiekdata[[1]]
  abiotische_parameters = doelabiotiekdata[["Abiotics"]]
  
  #Make doelsoort vector
  for(nr1 in 1:length(doelabiotiek)){
    #Get doelsoort referenties
    store_doelsoortreferenties = c(store_doelsoortreferenties,doelabiotiek[[nr1]][["reftype"]])
  }
  
  #Make locaties vector
  for(nr2 in 1:length(locatiereferentie)){
    #Get doelsoort referenties
    store_locatiereferenties = c(store_locatiereferenties,locatiereferentie[[nr2]][["reftype"]])
  }
  
  locref_match = match(store_locatiereferenties,store_doelsoortreferenties)
  doelref_match = match(store_doelsoortreferenties,store_locatiereferenties)
  
  if(TRUE %in% is.na(locref_match)){
    fid = c(fid,paste("WAARSCHUWING: de volgende locatie referenties komen niet voor in de aangeleverde doelreferenties '",
                      paste(store_locatiereferenties[is.na(locref_match)], sep = "','"),"'.",
                      " De locaties met deze referenties worden verwijderd uit de analyse.", sep = ""))
    
  }
  if(TRUE %in% is.na(doelref_match)){
    fid = c(fid,paste("WAARSCHUWING: de volgende beschikbare doelreferenties worden niet gebruikt door de locaties '",
                      paste(store_doelsoortreferenties[is.na(doelref_match)], sep = "','", collapse = "','"),"'.",
                      sep = ""))    
  }

  #Make complete doel per locatie list
  full_list = list()
  nr_full_list = 0
  for(nr3 in 1:length(locatiereferentie)){
    
    #Check if locatiereferentie will be taken up
    match_value = locref_match[nr3]
    
    if(!(is.na(match_value))){
      #add number
      nr_full_list = nr_full_list + 1
      
      #Get doel
      doel = doelabiotiek[[match_value]]
      
      #Get locatie
      locatie = locatiereferentie[[nr3]]
      
      #Combine in list
      temp_list = list()
      temp_list[["locname"]] = locatie[["locname"]]
      temp_list[["reftype"]] = locatie[["reftype"]]
      temp_list[["Abiotic_results"]] = doel[["Abiotic_results"]]
      temp_list[["Statistic_results"]] = doel[["Statistic_results"]]
      
      full_list[[nr_full_list]] <- temp_list
    }
  }
  if(length(full_list) == 0){
    fid = c(fid,paste("ERROR: geen van de lokaties en referenties lijken met elkaar te corresponderen. Controleer deze."))
    fatal_error  = 1
    full_list_comp = list()
  }else{
    full_list_comp = list("Sample_data" = full_list,"Abiotics" = abiotische_parameters)
  }
  return(list("Data" = full_list_comp, "Comments" = fid, "Fatal_error" = fatal_error))
}      

MatchMonsterReferentie <- function(monsters, referentie){
  # Deze functie verbind de gemeten en gewenste abiotiek aan elkaar.
  # voor zowel de gemeten (monsters) als gewenste (referentie) moet
  # de Statistiek zijn bepaald met BerekenenStatistiekWaarden() en hierna moeten voor 
  # de gewenste (referentie) abiotiek deze worden verbonden aan de meetlocaties door
  # het uitvoeren van de functie ControleLocatieReferentie() op de locatiereferentie data
  # en MatchReferentieDoel() voor de locatiereferentie data en gewenste (referentie) abiotiek.
  
  ## matched = MatchMonsterReferentie(statistieklist_samp[[1]], combined_list2[[1]])
  # monsters = statistieklist_samp[[1]]
  # referentie = combined_list2[[1]]
  # monsters = statistieklist_samp[[1]]
  # referentie = statistieklist_doel[[1]]
  
  
  fid = c()
  fatal_error = 0
  
  store_monsterlocaties = c()
  store_referentielocaties = c()
  
  #Get monsterabiotiek and abiotische parameters
  monsterabiotiek = monsters[[1]]
  monsters_abpar = monsters[["Abiotics"]]
  
  #Get refentieabiotiek and abiotische parameters
  referentieabiotiek = referentie[[1]]
  referentie_abpar = referentie[["Abiotics"]]
  
  if(!(all.equal(monsters_abpar,referentie_abpar))){
    
    full_list_comp = list()
    fid = c(fid, paste("ERROR: Verschil in abiotische parameters tussen meetdata en referentie./n",
                       "Mogelijk zijn hier verschillende soortgroepen gebruikt./n",
                       " Meetdata : ", paste(monsters_abpar[1,],collapse = ","),"./n",
                       " Referentie : ", paste(referentie_abpar[1,],collapse = ","),"./n", sep = ""))
    fata_error = 1
    return(list("Data" = full_list_comp,"Comments" = fid, "Fatal_error" = fatal_error))
  }
  
  #Make monster vector
  for(nr1 in 1:length(monsterabiotiek)){
    #Get monsterlokaties
    store_monsterlocaties = c(store_monsterlocaties,monsterabiotiek[[nr1]][["locname"]])
  }
  
  #Make locaties vector
  for(nr2 in 1:length(referentieabiotiek)){
    #Get referentielokaties
    store_referentielocaties = c(store_referentielocaties,referentieabiotiek[[nr2]][["locname"]])
  }
  
  monloc_match = match(store_monsterlocaties,store_referentielocaties)
  refloc_match = match(store_referentielocaties,store_monsterlocaties)
  
  if(TRUE %in% is.na(monloc_match)){
    fid = c(fid,paste("WAARSCHUWING: de volgende monster locaties komen niet voor in de aangeleverde doelreferenties '",
                      paste(store_monsterlocaties[is.na(monloc_match)], sep = "','"),"'.",
                      " De locaties met deze naam worden verwijderd uit de analyse.", sep = ""))
    
  }
  if(TRUE %in% is.na(refloc_match)){
    fid = c(fid,paste("WAARSCHUWING: de volgende beschikbare refenties voor locaties komen niet voor in de meetdata '",
                      paste(store_referentielocaties[is.na(refloc_match)], sep = "','", collapse = "','"),"'.",
                      sep = ""))    
  }
  
  #Make complete doel per locatie list
  full_list = list()
  nr_full_list = 0
  for(nr3 in 1:length(monsterabiotiek)){
    
    #Check if locatiereferentie will be taken up
    match_value = monloc_match[nr3]
    
    if(!(is.na(match_value))){
      #add number
      nr_full_list = nr_full_list + 1
      
      #Get referentie
      referentie = referentieabiotiek[[match_value]]
      
      #Get locatie
      monster = monsterabiotiek[[nr3]]
      
      # print(referentie[["Statistic_results"]])
      
      #Combine in list
      temp_list = list()
      temp_list[["smpcode"]] = monster[["smpcode"]]
      temp_list[["locname"]] = monster[["locname"]]
      temp_list[["datesmp"]] = monster[["datesmp"]]
      temp_list[["locX"]] = monster[["locX"]]
      temp_list[["locY"]] = monster[["locY"]]
      temp_list[["reftype"]] = referentie[["reftype"]]
      temp_list[["sample_statistics"]] = monster[["Statistic_results"]]
      temp_list[["reference_statistics"]] = referentie[["Statistic_results"]]
      
      full_list[[nr_full_list]] <- temp_list
    }
  }
  if(length(full_list) == 0){
    fid = c(fid,paste("ERROR: geen van de meetwaarden lokaties en referentie lokaties",
          "lijken met elkaar te corresponderen. Controleer deze.", sep = ""))
    fatal_error  = 1
    full_list_comp = list()
  }else{
    full_list_comp = list("Sample_data" = full_list,"Abiotics" = monsters_abpar)
  }
  return(list("Data" = full_list_comp,"Comments" = fid,"Fatal_error" = fatal_error))
}

MatchMonsterKRWReferentie <- function(monsters, referentie){
  # Deze functie verbind de gemeten en gewenste abiotiek aan elkaar.
  # voor zowel de gemeten (monsters) als gewenste (referentie) moet
  # de Statistiek zijn bepaald met BerekenenStatistiekWaarden() en hierna moeten voor 
  # de gewenste (referentie) abiotiek deze worden verbonden aan de meetlocaties door
  # het uitvoeren van de functie ControleLocatieReferentie() op de locatiereferentie data
  # en MatchReferentieDoel() voor de locatiereferentie data en gewenste (referentie) abiotiek.
  
  ## matched = MatchMonsterReferentie(statistieklist_samp[[1]], combined_list2[[1]])
  # monsters = statistieklist_samp[[1]]
  # referentie = combined_list2[[1]]
  # monsters = statistieklist_samp[[1]]
  # referentie = statistieklist_krw[[1]]
  
  
  fid = c()
  fatal_error = 0
  
  store_monsterlocaties = c()
  store_referentielocaties = c()
  
  #Get monsterabiotiek and abiotische parameters
  monsterabiotiek = monsters[[1]]
  monsters_abpar = monsters[["Abiotics"]]
  
  #Get refentieabiotiek and abiotische parameters
  referentieabiotiek = referentie[[1]]
  referentie_abpar = referentie[["Abiotics"]]
  
  if(!(all.equal(monsters_abpar,referentie_abpar))){
    
    full_list_comp = list()
    fid = c(fid, paste("ERROR: Verschil in abiotische parameters tussen meetdata en referentie./n",
                       "Mogelijk zijn hier verschillende soortgroepen gebruikt./n",
                       " Meetdata : ",paste(monsters_abpar[1,],collapse = ","),"./n",
                       " Referentie : ",paste(referentie_abpar[1,],collapse = ","),"./n", sep = ""))
    fata_error = 1
    return(list("Data" = full_list_comp,"Comments" = fid, "Fatal_error" = fatal_error))
  }
  
  #Make monster vector
  for(nr1 in 1:length(monsterabiotiek)){
    # nr1 <- 1
    #Get monsterlokaties
    store_monsterlocaties = c(store_monsterlocaties,monsterabiotiek[[nr1]][["reftype"]])
  }
  
  #Make locaties vector
  for(nr2 in 1:length(referentieabiotiek)){
    # nr2 <- 1
    #Get referentielokaties
    store_referentielocaties = c(store_referentielocaties,referentieabiotiek[[nr2]][["reftype"]])
  }
  
  monloc_match = match(store_monsterlocaties,store_referentielocaties)
  refloc_match = match(store_referentielocaties,store_monsterlocaties)
  
  if(TRUE %in% is.na(monloc_match)){
    fid = c(fid,paste("WAARSCHUWING: de volgende monster locaties komen niet voor in de aangeleverde doelreferenties '",
                      paste(store_monsterlocaties[is.na(monloc_match)], sep = "','"),"'.",
                      " De locaties met deze naam worden verwijderd uit de analyse.", sep = ""))
    
  }
  if(TRUE %in% is.na(refloc_match)){
    fid = c(fid,paste("WAARSCHUWING: de volgende beschikbare refenties voor locaties komen niet voor in de meetdata '",
                      paste(store_referentielocaties[is.na(refloc_match)], sep = "','", collapse = "','"),"'.",
                      sep = ""))    
  }
  
  #Make complete doel per locatie list
  full_list = list()
  nr_full_list = 0
  for(nr3 in 1:length(monsterabiotiek)){
    
    #Check if locatiereferentie will be taken up
    match_value = monloc_match[nr3]
    
    if(!(is.na(match_value))){
      #add number
      nr_full_list = nr_full_list + 1
      
      #Get referentie
      referentie = referentieabiotiek[[match_value]]
      
      #Get locatie
      monster = monsterabiotiek[[nr3]]
      
      # print(referentie[["Statistic_results"]])
      
      #Combine in list
      temp_list = list()
      temp_list[["smpcode"]] = monster[["smpcode"]]
      temp_list[["locname"]] = monster[["locname"]]
      temp_list[["datesmp"]] = monster[["datesmp"]]
      temp_list[["locX"]] = monster[["locX"]]
      temp_list[["locY"]] = monster[["locY"]]
      temp_list[["reftype"]] = referentie[["reftype"]]
      temp_list[["sample_statistics"]] = monster[["Statistic_results"]]
      temp_list[["reference_statistics"]] = referentie[["Statistic_results"]]
      
      full_list[[nr_full_list]] <- temp_list
    }
  }
  if(length(full_list) == 0){
    fid = c(fid,paste("ERROR: geen van de meetwaarden lokaties en referentie lokaties",
                      "lijken met elkaar te corresponderen. Controleer deze.", sep = ""))
    fatal_error  = 1
    full_list_comp = list()
  }else{
    full_list_comp = list("Sample_data" = full_list,"Abiotics" = monsters_abpar)
  }
  return(list("Data" = full_list_comp,"Comments" = fid,"Fatal_error" = fatal_error))
}



BerekenAbiotiek <- function(monsters,type_data,path_databases){
  # Deze functie is zowel toepasbaar voor de gemeten abiotiek (monster)
  # als voor de gewenste abiotiek (referentie).
  # De Abiotiek waarden worden op basis van soortnamen en het type_data
  # ontrokken vanuit de bijhorende database (Macrofyten, Macrofauna, Vissen, Diatomeeen).
  # Het monster of de doelsoorten dient gecontroleerd te zijn met ControleMonsterData() of ControleDoelsoortData()
  # voordat de data wordt toegeleverd aan deze functie.
  
  ## debug
  # monsters <- vertaallist_samp[[1]]
  # monsters <- controlelist_doel[[1]]
  # type_data <- soortgroep
  # path_databases <- "data\\"
  # type_data <- "Macrofauna_V2"
  #
  
  mapping_file <- paste(path_databases,"AqMaD_Indeling-parameters-in-groepen.csv", sep = "")
  df_mapping_file <- read.table(mapping_file, header = T, sep = ";")
  
  fid = c()
  fatal_error = 0
  
  #Get database name
  if(type_data == "Macrofyten"){
    database_file = paste(path_databases,"AqMaD plantkenmerken.csv", sep = "")
    mapping = unique(df_mapping_file[df_mapping_file$soortgroep == type_data, c("kolomnaam", "parameter_koppelnaam")])
    Nrheaders = 1
    name_colspecnames = "Alle.Soorten"
    new_colnames = c("type", as.character(mapping$kolomnaam))
    abiotic_columns = c("type", as.character(mapping$parameter_koppelnaam))
    # new_colnames = c("type","Temperatuur","Zuurgraad","EGV","Doorzicht","Diepte","Bodemzicht","Zuurstof",
    #                 "ZuurstofPerc","BOD5","COD","TP","orthoP","TN",
    #                 "KjelN","NH3","NH4","NO2","NO3",
    #                 "ChlorofylA","Pheo","Kalium","Calcium","Ijzer","Magnesium","Natrium","Chloride",
    #                 "Sulfaat","Bicarbonaat","Zink","SS","Saliniteit")
    # abiotic_columns = strsplit(paste("type;T (oC);pH;EGV;Doorzicht (m);Diepte (m);Bodemzicht ;O2 (mg/l);",
    #                        "O2 (%);BZV (mg/l);CZV (mg/l);",
    #                        "P-tot (mg/l);o-P (mg/l);N-tot (mg.N/l);N-Kjel (mg.N/l);NH3 (mg N/l);",
    #                        "NH4 (mg N/l);NO2- (mg N/l);NO3- (mg N/l);Chlor-a (ug/l);Phaeo (ug/l);K (mg/l);Ca (mg/l);",
    #                        "Fe (mg/l);Mg (mg/l);Na (mg/l);Cl (mg/l);SO4 (mg/l);HCO3- (mg/l);Zn (ug/l);Zwev stof (mg/l);",
    #                        "Saliniteit (ppt)",sep = ""),split = ";")
  }else if(type_data == "Diatomeeen"){
    database_file = paste(path_databases,"AqMaD_Diatomeeen_v1_1_brondata.csv", sep = "")
    mapping = unique(df_mapping_file[df_mapping_file$soortgroep == type_data, c("kolomnaam", "parameter_koppelnaam")])
    Nrheaders = 5
    name_colspecnames = "__Taxon_Kolomnummer_Eenheid"
    new_colnames = c(as.character(mapping$kolomnaam))
    abiotic_columns = c(as.character(mapping$parameter_koppelnaam))   
    
    #new_colnames = c("Diepte","Temperatuur","Doorzicht","SS","ChlorofylA","Zuurgraad","EC","IR","Calcium","Kalium","Chloride",
    #                  "Sulfaat","TP","orthoP","TN","OrgN","KjelN","NH3","NH4","NO2","NO3","sNO2NO3","NPratio","ZuurstofPerc","BOD5")
    #abiotic_columns = strsplit(paste("Variabelen_Gemiddelden_Diepte_7_m;X__Temperatuur_8_oC;X__Doorzicht_9_m;X__ZwevendStof_10_mg.l;",
    #                                 "X__Chlorofyl_11_ug.l;X__pH_12_.;X__EC_13_mS.m;X__IR_14_.;X__CA_15_mg.l;X__K_16_mg.l;X__Cl_17_mg.l;",
    #                                 "X__SO4_18_mg.l;X__P.totaal_19_ugP.l;X__Ortho.P_20_ugP.l;X__N.totaal_21_mgN.l;X__OrgN_22_mgN.l;",
    #                                 "X__KjeldahlN_23_mgN.l;X__Ammoniak_24_mgN.l;X__Ammonium_25_mgN.l;X__NO2_26_mgN.l;X__NO3_27_mgN.l;",
    #                                 "X__NO2.3_28_mgN.l;X__N.P_29_.;X__O2._30_.;X__BOD5_31_mg.l",sep = ""),split = ";")
  }else if(type_data == "Vissen"){
    database_file = paste(path_databases,"AqMaD_Vissen_v1_1_brondata_adult.csv", sep = "")
    mapping = unique(df_mapping_file[df_mapping_file$soortgroep == type_data, c("kolomnaam", "parameter_koppelnaam")])
    Nrheaders = 6
    name_colspecnames = "_Wetenschappelijke.naam___3_"
    new_colnames = c(as.character(mapping$kolomnaam))
    abiotic_columns = c(as.character(mapping$parameter_koppelnaam))
    #new_colnames = c("Temperatuur","Stroomsnelheid","Diepte","pH")
    #abiotic_columns = strsplit(paste("_Temperatuur.gewogen.gemiddeld_Temperatuur__15_graden.C;__Stroomsnelheid__22_m.s;__Diepte__28_m;",
    #                                 "_pH_pH__57_-",sep = ""),split = ";")
    
  }else if(type_data == "Macrofauna"){
    database_file = paste(path_databases,"AqMaD_Macrofauna_v1_1_brondata.csv", sep = "")
    mapping = unique(df_mapping_file[df_mapping_file$soortgroep == type_data, c("kolomnaam", "parameter_koppelnaam")])
    Nrheaders = 5
    name_colspecnames = "taxonnaam___Kolomnummer_Eenheid"
    new_colnames = c(as.character(mapping$kolomnaam))
    abiotic_columns = c(as.character(mapping$parameter_koppelnaam))
    #new_colnames = c("Chloride","Diepte","Droogval","Oppervlak","Isolatie","Saprobie","Stroming",
    #                 "GrofhAnorgSubstr","GrofhOrgSubstr","Waterplanten","Trofie","pH")
    #abiotic_columns = strsplit(paste("X__Chloride_7_mg.l;X_Score_Diepte_19_index;X_Score__26_index;X__Oppervlak_42_index;",
    #                                 "X__Isolatie_46_index;X_Saprobie_Saprobie_52_index;X_Stroming_Stroming_59_cm.s;",
    #                                 "Substraat2_Grofheid.anorg.substraat_Grofh.Anorg.Substr._72_index;",
    #                                 "X_Grofheid.Org.substraat_Grofh.Org.Substr._74_index;X_Waterplanten_Waterplanten_76_index;",
    #                                 "X_Score_Trofie_83_index;X_pH_pH_88_.",sep = ""),split = ";")
    
  }else if(type_data == "Macrofauna_V2"){
    #In this method we try to apply the rules described in Verdonschot 2011 and Verberk 2012
    # This method is quite different from the rest of the calculations and there fore has been included in a seperate
    # function. However to show these results the figures will need to be updated in the AqMaD rShiny application
    data_overview = BerekenAbiotiek_Macrofauna(monsters,soortgroep, fid,fatal_error,df_mapping_file,path_databases)
    data_mafa = data_overview[[1]]
    fid = data_overview[[2]]
    fatal_error = data_overview[[3]]
    return(list("Data" = data_mafa,"Comments" = fid,"Fatal_error" =  fatal_error))
    
  }else{
    fid = c(fid,paste("Error : onbekende soortgroep",as.character(type_data),sep =""))
    fatal_error = 1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  #Read database of type_data
  database_list = inlezenExcelOfCsv(database_file)
  database = database_list[[1]]
  
  #Create result list
  results <- list()
  
  #Get columns
  columns_naming = apply(database[1:Nrheaders , ], 2, function(x) paste(x, sep = "", collapse = "_"))
  columns_naming_cor = as.vector(sapply(columns_naming, FUN = make.names))
  
  #Make abiotic columns
  abiotic_columns = as.vector(unlist(abiotic_columns))
  
  #Make sure abiotic columns match with columns_naming_cor
  abiotic_columns = gsub("%|-|/", ".", abiotic_columns)
  #Add X infront of string that starts with "_"
  for(nr in 1:length(abiotic_columns)){
    if(substring(abiotic_columns[nr], 1, 1)[[1]] == "_"){
      abiotic_columns[nr] = paste("X",abiotic_columns[nr], sep ="")
    }
  }
  
  #Make species column
  name_colspecnames_cor = make.names(name_colspecnames)
  
  #Set database names
  colnames(database) <- columns_naming_cor
  
  #Test if abiotic columns are equal
  if(length(new_colnames) != length(abiotic_columns)){
    fid = c(fid,paste("ERROR: Database kolommen en vertaling kolommen kloppen niet in het script!",
                      " Kijk naar het aantal kolommen. Vertaling = ",as.character(length(new_colnames)),
                      " , Database = ",as.character(length(abiotic_columns))," .",sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  
  #Start processing the samples
  colspecnames_temp = which(columns_naming_cor == name_colspecnames_cor)
  if(length(colspecnames_temp) == 0){
    fid = c(fid,paste("ERROR: Database kolommen kloppen niet met het script!",
                      " Kijk naar de taxon kolom : ",name_colspecnames,
                      sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  #get first element
  colspecnames = colspecnames_temp[[1]]
  
  #Get abiotic columns
  columns_index = match(abiotic_columns,columns_naming_cor)
  
  #Check if all matched
  if(TRUE %in% is.na(columns_index)){
    fid = c(fid,paste("ERROR: Database kolommen kloppen niet met het script!",
                      "Kijk naar de kolommen : ",abiotic_columns[is.na(columns_index)],
                      sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  fid
  #Get the required data from the database and make it numeric
  database_abiotics = data.frame(database[,c(colspecnames,columns_index)],stringsAsFactors = FALSE)
  
  #If aantal is added to the abiotic data than change colnames
  if(type_data %in% c("Diatomeeen","Macrofauna")){
    #Add aantal to columnnames
    new_colnames = c("aantal",new_colnames)
    abiotic_columns = c("aantal",abiotic_columns)
  }
  
  for(nr in (1:length(monsters))){
    species = as.character(monsters[[nr]][["specname"]])
    
    #Test if species correctly named
    correct_names = species[species %in% database[,colspecnames]]
    false_names = species[!(species %in% database[,colspecnames])]
    
    #Make link to database lines
    species_index = match(correct_names,database[,colspecnames])
    
    #Get species data
    #Depending on whether the soortgroep is Diatomeen and Macrofauna or other
    # preform the calculation differently
    if(type_data %in% c("Diatomeeen","Macrofauna")){
      sec_set_abiotic_columns = abiotic_columns[!(abiotic_columns %in% "aantal")]
      
      if(TRUE %in% !(is.na(species_index))){
        #if the type_data is Diatomeeen of Macrofauna, the indicator per
        #species is scaled to the number of that species found
        
        TEMP_species_abiotics = database_abiotics[species_index,]

        #Make into values
        nr_columns = ncol(TEMP_species_abiotics)
        TEMP_species_abiotics[,(2:nr_columns)] =  lapply(TEMP_species_abiotics[,(2:nr_columns)],
                                                         function(x){if(is.factor(x)){as.numeric(as.character(x))
                                                         }else{as.numeric(x)}})
        
        # if("waarde" %in% names(monsters[[nr]])){
        #Get waarde van correct_species
        correct_waarde = monsters[[nr]][["waarde"]][species %in% database[,colspecnames]]
        
        #add to overview dataframe
        species_abiotics_overview = cbind(TEMP_species_abiotics[,colspecnames],
                                          "aantal" = correct_waarde,
                                          TEMP_species_abiotics[, sec_set_abiotic_columns])
        species_abiotics_overview[species_abiotics_overview == ""]<-NA
        
        #add to data frame with individuals
        row_rep = c()
        spec_names_rep = c()
        for(i in 1:nrow(TEMP_species_abiotics)){
          row_rep = c(row_rep,rep(i,correct_waarde[i]))
          spec_names_rep = c(spec_names_rep,rep(TEMP_species_abiotics[,colspecnames][i],correct_waarde[i]))
        }
        species_abiotics = cbind(spec_names_rep,TEMP_species_abiotics[row_rep,sec_set_abiotic_columns])
        species_abiotics[species_abiotics == ""]<-NA
        
      }else{
        fid = c(fid,paste("WAARSCHUWING: Voor monster ",nr," zijn geen passende",
                          " soorten in de database gevonden.", sep = ""))
        species_abiotics_overview = cbind(database_abiotics[0,colspecnames], "aantal" = character(0),
                                          database_abiotics[0,sec_set_abiotic_columns])
        species_abiotics = cbind(database_abiotics[0,colspecnames],
                                 database_abiotics[0,sec_set_abiotic_columns])
      }
    }else{
      #if there is a different type_data than Diatomeen or Macrofauna
      #the indicator per species is used without taking into
      #account the number of species found.
      if(TRUE %in% !(is.na(species_index))){
        species_abiotics_overview = database_abiotics[species_index,]
        species_abiotics_overview[species_abiotics_overview == ""]<-NA
        
        species_abiotics = database_abiotics[species_index,]
        species_abiotics[species_abiotics == ""]<-NA
      }else{
        fid = c(fid,paste("WAARSCHUWING: Voor monster ",nr," zijn geen passende",
                          " soorten in de database gevonden.", sep = ""))
        species_abiotics_overview = database_abiotics[0,]
        species_abiotics = database_abiotics[0,]
      }
    }
    #Rename the columns
    colnames(species_abiotics_overview) <- c("species_name",new_colnames)
    row.names(species_abiotics_overview) <- NULL
    
    abiotic_colnames = new_colnames[!(new_colnames %in% "aantal")]
    colnames(species_abiotics) <- c("species_name",abiotic_colnames)
    row.names(species_abiotics) <- NULL
    
    #Fill list
    monsters[[nr]][["correct_species_names"]] = correct_names
    monsters[[nr]][["incorrect_species_names"]] = false_names
    monsters[[nr]][["Abiotic_results"]] = as.data.frame(species_abiotics,stringsAsFactors = FALSE)
    monsters[[nr]][["Abiotic_results_overview"]] = as.data.frame(species_abiotics_overview,stringsAsFactors = FALSE)
    
    #Make a warning is some species not present
    if(length(false_names)>0){
      fid = c(fid,paste("WAARSCHUWING: Voor monster ",nr," zijn enkele ",
                        "soorten niet in de database terug gevonden : '",paste(false_names, collapse = "','",sep = ""),
                        "' .",sep = ""))
    }
  }
  
  abiotics_overview = rbind(abr_name = new_colnames,full_name = abiotic_columns,
                            cor_name = abiotic_columns)
  fullsample = list("Sample_data" = monsters,"Abiotics" = abiotics_overview)
  
  return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
}
  
BerekenAbiotiek_Macrofauna <- function(monsters,soortgroep, fid,fatal_error,df_mapping_file,path_databases){
  
  ## debug
  # monsters <- vertaallist_samp[[1]]
  # monsters <- controlelist_doel[[1]]
  # type_data <- soortgroep
  # path_databases <- "data\\"
  # type_data <- "Macrofauna_V2"
  # mapping_file <- paste(path_databases,"AqMaD_Indeling-parameters-in-groepen.csv", sep = "")
  # df_mapping_file <- read.table(mapping_file, header = T, sep = ";")
  # fid = c()
  # fatal_error = 0
  
  #FOR Z-value calculation
  database_file = paste(path_databases,"AqMaD_Macrofauna_v1_1_brondata.csv", sep = "")
  Nrheaders = 5
  name_colspecnames = "taxonnaam___Kolomnummer_Eenheid"
  new_colnames1 = c("Chloride","Chloride","Chloride","Chloride","Chloride",
                    "Diepte","Diepte","Diepte",
                    "Droogval","Droogval","Droogval","Droogval","Droogval",
                    "Oppervlakte","Oppervlakte","Oppervlakte","Oppervlakte",
                    "Isolatie","Isolatie",
                    "Saprobie","Saprobie","Saprobie","Saprobie",
                    "Stroming","Stroming","Stroming","Stroming","Stroming",
                    "Substraat_Anorganisch","Substraat_Anorganisch","Substraat_Anorganisch","Substraat_Anorganisch","Substraat_Anorganisch",
                    "Substraat_Organisch","Substraat_Organisch","Substraat_Organisch",
                    "Substraat_Waterplanten","Substraat_Overig",
                    "Trofie","Trofie","Trofie","Trofie","Trofie",
                    "Zuurgraad","Zuurgraad","Zuurgraad","Zuurgraad")
  new_colnames2 = c("Chloride_<300","Chloride_300-1000","Chloride_1000-3000","Chloride_3000-10000","Chloride_>10000",
                    "Diepte_Zeer_ondiep","Diepte_Ondiep","Diepte_Diep",
                    "Droogval_niet_droogvallend","Droogval_<6wk","Droogval_6wk-3mnd","Droogval_3-5mnd","Droogval_>5mnd",
                    "Oppervlak_Zeer_klein","Oppervlakte_Klein","Oppervlakte_Middelgroot","Oppervlakte_Groot",
                    "Isolatie_Geisoleerd","Isolatie_Open",
                    "Saprobie_Oligosaprobie","Saprobie_A-mesosaproob","Saprobie_B-mesosaproob","Saprobie_Polysaproob",
                    "Stroming_Stilstaand_<5cms","Stroming_Zeer_langzaam_5-9cms","Stroming_Langzaam_10-15cms","Stroming_Matig_16-25cms","Stroming_Snel_>25cms",
                    "Substraat_Slib","Substraat_klei&leem","Substraat_Zand","Substraat_Grind","Substraat_Stenen","Substraat_Fijn_detritus","Substraat_Grof_detritus","Substraat_Hout",
                    "Substraat_Waterplanten","Substraat_overig",
                    "Trofie_I","Trofie_II","Trofie_III","Trofie_IV","Trofie_V",
                    "Zuurgraad_Zuur","Zuurgraad_Zwak_zuur","Zuurgraad_Neutraal","Zuurgraad_Basisch")
  
  abiotic_columns = strsplit(paste("Chloride_.300_150_2_;X_300.1000_650_3_;X_1000.3000_2000_4_;X_3000.10000_6500_5_;X_.10000_15000_6_;",
                                   # nog niet erin  "Diepte_.zeer..ondiep..moerassig.__9_;X_ondiep.stilstaand__10_;X_diep.stilstaand__11_;X_indifferent..diepte.__12_;",
                                   # nog niet erin  "X_zeer.ondiep..bron.__13_;X_ondiep.stromend__14_;X_diep.stromend__15_;",
                                   "Diepte2_Zeer.ondiep__16_;X_Ondiep__17_;X_Diep__18_;",
                                   "Droogval_niet.droogvallend__21_;X_korter.dan.6.wk__22_;X_6.wk...3mnd__23_;X_3.5.mnd__24_;X_langer.dan.5.mnd__25_;",
                                   # nog niet erin  "Oppervlak_.zeer..klein..opp.__28_;X_.zeer..klein..opp..geisoleerd__29_;X_.zeer..klein..opp..open__30_;X_klein.geisoleerd__31_;X_klein.open__32_;",
                                   # nog niet erin  "X_middelgroot__33_;X_middelgroot.open__34_;X_groot__35_;X_groot.open__36_;X_indifferent..opp.__37_;",
                                   "X_Oppervlak_Zeer.klein_38_;X__Klein_39_;X__Middelgroot_40_;X__Groot_41_;",
                                   "X__Geisoleerd_44_;X__Open_45_;",
                                   "Saprobie_Oligosaproob__48_;X_A.mesosaproob__49_;X_B.mesosaproob__50_;X_Polysaproob__51_;",
                                   "Stroming_Stilstaand..5.cm.s_2.5_54_;X_Zeer.langzaam.stromend.5.9.cm.s_7_55_;X_Langzaam.stromend.10.15.cm.s_12.5_56_;X_Matig.stromend.16.25.cm.s_20.5_57_;",
                                   "X_Snel.stromend..25.cm.s_37.5_58_;",
                                   "Substraat_slib__61_;X_klei...leem__62_;X_zand..fijn...grof.__63_;X_grind..fijn..matig...grof.__64_;X_stenen__65_;X_fijne.detritus__66_;X_grove.detritus__67_;",
                                   "X_hout__68_;X_waterplanten__69_;X_overig__71_;",
                                   "Trofie_I__78_;X_II__79_;X_III__80_;X_IV__81_;X_V__82_;",
                                   "Zuurgraad_zuur_4_85_;X_zwak.zuur_5.5_86_;X_neutraal_7_87_;X_basisch_8.5_88_",sep = ""),split = ";")
  factor_name    = c("Chloride","Diepte","Droogval","Oppervlakte","Isolatie",
                     "Saprobie","Stroming","Substraat_Anorganisch","Substraat_Organisch","Substraat_Waterplanten","Trofie",
                     "Zuurgraad")
  factor_columns = strsplit(paste("X__Weegfactor_8_;X_weegfactor__20_;X_weegfactor__20_;X__Weegfactor_27_;X__Weegfactor_43_;",
                                  "X__Weegfactor_47_;X__Weegfactor_53_;X__Weegfactor_60_;X_Weegfactor__73_;X_Weegfactor__75_;X_Weegfactor__77_;",
                                  "X__Weegfactor_84_;X__Weegfactor_90_",sep = ""),split = ";")
  
  applicatibility_columns = strsplit(paste("X__ADiepte__;X__A__;X__AOppervlak__;X__AIsolatie__;X__ASaprobie__;X__AStroming__;",
                                           "X__AGrofh.Anorg.Substr.__;X__AGrofh.Org.Substr.__;X__AWaterplanten__;X__ATrofie__;X__ApH__",
                                           sep = ""),split = ";")
  
  #FOR Abiotic values calculation
  mapping = unique(df_mapping_file[df_mapping_file$soortgroep == "Macrofauna", c("kolomnaam", "parameter_koppelnaam")])
  name_colspecnames_mapping = "taxonnaam___Kolomnummer_Eenheid"
  new_colnames_mapping = c(as.character(mapping$kolomnaam))
  abiotic_columns_mapping = c(as.character(mapping$parameter_koppelnaam))
  
  #Read database of type_data
  database_list = inlezenExcelOfCsv(database_file)
  database = database_list[[1]]
  
  #Create result list
  results <- list()
  
  #Get columns
  columns_naming = apply(database[1:Nrheaders , ], 2, function(x) paste(x, sep = "", collapse = "_"))
  columns_naming_cor = as.vector(sapply(columns_naming, FUN = make.names))
  
  ##FOR ABIOTIC OVERVIEW
  
  #Make abiotic columns
  abiotic_columns = as.vector(unlist(abiotic_columns))
  
  #Make sure abiotic columns match with columns_naming_cor
  abiotic_columns = gsub("%|-|/", ".", abiotic_columns)
  #Add X infront of string that starts with "_"
  for(nr in 1:length(abiotic_columns)){
    if(substring(abiotic_columns[nr], 1, 1)[[1]] == "_"){
      abiotic_columns[nr] = paste("X",abiotic_columns[nr], sep ="")
    }
  }

  factor_columns = as.vector(unlist(factor_columns))
  
  #Make species column
  name_colspecnames_cor = make.names(name_colspecnames)
  
  ##FOR ABIOTIC RESULTS
  #Make abiotic columns mapping
  abiotic_columns_mapping = as.vector(unlist(abiotic_columns_mapping))
  
  #Make sure abiotic columns match with columns_naming_cor
  abiotic_columns_mapping = gsub("%|-|/", ".", abiotic_columns_mapping)
  #Add X infront of string that starts with "_"
  for(nr in 1:length(abiotic_columns_mapping)){
    if(substring(abiotic_columns_mapping[nr], 1, 1)[[1]] == "_"){
      abiotic_columns_mapping[nr] = paste("X",abiotic_columns_mapping[nr], sep ="")
    }
  }
  
  
  ## SET AND TEST DATABASE NAMES
  colnames(database) <- columns_naming_cor
  
  #Test if abiotic columns are equal
  if(length(new_colnames1) != length(abiotic_columns)){
    fid = c(fid,paste("ERROR: Database kolommen en vertaling kolommen kloppen niet in het script!",
                      " Kijk naar het aantal kolommen. Vertaling = ",as.character(length(new_colnames1)),
                      " , Database = ",as.character(length(abiotic_columns))," .",sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  if(length(new_colnames_mapping) != length(abiotic_columns_mapping)){
    fid = c(fid,paste("ERROR: Database kolommen en vertaling kolommen kloppen niet in het script!",
                      " Kijk naar het aantal kolommen. Vertaling = ",as.character(length(new_colnames)),
                      " , Database = ",as.character(length(abiotic_columns))," .",sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  ##START PROCESSING THE SAMPLES
  colspecnames_temp = which(columns_naming_cor == name_colspecnames_cor)
  if(length(colspecnames_temp) == 0){
    fid = c(fid,paste("ERROR: Database kolommen kloppen niet met het script!",
                      " Kijk naar de taxon kolom : ",name_colspecnames,
                      sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  #get first element
  colspecnames = colspecnames_temp[[1]]
  
  #Get abiotic columns
  columns_index = match(abiotic_columns,columns_naming_cor)
  columns_index_mapping = match(abiotic_columns_mapping,columns_naming_cor)
  
  #Check if all matched
  if(TRUE %in% is.na(columns_index)){
    fid = c(fid,paste("ERROR: Database kolommen kloppen niet met het script!",
                      "Kijk naar de kolommen : ",abiotic_columns[is.na(columns_index)],
                      sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  #Check if all matched
  if(TRUE %in% is.na(columns_index_mapping)){
    fid = c(fid,paste("ERROR: Database kolommen kloppen niet met het script!",
                      "Kijk naar de kolommen : ",abiotic_columns_mapping[is.na(columns_index_mapping)],
                      sep = ""))
    fullsample = list()
    fatal_error =  1
    return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
  }
  
  #split column names to get first group for abiotic overview
  new_colnames1_split_dim = as.vector(lapply(strsplit(new_colnames1,"_"), FUN=length))
  new_colnames1_split_first = sapply(strsplit(new_colnames1,"_"),FUN = function(x){x[1]})
    
  #Get the required data from the database and make it numeric for the abiotic and abiotic overview results
  database_abiotics = data.frame(database[,c(colspecnames,columns_index_mapping)],stringsAsFactors = FALSE) 
  columns_index = match(c(abiotic_columns,factor_columns),columns_naming_cor)
  database_abiotics_overview = data.frame(database[,c(colspecnames,columns_index)],stringsAsFactors = FALSE)

  #Set new_colnames
  new_colnames = new_colnames2

  for(nr in (1:length(monsters))){
    species = as.character(monsters[[nr]][["specname"]])
    
    #Test if species correctly named
    correct_names = species[species %in% database[,colspecnames]]
    false_names = species[!(species %in% database[,colspecnames])]
    
    #Make link to database lines
    species_index = match(correct_names,database[,colspecnames])

    #Get species data
    #Depending on whether the soortgroep is Diatomeen and Macrofauna or other
    # preform the calculation differently
    sec_set_abiotic_columns = abiotic_columns_mapping[!(abiotic_columns_mapping %in% "aantal")]
    sec_set_abiotic_overview_columns = abiotic_columns[!(abiotic_columns %in% "aantal")]
      
    if(TRUE %in% !(is.na(species_index))){
      #if the type_data is Diatomeeen of Macrofauna, the indicator per
      #species is scaled to the number of that species found
        
      TEMP_species_abiotics = database_abiotics[species_index,]
      TEMP_species_abiotics_overview = database_abiotics_overview[species_index,]
        
      #Make into values
      nr_columns = ncol(TEMP_species_abiotics)
      TEMP_species_abiotics[,(2:nr_columns)] =  lapply(TEMP_species_abiotics[,(2:nr_columns)],
                                                       function(x){if(is.factor(x)){as.numeric(as.character(x))
                                                       }else{as.numeric(x)}})
        
      nr_columns_overview = ncol(TEMP_species_abiotics_overview)
      TEMP_species_abiotics_overview[,(2:nr_columns_overview)] =  lapply(TEMP_species_abiotics_overview[,(2:nr_columns_overview)],
                                                                         function(x){if(is.factor(x)){as.numeric(as.character(x))
                                                                         }else{as.numeric(x)}})
        
      #Get waarde van correct_species
      correct_waarde = monsters[[nr]][["waarde"]][species %in% database[,colspecnames]]
        
      #Prepare data for abiotic overview
          
      #Get abiotic columns
      species_abiotics_temp = TEMP_species_abiotics_overview[,sec_set_abiotic_overview_columns]
          
      #Get weegfactoren
      species_weegfactoren = TEMP_species_abiotics_overview[,factor_columns]
          
      #Get specie individuals log
      specie_log = sapply(correct_waarde, FUN = function(x){log2(x + 1)})
          
      #Prepare data for abiotic results
      row_rep = c()
      spec_names_rep = c()
      for(i in 1:nrow(TEMP_species_abiotics)){
        row_rep = c(row_rep,rep(i,correct_waarde[i]))
        spec_names_rep = c(spec_names_rep,rep(TEMP_species_abiotics[,colspecnames][i],correct_waarde[i]))
      }
      species_abiotics = cbind(spec_names_rep,TEMP_species_abiotics[row_rep,sec_set_abiotic_columns])

      #Start loop per column for abiotic overview 
      tot_Set = 0
      
      for(split_name in unique(new_colnames1_split_first)){
        
        temp_Set = 0
        
        dim_parameter = new_colnames1_split_dim[new_colnames1_split_first %in% split_name][1]
        sublevels = unique(new_colnames1[new_colnames1_split_first %in% split_name])
        
        #for if there are sublevels
        for(sublevel in sublevels){
          
          #Get abiotic value
          col_req = sec_set_abiotic_overview_columns[new_colnames1 %in% sublevel]
          spec_val = species_abiotics_temp[,col_req,drop=FALSE]
          
          factor_columns
          if(sublevel %in% factor_name){
            #Get specific weegfactor
            weegfactor_colnam = factor_columns[unique(new_colnames1) %in% sublevel]
            weegfactor_spec = species_weegfactoren[,weegfactor_colnam]
          }else{
            #Generate a weegfactor of 1
            weegfactor_spec = rep(1,length(species_weegfactoren[,1]))
          }
          
          #Make abiotics
          temp_abdata = data.frame(spec_val * as.vector(weegfactor_spec))
          
          #Multiply
          temp_data = t(data.frame(colSums(temp_abdata * specie_log, na.rm = TRUE)))
          rownames(temp_data)<-1
          
          if(identical(temp_Set,0)){
            temp_Set = temp_data
          }else{
            temp_Set = cbind(temp_Set,temp_data)
          }
        }
        
        corr_Set = (temp_Set / sum(temp_Set))*10
        
        if(identical(tot_Set,0)){
          tot_Set = corr_Set
        }else{
          tot_Set = cbind(tot_Set,corr_Set)
        }
      }
      
      #Clean up data
      species_abiotics_overview = cbind("overzicht" = c("Behoefte"),tot_Set)
      species_abiotics_overview[species_abiotics_overview == ""]<-NA
      
      species_abiotics[species_abiotics == ""]<-NA
      
      }else{
        fid = c(fid,paste("WAARSCHUWING: voor monster ",nr," zijn geen passende",
                          " soorten in de database gevonden.", sep = ""))
        species_abiotics_overview = cbind(database_abiotics[0,colspecnames], "aantal" = character(0),
                                          database_abiotics[0,sec_set_abiotic_overview_columns])
        species_abiotics = cbind(database_abiotics[0,colspecnames],
                                 database_abiotics[0,sec_set_abiotic_columns])
      }

    #Rename the columns
    colnames(species_abiotics_overview) <- c("species_name",new_colnames)
    row.names(species_abiotics_overview) <- NULL
    
    abiotic_colnames_mapping = new_colnames_mapping[!(new_colnames_mapping %in% "aantal")]
    colnames(species_abiotics) <- c("species_name",abiotic_colnames_mapping)
    row.names(species_abiotics) <- NULL
    
    #Fill list
    monsters[[nr]][["correct_species_names"]] = unique(correct_names)
    monsters[[nr]][["incorrect_species_names"]] = unique(false_names)
    monsters[[nr]][["Abiotic_results"]] = as.data.frame(species_abiotics,stringsAsFactors = FALSE)
    monsters[[nr]][["Abiotic_results_overview"]] = as.data.frame(species_abiotics_overview,stringsAsFactors = FALSE)
    
    #Make a warning is some species not present
    if(length(monsters[[nr]][["incorrect_species_names"]])>0){
      fid = c(fid,paste("WAARSCHUWING: voor monster ",nr," zijn enkele ",
                        "soorten niet in de database terug gevonden : '",paste(monsters[[nr]][["incorrect_species_names"]], collapse = "', '", sep = ""),
                        "' .",sep = ""))
    }
  }
  
  abiotics_overview = rbind(abr_name = new_colnames,full_name = abiotic_columns,
                            cor_name = abiotic_columns)
  fullsample = list("Sample_data" = monsters,"Abiotics" = abiotics_overview)
  return(list("Data" = fullsample,"Comments" = fid,"Fatal_error" = fatal_error))
}
  
  
  
BerekenenStatistiekWaarden <- function(data, soortgroep){
  # Deze functie is zowel toepasbaar voor de gemeten abiotiek (monster) 
  # als voor de gewenste abiotiek (referentie).
  # Bereken het gemiddelde, de standaarddeviatie, minimum, 
  # 25 percentile,median, 75 percentile and Maximum vanuit de Abiotiek resultaten.
  #Hiervoor dient eerst de functie BerekenAbiotiek()  
  # gebruikt te worden.

  fid = c()
  fatal_error = 0
  
  #De berekening voor Macrofauna_V2 is dermate anders dat deze in een appartte functie wordt uitgevoerd.
  if(soortgroep == "Macrofauna_V2"){
    data_overview = BerekenStatistiekWaarden_Macrofauna(data, soortgroep, fid,fatal_error)
    data_mafa = data_overview[[1]]
    fid = data_overview[[2]]
    fatal_error = data_overview[[3]]
    return(list("Data" = data_mafa,"Comments" = fid,"Fatal_error" =  fatal_error))
  }
  
  #MAAK EEN OPGECHOONDE DATASET VAN DE SAMPLES  
  for(nr in 1:length(data[[1]])){
    
    #Get Abiotiek from sample
    abiotiek = data[[1]][[nr]][["Abiotic_results"]]

    #See if it concerns macrofytes
    if("type" %in% colnames(abiotiek)){
      if(soortgroep == "Macrofyten"){
        
        #Split dataframes on type
        type_list = split(abiotiek,f = abiotiek$type)
        
        #Fill type list if none existend
        if(FALSE %in% (c("1","2","3") %in% type_list)){
          for(i in c("1","2","3")){
            if(!(i %in% names(type_list))){
              type_list[[i]] = abiotiek[NULL,]
            }
          }
        }
        
        #columnames required
        col_req = colnames(abiotiek)[!(colnames(abiotiek) %in% c("species_name","type"))]
        
        #Make dataframe waterplanten        
        waterplanten_ab = rbind(type_list[["1"]][,col_req],type_list[["3"]][,col_req])
              
        #Make dataframe oeverplanten
        oeverplanten_ab = rbind(type_list[["2"]][,col_req],type_list[["3"]][,col_req])

        #Make into one list
        data_list = list("waterplanten" = waterplanten_ab,"oeverplanten" = oeverplanten_ab)

      }else{
        fid = c(fid,paste("ERROR: 'type' is nog niet beschikbaar voor andere soortgroepen dan macrofyten."))
        fatal_error = 1
        return(list("Data" = list(),"Comments" = fid,"Fatal_error" =  fatal_error))
      }
    }else{
      #columnames required
      col_req = colnames(abiotiek)[!(colnames(abiotiek) %in% c("species_name"))]
      
      #Make into one list
      data_list = list()
      data_list[[soortgroep]] = abiotiek[,col_req]
      
    }
    #Loop over Statistics calculation
    temp_list = list()
    statistics = c("N","gemiddelde","standaarddeviatie","minimum","25%","mediaan","75%","maximum")
    
    #Remove aantal from columns required
    col_req = col_req[!(col_req %in% c("aantal"))]
    
    #BEREKEN STATISTIEK PER SAMPLE
    for(nr2 in 1:length(data_list)){
      
      #get required columns and make numeric
      if(length(data_list[[nr2]][,1]) > 1){
        data_list_data = as.data.frame(sapply(data_list[[nr2]][,col_req], as.numeric),stringsAsFactors = FALSE)
      }else if(length(data_list[[nr2]][,1]) > 0){
        data_list_data = t(as.data.frame(sapply(data_list[[nr2]][,col_req], as.numeric),stringsAsFactors = FALSE))
      }else{
        data_list_data = data_list[[nr2]]
      }
      
      #calculate stats
      if(length(data_list_data[,1]) == 0){
        #no values
        N = rep(0,length(col_req))
        temp_mean = rep(NA,length(col_req))
        temp_sd = rep(NA,length(col_req))
        temp_min = rep(NA,length(col_req))
        temp_Quan25 = rep(NA,length(col_req))
        temp_median = rep(NA,length(col_req))
        temp_Quan75 = rep(NA,length(col_req))
        temp_max = rep(NA,length(col_req))
      }else if(length(data_list_data[,1]) == 1){
        #one value per parameter
        N = as.vector(apply(data_list_data,2,function(x) length(which(!is.na(x)))))
        temp_mean = data_list_data
        temp_sd = rep(NA,length(col_req))
        temp_min = rep(NA,length(col_req))
        temp_Quan25 = rep(NA,length(col_req))
        temp_median = rep(NA,length(col_req))
        temp_Quan75 = rep(NA,length(col_req))
        temp_max = rep(NA,length(col_req))
      }else if(length(data_list_data[,1]) <=3){
        #less or equal values as 3 per parameter
        N = as.vector(apply(data_list_data,2,function(x) length(which(!is.na(x)))))
        temp_mean = as.vector(apply(data_list_data,2, mean, na.rm=TRUE))
        temp_sd = rep(NA,length(col_req))
        temp_min = rep(NA,length(col_req))
        temp_Quan25 = rep(NA,length(col_req))
        temp_median = rep(NA,length(col_req))
        temp_Quan75 = rep(NA,length(col_req))
        temp_max = rep(NA,length(col_req))
      }else if(length(data_list_data[,1]) > 3){
        #more values then 3 per parameter
        N = as.vector(apply(data_list_data,2,function(x) length(which(!is.na(x)))))
        temp_mean =  as.vector(apply(data_list_data,2, mean, na.rm=TRUE))
        temp_sd =  as.vector(apply(data_list_data,2, sd, na.rm=TRUE))
        temp_min = as.vector(apply(data_list_data,2, min, na.rm=TRUE))
        temp_Quan25 = as.vector(apply(data_list_data,2, quantile, probs = c(0.25), na.rm=TRUE))
        temp_median = as.vector(apply(data_list_data,2,median, na.rm=TRUE))
        temp_Quan75 = as.vector(apply(data_list_data,2,quantile, probs = c(0.75), na.rm=TRUE))
        temp_max = as.vector(apply(data_list_data,2,max, na.rm=TRUE))
      }

      #make into dataframe      
      temp_comp = rbind(N,temp_mean,temp_sd,temp_min,temp_Quan25,temp_median,temp_Quan75,temp_max)
      temp_comp = cbind("Statistiek" = statistics, temp_comp)
      temp_comp = as.data.frame(temp_comp,stringsAsFactors = FALSE)
    
      colnames(temp_comp) <- c("Statistiek",col_req)
      row.names(temp_comp) <- NULL 
      
      #add to list
      temp_list[[names(data_list)[nr2]]] <- temp_comp
      
    }
    
    #Add results to complete list
    data[[1]][[nr]][["Statistic_results"]] = temp_list
    
  }
  return(list("Data" = data,"Comments" = fid,"Fatal_error" =  fatal_error))
}

BerekenStatistiekWaarden_Macrofauna <- function(data, soortgroep, fid, fatal_error){
  #De systematiek voor de macrofauna berekening is zeer verschillend van
  # de standaard Z-waarde berekening. Hierom hebben we dit opgenomen in
  # een apparte functie.
  #
  # data <- abiotieklist_samp[[1]]
  # soortgroep <- "Macrofauna_V2"
  # fid <- character(0)
  # fatal_error <- 0

  
  new_colnames1 = c("Chloride","Chloride","Chloride","Chloride","Chloride",
                    "Diepte","Diepte","Diepte",
                    "Droogval","Droogval","Droogval","Droogval","Droogval",
                    "Oppervlakte","Oppervlakte","Oppervlakte","Oppervlakte",
                    "Isolatie","Isolatie",
                    "Saprobie","Saprobie","Saprobie","Saprobie",
                    "Stroming","Stroming","Stroming","Stroming","Stroming",
                    "Substraat_Anorganisch","Substraat_Anorganisch","Substraat_Anorganisch","Substraat_Anorganisch","Substraat_Anorganisch",
                    "Substraat_Organisch","Substraat_Organisch","Substraat_Organisch",
                    "Substraat_Waterplanten","Substraat_Overig",
                    "Trofie","Trofie","Trofie","Trofie","Trofie",
                    "Zuurgraad","Zuurgraad","Zuurgraad","Zuurgraad")
  unique_colnames1 = unique(new_colnames1) 
  colnames_overall = as.vector(sapply(strsplit(new_colnames1,"_"),FUN = function(x){x[1]}))
  unique_colnames_overall = unique(colnames_overall)
  new_colnames1_score = c(1,2,3,4,5,
                         1,2,3,
                         1,2,3,4,5,
                         1,2,3,4,
                         1,2,
                         1,2,3,4,
                         1,2,3,4,5,
                         1,2,3,4,5,
                         1,2,3,
                         1,1,
                         1,2,3,4,5,
                         1,2,3,4)
 
  for(nr in 1:length(data[[1]])){
    
    # nr <- 1
    
    #Get Overview from sample
    overview = data[[1]][[nr]][["Abiotic_results_overview"]]
    
    #columnames required
    col_req = colnames(overview)[!(colnames(overview) %in% c("species_name"))]
  
    overview_list = list()
    overview_list[[soortgroep]] = overview[,col_req]
  
    #Remove aantal from columns required
    col_req = col_req[!(col_req %in% c("aantal"))]
   
    #Translate datalist results to relevant columns for Macrofauna V2
    unique_columns = unique(new_colnames1)
    
    temp_list = list()
    
    #BEREKEN STATISTIEK PER SAMPLE
    for(nr2 in 1:length(overview_list)){
      
      temp_overview = data.frame("Statistiek" = c("gemiddelde","standaarddeviatie"))
    
      #get required columns and make numeric
      overview_list_data = as.data.frame(t(sapply(overview_list[[1]][,col_req], as.numeric)),stringsAsFactors = FALSE)

      #loop over the sets of data
      for(uncolov in unique_colnames_overall){
        indexing = colnames_overall %in% uncolov
        values = as.vector(overview_list_data[1,indexing])
        scores = new_colnames1_score[indexing]
        colnames_sel = new_colnames1[indexing]
        unique_colnames_sel = unique(colnames_sel)
      
        for(uncol in unique_colnames_sel){
          indexing2 =  colnames_sel %in% uncol

          #Calculate value and stdv
          avg = (sum(scores[indexing2] * t(values[indexing2]))/sum(values))
          stdev = sqrt(sum((scores[indexing2] - avg)**2 * t(as.matrix(values[indexing2])))*(1/sum(values)))
          
          #Save the results
          add_datfr = data.frame("x" = c(avg,stdev))
          colnames(add_datfr) <- uncol
          temp_overview = cbind(temp_overview,add_datfr)
        }  
      }
      #Restruct
      temp_overview = as.data.frame(temp_overview,stringsAsFactors = FALSE)
      row.names(temp_overview) <- NULL 
      
      #add to list
      temp_list[[names(overview_list)[nr2]]] <- temp_overview
    }
    
    data[[1]][[nr]][["Statistic_results"]] = temp_list
  }
  return(list("Data" = data,"Comments" = fid,"Fatal_error" =  fatal_error))
}

BerekenZWaarden <- function(monsterrefdata){
  # Bereken de Z-Waarden vanuit de Statistiek-waarden
  # Hiervoor dient eerst de functie MatchMonsterReferentie() 
  # gebruikt te worden om de gemeten en gewenste abiotiek resultaten
  # aan elkaar te verbinden.
  
  # monsterrefdata <- matched[[1]]
  
  fid = c()
  fatal_error = 0
  
  #Make complete Z Value list
  full_list = list()
  
  #Save the abiotic parameters included
  abiotische_parameters = monsterrefdata[["Abiotics"]]
  data_mosterref = monsterrefdata[[1]]
  
  #loop over sample and reference data per smplcode
  for(nr in 1:length(data_mosterref)){
    
    # nr <- 1 
    
    #Combine in list
    temp_list = list()
    temp_list[["smpcode"]] = data_mosterref[[nr]][["smpcode"]]
    temp_list[["locname"]] = data_mosterref[[nr]][["locname"]]
    temp_list[["datesmp"]] = data_mosterref[[nr]][["datesmp"]]
    temp_list[["reftype"]] = data_mosterref[[nr]][["reftype"]]
    temp_list[["locX"]] = data_mosterref[[nr]][["locX"]]
    temp_list[["locY"]] = data_mosterref[[nr]][["locY"]]
     
    #Get statistics calculated
    names_seperation = names(data_mosterref[[nr]][["sample_statistics"]])

    #Z-Value storage list
    temp2_list = list()
    
    for(nr2 in 1:length(names_seperation)){
      #Get stats data from sample and reference

      # nr2 <- 1
      sample_df = as.data.frame(data_mosterref[[nr]][["sample_statistics"]][[names_seperation[nr2]]], stringsAsFactors = FALSE, drop = FALSE)
      ref_df = as.data.frame(data_mosterref[[nr]][["reference_statistics"]][[names_seperation[nr2]]], stringsAsFactors = FALSE, drop = FALSE)
      
      #Convert to character
      sample_df[] <- lapply(sample_df, as.character)
      ref_df[] <- lapply(ref_df, as.character)
      
      #Store parameters
      parameters = colnames(sample_df)[2:length(colnames(sample_df))]
      
      #Calculate Z-value
      average_sample = as.numeric(sample_df[sample_df$Statistiek == "gemiddelde",2:length(colnames(sample_df))])
      average_reference = as.numeric(ref_df[ref_df$Statistiek == "gemiddelde",2:length(colnames(ref_df))])
      SD_reference = as.numeric(ref_df[ref_df$Statistiek == "standaarddeviatie",2:length(colnames(ref_df))])

      # print(str(sample_df[(sample_df$Statistiek == "gemiddelde"),]))
      # print(average_sample)  
      
      #Make into a dataframe  
      result = data.frame("Parameter" = parameters,"gemiddelde_monster" = average_sample,"gemiddelde_referentie" = average_reference,
              "SD_referentie" = SD_reference)
      
      result$Zwaarde = (result$gemiddelde_monster-result$gemiddelde_referentie)/result$SD_referentie

      #Give score
      result$klasse = ""
      waarde = result$Zwaarde
      if(names_seperation[nr2] == "waterplanten"){
        result$klasse[waarde > 0.75] <- "Te hoog"
        result$klasse[waarde <= 0.75 & waarde > 0.5 & !(is.na(waarde))] <- "Hoog"
        result$klasse[waarde <= 0.5 & waarde >= -0.5 & !(is.na(waarde))] <- "Geen afwijking"
        result$klasse[waarde < -0.5 & waarde >= -0.75 & !(is.na(waarde))] <- "Laag"
        result$klasse[waarde < -0.75 & !(is.na(waarde))] <- "Te laag"
      }else{
        result$klasse[waarde > 1.0] <- "Te hoog"
        result$klasse[waarde <= 1.0 & waarde > 0.5 & !(is.na(waarde))] <- "Hoog"
        result$klasse[waarde <= 0.5 & waarde >= -0.5 & !(is.na(waarde))] <- "Geen afwijking"
        result$klasse[waarde < -0.5 & waarde >= -1.0 & !(is.na(waarde))] <- "Laag"
        result$klasse[waarde < -1.0 & !(is.na(waarde))] <- "Te laag"        
      }
      result$klasse[is.na(waarde)] <- "Niet bekend"
      
      #Store in list      
      temp2_list[[names_seperation[nr2]]] = result
    }
    
    #Store Z-values in temp list
    temp_list[["Z_value"]] = temp2_list
    
    #Store all in full list
    full_list[[nr]] <- temp_list
  }
  full_list_comp = list(full_list,"Abiotics" = abiotische_parameters)
  
  return(list("Data" = full_list_comp,"Comments" = fid,"Fatal_error" =  fatal_error))  
}
