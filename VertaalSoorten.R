VertaalSoorten <- function(monsters, type_data , path_databases){
  
  #monsters <- controlelist[[1]]
  #type_data <- soortgroep
  #path_databases <- "data\\"
  
  #Deze functie vervangd wanneer mogelijk de soortennamen, soort afkortingen
  #, nederlandse namen voor de latijnse namen.
  fid = c()
  fatal_error = 0
  
  #Get database name
  database_names <- c(
    "Macrofyten"    = "TWN_Macrophytes_20180619.csv",
    "Diatomeeen"    = "TWN_Diatoms_20180619.csv",
    "Vissen"        = "TWN_Fish_20180619.csv",
    "Macrofauna"    = "TWN_Macroinvertebrates_20180619.csv",
    "Macrofauna_V2" = "TWN_Macroinvertebrates_20180619.csv"
  )
  
  # JT: Eerst non-valide type_data uitsluiten
  if (!type_data %in% names(database_names)) {
    fid = c(fid, paste0("ERROR: onbekende soortgroep ", as.character(type_data), "."))
    fatal_error = 1
    fullsample = list()
    return(list("Data" = fullsample,"Comments" = fid, "Fatal_error" = fatal_error))
  }
  
  TWN_list_path <- paste0(path_databases, database_names[[type_data]])
  
  #Read database of type_data
  TWN_list = inlezenExcelOfCsv(TWN_list_path)
  TWN = TWN_list[[1]] 
  
  # JT: De onderstaande 3 regels code zouden in de functie inlezenExcelOfCsv
  # thuishoren. Het is ook makkelijk op te lossen door als argument `header =
  # TRUE` mee te geven. Beter nog is readr::read_csv2 dan zijn de defaultwaarden
  # geschikt voor het inlezen van Europese csv-bestanden.
  header_column = TWN[1,]
  data_TWN = TWN[(2:length(TWN[,1])),]
  colnames(data_TWN) <- as.vector(header_column)
  
  #Correct species names
  # JT: monsterlengte 0 uitsluiten
  if (length(monsters) == 0) { 
    fid = c(fid, paste0("ERROR: geen relevante monsters gevonden in aangeleverde monitoringsdata."))
    fatal_error = 1
    }

  #Check if the monsters is filled
  for (nr in seq_along(monsters)) {
    
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
  
  }
  return(list("Data" = monsters, "Comments" = fid, "Fatal_error" = fatal_error))
}