###########################################################
# AqMaD inlees functionaliteiten voor RShiny applicatie   #
#                                                         #
# Auteurs: Willem Stolte                                  #
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

require(tidyverse)
require(plotly) # eventueel voor interactieve plots (hoverfunctie)

#' Maak dataframe geschikt voor plotten uit abiotieklist
#' @param abiotieklist list uit abiotiekberekening
#' @return een tidy dataframe geschikt voor plotten
bereidAbiotiekPlotData <- function(abiotlist, sg){
  
  # abiotlist <- abiotieklist[[1]]  # only for testing
  # abiotlist <- abiotieklist_samp[[1]]
  # sg <- "Macrofyten"
  # sg <- 'Diatomeeen'
  # sg <- "Macrofauna"
  # sg <- "Macrofauna_V2"
  # ptm <- proc.time()
  for(ii in 1:length(abiotlist[[1]])){
    # print(ii)
    # ii <- 2
    df <- abiotlist[[1]][[ii]]$Abiotic_results
    if(ii == 1)  data <- df[FALSE,]
    if(nrow(df) == 0) {next}
    if(nrow(df) > 0){
      df$reftype <- abiotlist[[1]][[ii]]$reftype
      df$locname <- abiotlist[[1]][[ii]]$locname
      df$loccode <- abiotlist[[1]][[ii]]$loccode
      df$locX <- abiotlist[[1]][[ii]]$locX
      df$locY <- abiotlist[[1]][[ii]]$locY
      df$datesmp <- abiotlist[[1]][[ii]]$datesmp %>% unname() %>% as.character()
      data = data %>% bind_rows(df)
    }
  }
  # proc.time() - ptm
  data$datesmp <- as.POSIXct(data$datesmp, tz = "CET")
  
  if(sg == "Macrofyten"){
    plotData <- data %>% 
      gather(key = "parameter", value = "waarde", -species_name, -type, -reftype, -locname, -loccode, -datesmp, -locX, -locY)
    ## rename type to oeverplant & waterplant
    duplicate_oeverwaterplanten <- plotData[plotData$type == 3,]
    duplicate_oeverwaterplanten$type <- "oeverplanten"
    plotData[plotData$type == 2, "type"] <- "oeverplanten"
    plotData[plotData$type == 1, "type"] <- "waterplanten"
    plotData[plotData$type == 3, "type"] <- "waterplanten"
    plotData <- rbind(plotData, duplicate_oeverwaterplanten)
  } else if(sg == "Diatomeeen"){
    data$aantal <- NULL
    plotData <- data %>% 
      gather(key = "parameter", value = "waarde", -species_name, -reftype, -locname, -loccode, -datesmp, -locX, -locY)
  } else {plotData <- data %>% 
    gather(key = "parameter", value = "waarde", -species_name, -reftype, -locname, -loccode, -datesmp, -locX, -locY)}     
  
  
  plotData$waarde <- as.numeric(plotData$waarde)
  return(plotData)
}

#' Maak dataframe geschikt voor plotten uit abiotieklist gebaseerd op referentie opgave
#' @param abiotieklist list uit abiotiekberekening
#' @return een tidy dataframe geschikt voor plotten
bereidRefAbiotiekPlotData <- function(abiotlist, sg){
  
  # abiotlist <- abiotieklist[[1]]  # only for testing
  # abiotlist <-   abiotieklist_doel[[1]]
  # sg <- 'Diatomeeen'
  
  for(ii in 1:length(abiotlist[[1]])){
    # print(ii)
    # ii <- 1
    df <- abiotlist[[1]][[ii]]$Abiotic_results
    if(ii == 1)  data <- df[FALSE,]
    if(nrow(df) == 0) {next}
    if(nrow(df) > 0){
      df$locname <- abiotlist[[1]][[ii]]$locname
      data = data %>% bind_rows(df)
    }
  }
  
  if(sg == "Macrofyten"){
    plotData <- data %>% 
      gather(key = "parameter", value = "waarde", -species_name, -type, -locname)
    ## rename type to oeverplant & waterplant
    duplicate_oeverwaterplanten <- plotData[plotData$type == 3,]
    duplicate_oeverwaterplanten$type <- "oeverplanten"
    plotData[plotData$type == 2, "type"] <- "oeverplanten"
    plotData[plotData$type == 1, "type"] <- "waterplanten"
    plotData[plotData$type == 3, "type"] <- "waterplanten"
    plotData <- rbind(plotData, duplicate_oeverwaterplanten)
  } else if(sg == "Diatomeeen"){
    data$aantal <- NULL
    plotData <- data %>% 
      gather(key = "parameter", value = "waarde", -species_name, -locname)
  } else {plotData <- data %>% 
    gather(key = "parameter", value = "waarde", -species_name, -locname)}     
  
  plotData$waarde <- as.numeric(plotData$waarde)
  
  return(plotData)
}

#' Maak dataframe geschikt voor plotten uit abiotieklist gebaseerd op referentie opgave
#' @param abiotieklist list uit abiotiekberekening
#' @return een tidy dataframe geschikt voor plotten
bereidRefKRWAbiotiekPlotData <- function(abiotlist, sg){
  
  ## geschreven voor macrofyten!
  # abiotlist <- abiotieklist[[1]]  # only for testing
  # abiotlist <-   abiotieklist_doel[[1]]
  # sg <- 'Macrofyten'
  
  for(ii in 1:length(abiotlist[[1]])){
    # print(ii)
    # ii <- 1
    df <- abiotlist[[1]][[ii]]$Abiotic_results
    if(ii == 1)  data <- df[FALSE,]
    if(nrow(df) == 0) {next}
    if(nrow(df) > 0){
      df$reftype <- abiotlist[[1]][[ii]]$reftype
      data = data %>% bind_rows(df)
    }
  }
  
  plotData <- data %>% 
    gather(key = "parameter", value = "waarde", -species_name, -type, -reftype)
  ## rename type to oeverplant & waterplant
  duplicate_oeverwaterplanten <- plotData[plotData$type == 3,]
  duplicate_oeverwaterplanten$type <- "oeverplanten"
  plotData[plotData$type == 2, "type"] <- "oeverplanten"
  plotData[plotData$type == 1, "type"] <- "waterplanten"
  plotData[plotData$type == 3, "type"] <- "waterplanten"
  plotData <- rbind(plotData, duplicate_oeverwaterplanten)
  
  plotData$waarde <- as.numeric(plotData$waarde)
  
  return(plotData)
}



bereidStatistiekDataHuidig <- function(statlist, sg){
  
  # statlist <- statistieklist_samp[[1]][[1]] ## get all samples from statistieklist
  # sg <- "Macrofyten"
  # sg <- 'Diatomeeen'
  # sg <- "Macrofauna"
  # sg <- "Macrofauna_V2"
  # bereidStatistiekDataHuidig(statlist, sg = "Diatomeeen")
  library(purrr)
  library(reshape2)
  
  if(sg == "Macrofyten"){
    
    # names(statlist) <- paste("Sample_", 1:length(statlist), sep = "")
    samples <- map_chr(statlist, "smpcode") ## get all sample codes
    
    #ptm <- proc.time()
    udf_prep <- statlist %>%
      set_names(samples) %>%
      enframe("samplecode", "statlist") %>%
      separate("samplecode", into = c("locatiecode", "datum"), sep = "_")
    
    udf_oever <- udf_prep %>%
      mutate(#locatiecode = map_chr(statlist, "loccode"),
        #reftype     = map_chr(statlist, "reftype"),
        #xcoor       = map_dbl(statlist, "locX"),
        #ycoor       = map_dbl(statlist, "locY"),
        soortgroep   = sg,
        type        = "oeverplanten",
        oeverplant  = map(statlist, c(16, 2))) %>%
      select(-statlist) %>%
      tidyr::unnest()
    
    udf_water <- udf_prep %>%
      mutate(#locatiecode = map_chr(statlist, "loccode"),
        # reftype     = map_chr(statlist, "reftype"),
        # xcoor       = map_dbl(statlist, "locX"),
        # ycoor       = map_dbl(statlist, "locY"),
        soortgroep   = sg,
        type        = "waterplanten",
        waterplant  = map(statlist, c(16, 1))) %>%
      select(-statlist) %>%
      tidyr::unnest()
    
    udf <- rbind(udf_water, udf_oever)
    colnames(udf)
    
    udf_return <- melt(udf, 
                  id.vars = c("locatiecode", "datum", "soortgroep", "type", "Statistiek"),
                  variable.name = "parameter",
                  value.name = "waarde")
    
    # setcolorder(udf, neworder = c(""))
    #proc.time() - ptm
  }
  
  
  if(sg != "Macrofyten"){
    
    # names(statlist) <- paste("Sample_", 1:length(statlist), sep = "")
    samples <- map_chr(statlist, "smpcode") ## get all sample codes
    
    #ptm <- proc.time()
    udf_prep <- statlist %>%
      set_names(samples) %>%
      enframe("samplecode", "statlist") %>%
      separate("samplecode", into = c("locatiecode", "datum"), sep = "_")
    
    udf_stats <- udf_prep %>%
      mutate(#locatiecode = map_chr(statlist, "loccode"),
        #reftype     = map_chr(statlist, "reftype"),
        #xcoor       = map_dbl(statlist, "locX"),
        #ycoor       = map_dbl(statlist, "locY"),
        #type        = "oeverplanten",
        soortgroep   = sg,
        stats        = map(statlist, c(16, 1))) %>%
        select(-statlist) %>%
        tidyr::unnest()
    
    udf_return <- melt(udf_stats, 
                       id.vars = c("locatiecode", "datum", "soortgroep", "Statistiek"),
                       variable.name = "parameter",
                       value.name = "waarde")
    
    # setcolorder(udf, neworder = c(""))
    #proc.time() - ptm
  }
  
  return(udf_return)
}

bereidStatistiekDataRef <- function(statlist, sg, optie){
  
  # statlist <- statistieklist_samp[[1]][[1]] ## get all samples from statistieklist
  # sg <- "Macrofyten"
  # sg <- 'Diatomeeen'
  # sg <- "Macrofauna"
  # sg <- "Macrofauna_V2"
  # bereidStatistiekData(statlist, sg = "Macrofyten")
  # str(statistieklist[[1]][[1]], 2)
  # statlist <- statistieklist[[1]][[1]]
  # str(statlist, 2)
  # statlist <- statistieklist_krw[[1]][[1]]
  # statlist <- statistieklist_doel[[1]][[1]]
  
  if(optie == 'KRW'){
  
  if(sg == "Macrofyten"){
    
    # names(statlist) <- paste("Sample_", 1:length(statlist), sep = "")
    samples <- map_chr(statlist, "reftype") ## get all sample codes
    
    #ptm <- proc.time()
    udf_prep <- statlist %>%
      set_names(samples) %>%
      enframe("reftype", "statlist") 
    
    udf_oever <- udf_prep %>%
      mutate(#locatiecode = map_chr(statlist, "loccode"),
        #reftype     = map_chr(statlist, "reftype"),
        #xcoor       = map_dbl(statlist, "locX"),
        #ycoor       = map_dbl(statlist, "locY"),
        soortgroep   = sg,
        type        = "oeverplanten",
        oeverplant  = map(statlist, c(8, 2))) %>%
      select(-statlist) %>%
      tidyr::unnest()
    
    udf_water <- udf_prep %>%
      mutate(#locatiecode = map_chr(statlist, "loccode"),
        # reftype     = map_chr(statlist, "reftype"),
        # xcoor       = map_dbl(statlist, "locX"),
        # ycoor       = map_dbl(statlist, "locY"),
        soortgroep   = sg,
        type        = "waterplanten",
        waterplant  = map(statlist, c(8, 1))) %>%
      select(-statlist) %>%
      tidyr::unnest()
    
    udf <- rbind(udf_water, udf_oever)
    # colnames(udf)
    
    udf_return <- melt(udf, 
                       id.vars = c("reftype", "soortgroep", "type", "Statistiek"),
                       variable.name = "parameter",
                       value.name = "waarde")
    
    # setcolorder(udf, neworder = c(""))
    #proc.time() - ptm
  }
  
  
  if(sg != "Macrofyten"){
    
    # names(statlist) <- paste("Sample_", 1:length(statlist), sep = "")
    samples <- map_chr(statlist, "reftype") ## get all sample codes
    
    #ptm <- proc.time()
    udf_prep <- statlist %>%
      set_names(samples) %>%
      enframe("reftype", "statlist")
    
    udf_stats <- udf_prep %>%
      mutate(#locatiecode = map_chr(statlist, "loccode"),
        #reftype     = map_chr(statlist, "reftype"),
        #xcoor       = map_dbl(statlist, "locX"),
        #ycoor       = map_dbl(statlist, "locY"),
        #type        = "oeverplanten",
        soortgroep   = sg,
        stats        = map(statlist, c(8, 1))) %>%
      select(-statlist) %>%
      tidyr::unnest()
    
    udf_return <- melt(udf_stats, 
                       id.vars = c("reftype", "soortgroep", "Statistiek"),
                       variable.name = "parameter",
                       value.name = "waarde")
    
    # setcolorder(udf, neworder = c(""))
    #proc.time() - ptm
  }
  } else {
    if(sg == "Macrofyten"){
      
      # names(statlist) <- paste("Sample_", 1:length(statlist), sep = "")
      samples <- map_chr(statlist, "locname") ## get all sample codes
      
      #ptm <- proc.time()
      udf_prep <- statlist %>%
        set_names(samples) %>%
        enframe("locname", "statlist")
      
      udf_oever <- udf_prep %>%
        mutate(#locatiecode = map_chr(statlist, "loccode"),
          #reftype     = map_chr(statlist, "reftype"),
          #xcoor       = map_dbl(statlist, "locX"),
          #ycoor       = map_dbl(statlist, "locY"),
          soortgroep   = sg,
          type        = "oeverplanten",
          oeverplant  = map(statlist, c(8, 2))) %>%
        select(-statlist) %>%
        tidyr::unnest()
      
      udf_water <- udf_prep %>%
        mutate(#locatiecode = map_chr(statlist, "loccode"),
          # reftype     = map_chr(statlist, "reftype"),
          # xcoor       = map_dbl(statlist, "locX"),
          # ycoor       = map_dbl(statlist, "locY"),
          soortgroep   = sg,
          type        = "waterplanten",
          waterplant  = map(statlist, c(8, 1))) %>%
        select(-statlist) %>%
        tidyr::unnest()
      
      udf <- rbind(udf_water, udf_oever)
      # colnames(udf)
      
      udf_return <- melt(udf, 
                         id.vars = c("locname", "soortgroep", "type", "Statistiek"),
                         variable.name = "parameter",
                         value.name = "waarde")
      
      # setcolorder(udf, neworder = c(""))
      #proc.time() - ptm
    }
    
    
    if(sg != "Macrofyten"){
      
      # names(statlist) <- paste("Sample_", 1:length(statlist), sep = "")
      samples <- map_chr(statlist, "locname") ## get all sample codes
      
      #ptm <- proc.time()
      udf_prep <- statlist %>%
        set_names(samples) %>%
        enframe("locname", "statlist") 
      
      udf_stats <- udf_prep %>%
        mutate(#locatiecode = map_chr(statlist, "loccode"),
          #reftype     = map_chr(statlist, "reftype"),
          #xcoor       = map_dbl(statlist, "locX"),
          #ycoor       = map_dbl(statlist, "locY"),
          #type        = "oeverplanten",
          soortgroep   = sg,
          stats        = map(statlist, c(9, 1))) %>%
        select(-statlist) %>%
        tidyr::unnest()
      
      # statlist[[1]][[8]][[1]]
      
      udf_return <- melt(udf_stats, 
                         id.vars = c("locname", "soortgroep", "Statistiek"),
                         variable.name = "parameter",
                         value.name = "waarde")
      
      # setcolorder(udf, neworder = c(""))
      #proc.time() - ptm
    }
  }
  
  return(udf_return)
}


#' ken groep toe aan abiotiekwaarden
#' @param  dataframe met z-waarden
#' @return plotData een plot object
groepeerAbiotiekPlotData <- function(plotData, sg, wt){
  
  ## sg <- "Macrofyten"
  ## sg <- "Macrofauna"
  ## wt <- "stilstaand"
  ## wt <- "stromend"
  ## sg <- "Vissen"
  ## plotData <- df.plotz
  ## plotData <- abiotieklist_samp[[1]]
  
  groepen_prep <- read_csv2('data/AqMaD_Indeling-parameters-in-groepen.csv') %>%
    dplyr::filter(soortgroep == sg) %>%
    filter(!is.na(kolomnaam))
  
  if(wt == "stilstaand"){
    colnames(groepen_prep)[which(colnames(groepen_prep) == "parametergroep")] <- "parametergroep_oud"
    colnames(groepen_prep)[which(colnames(groepen_prep) == "esf_stilstaand")] <- "parametergroep"
    groepen_prep[, which(colnames(groepen_prep)=="esf_stromend_cluster")] <- NULL
    groepen <- unique(groepen_prep)
    }
  
  if(wt == "stromend"){
    colnames(groepen_prep)[which(colnames(groepen_prep) == "parametergroep")] <- "parametergroep_oud"
    colnames(groepen_prep)[which(colnames(groepen_prep) == "esf_stromend_cluster")] <- "parametergroep"
    groepen_prep[, which(colnames(groepen_prep)=="esf_stilstaand")] <- NULL
    groepen <- unique(groepen_prep)
  }
  
  gegroepeerdeAbiotiekPlotData <- plotData %>% 
    left_join(groepen, by = c("parameter" = "kolomnaam")) %>%
    mutate(groep = soortgroep)
  return(gegroepeerdeAbiotiekPlotData)
}

#' plot abiotiek berekend uit opnames
#' @param plotdf dataframe met abiotiekwaarden
#' @param jaar jaar van opname
#' @param locatie locatie van opname
#' @return een plot object
plotAbiotiek <- function(plotdf, jaar = NULL, locatie = NULL, refrtype = NULL, sg = NULL, pg = NULL, parm = NULL, ptype = NULL){
  # functie voor het plotten van abiotiek, berekend uit de opname van waterplanten, macrofyten, diatomeen of beestjes
  # 
  # Input: dataframe met abiotiekwaarden
  # 
  # | COLUMNS   |  sample 1 /rec 1 | sample 1 / rec 2 | sample 2 /rec 1 | etc
  # -----------voorbeeldinvoer---------------------------------------------------------------
  # | smocode   | 
  # | reftype   |
  # | extref    | 
  # | locname   | 
  # | loccode   | 
  # | datesmp   | 
  # | locX      |  
  # | locY      | 
  # | abiotparam|
  # | soortnaam |
  # | waarde    |
  
  # plotAbiotiek(groepPlotData(), jaar = input$abiotJaar, locatie = input$abiotLocatie, pg = input$abiotPargroep)
  # plotAbiotiek(plotdf = groepPlotData, pg = "Zuurstof")
  # plotdf <- groepPlotData
  # plotdf <- groepPlotData_doel
  # jaar <- 2007
  # jaar <- NULL
  # pg <- "Nutrienten"
  # locatie <- "Aa_Voorste Stroom onder brug"
  # locatie <- "Bij de duiker_431658"
  # locatie <- "ALBL0002"
  # sg <- "Macrofyten"
  # parm <- NULL
  # ptype <- NULL
  # ptype <- "oeverplanten"
  # plotAbiotiek(huidigAbiotiekGroep(), jaar = input$huidigAbiotiekJaar_ui, locatie = input$huidigAbiotiekLocatie_ui, pg = input$huidigAbiotiekParametergroep_ui, ptype = input$huidigAbiotiekPtype_ui, sg = variables$soortgroep)
  # pg <- "Habitatgeschiktheid"
  
  
  # fouten afvangen? 
  
  # stijl wordt later toegevoegd aan plot. Hier veranderen voor andere stijl.
  plotstijl <-
    theme_bw() + theme(
      text = element_text(size = 15),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "right",
      axis.line = element_blank(),
      axis.ticks.x = element_blank())
  
  
  ##==== dataselectie =================================
  
  subdf = plotdf
  
    # alleen als er jaar meegegeven is
  if(!is.null(jaar)){
    subdf$jaartal <- as.numeric(lubridate::year(subdf$datesmp))
    subdf <- subdf %>% filter(jaartal == jaar)
  }
  
  # alleen als er een locatie meegegeven is
  if(!is.null(locatie)){
    subdf <- subdf %>% filter(locname == locatie)  # of loccode
  }
  
  # alleen als er een refrtype meegegeven is
  if(!is.null(refrtype)){
    subdf <- subdf %>% filter(reftype == refrtype)  # of loccode
  }
  
  
  # alleen als er een parametergroep meegegeven is
  if(!is.null(pg)){
    subdf <- subdf %>% filter(parametergroep == pg)
  }
  
  # alleen als er een soortgroep meegegeven is
  if(!is.null(sg)){
    subdf <- subdf %>% filter(soortgroep == sg) 
  }
  
  # alleen als er een parameter meegegeven is
  if(!is.null(parm)){
    subdf <- subdf %>% filter(parameter == parm) 
  }
  
  # alleen als er een parameter meegegeven is
  if(!is.null(ptype)){
    subdf <- subdf %>% filter(type == ptype) 
  }
  
  ##======== basisplot ================================
  
  # if(sg == "Macrofyten"){
  #   p <- subdf %>%
  #     ggplot(aes(x = "",y = waarde)) +
  #     geom_boxplot(aes(color = type), outlier.shape = 21) +
  #     # facet_wrap(.~parameter, scales = "free", nrow = length(unique(subdf$parameter))) +
  #     facet_wrap("parameter_eenheid_kort", scales = "free", nrow = 1, drop = F) #+ 
  #   #coord_flip()
  #   
  #   }

  # if(sg != "Macrofyten"){
  #  p <- subdf %>%
  #   ggplot(aes(x = "",y = waarde)) +
  #   geom_boxplot(outlier.shape = 21) +
  #   # facet_wrap(.~parameter, scales = "free", nrow = length(unique(subdf$parameter))) +
  #   facet_wrap("parameter_eenheid_kort", scales = "free", nrow = 1) #+ 
  #   #coord_flip()
  # }

  p <- subdf %>%
    ggplot(aes(x = "",y = waarde)) +
    geom_boxplot(outlier.shape = 21) +
    # facet_wrap(.~parameter, scales = "free", nrow = length(unique(subdf$parameter))) +
    facet_wrap("parameter_eenheid_kort", scales = "free", nrow = 1)
  
  ##======== facetting afhankelijk van invoer =========
  
  # # alleen als alleen een jaar meegegeven is
  # if(!is.null(jaar) & is.null(locatie)){
  #   p = p + facet_grid(locatie ~ .)
  # }
  # 
  # # alleen als alleen een locatie meegegeven is
  # if(!is.null(locatie) & is.null(jaar)){
  #   p = p + facet_grid(jaar ~ .)
  # }
  # 
  # # alleen als een jaar en locatie meegegeven is
  # if(!is.null(jaar) & !is.null(locatie)){
  #   # no facetting necessary
  # }
  # 
  # # alleen als geen jaar en geen locatie meegegeven is
  # if(is.null(jaar) & is.null(locatie)){
  #   p = p + facet_grid(jaar ~ locname)
  # }
  
  ##======= stijl toevoegen =================================================
  
  p = p + plotstijl
  
  ## overwegen om plotly als uitvoer plot te gebruiken 
  ## NB plotly vraagt om een andere functie voor rendering 
  ## plotly facetting werkt niet goed in combinatie met boxplots - wordt dus niet gedaan
  
  # p <- plotly_build(p)

  # p %>% layout(autosize = F, width = 500, height = 500)
  
  ## to remove outliers from plot
  # p$x$data <- lapply(p$x$data, FUN = function(x){
  #   x$marker = list(opacity = 0)
  #   return(x)
  # })
  
  # p$layout$height = 300 * length(unique(subdf$parameter))
  # p$width <- 8 * 150
  # p$height <- 500
  
  
  ##======= uitvoer =========================================================  

  return(p)
  
}

plotRefAbiotiek <- function(plotdf, locatie = NULL, sg = NULL, pg = NULL, refrtype = NULL, ptype = NULL){
  # functie voor het plotten van abiotiek, berekend uit de opname van waterplanten, macrofyten, diatomeen of beestjes
  # 
  # Input: dataframe met abiotiekwaarden
  # 
  # | COLUMNS   |  sample 1 /rec 1 | sample 1 / rec 2 | sample 2 /rec 1 | etc
  # -----------voorbeeldinvoer---------------------------------------------------------------
  # | smocode   | 
  # | reftype   |
  # | extref    | 
  # | locname   | 
  # | loccode   | 
  # | datesmp   | 
  # | locX      |  
  # | locY      | 
  # | abiotparam|
  # | soortnaam |
  # | waarde    |
  
  # plotAbiotiek(groepPlotData(), jaar = input$abiotJaar, locatie = input$abiotLocatie, pg = input$abiotPargroep)
  # plotAbiotiek(plotdf = groepPlotData, pg = "Zuurstof")
  # plotdf <- groepPlotData
  # plotdf <- groepPlotData_doel
  # pg <- "Nutrienten"
  # locatie <- "Aa_Voorste Stroom onder brug"
  # sg <- "Macrofyten"
  # parm <- NULL
  # ptype <- "oeverplanten"
  
  # fouten afvangen? 
  
  # stijl wordt later toegevoegd aan plot. Hier veranderen voor andere stijl.
  plotstijl <-
    theme_bw() + theme(
      text = element_text(size = 15),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "right",
      axis.line = element_blank(),
      axis.ticks.x = element_blank())
  
  
  ##==== dataselectie =================================
  
  subdf = plotdf
  
  # alleen als er jaar meegegeven is
  if(!is.null(jaar)){
    subdf <- subdf %>% filter(jaartal == jaar)
  }
  
  # alleen als er een locatie meegegeven is
  if(!is.null(locatie)){
    subdf <- subdf %>% filter(locname == locatie)  # of loccode
  }
  
  # alleen als er een locatie meegegeven is
  if(!is.null(refrtype)){
    subdf <- subdf %>% filter(reftype == refrtype)  # of loccode
  }
  
  
  # alleen als er een parametergroep meegegeven is
  if(!is.null(pg)){
    subdf <- subdf %>% filter(parametergroep == pg)
  }
  
  # alleen als er een soortgroep meegegeven is
  if(!is.null(sg)){
    subdf <- subdf %>% filter(soortgroep == sg) 
  }
  
  # alleen als er een parameter meegegeven is
  if(!is.null(parm)){
    subdf <- subdf %>% filter(parameter == parm) 
  }
  
  # alleen als er een parameter meegegeven is
  if(!is.null(ptype)){
    subdf <- subdf %>% filter(type == ptype) 
  }
  
  ##======== basisplot ================================
  
  # if(sg == "Macrofyten"){
  #   p <- subdf %>%
  #     ggplot(aes(x = "",y = waarde)) +
  #     geom_boxplot(aes(color = type), outlier.shape = 21) +
  #     # facet_wrap(.~parameter, scales = "free", nrow = length(unique(subdf$parameter))) +
  #     facet_wrap("parameter_eenheid_kort", scales = "free", nrow = 1, drop = F) #+ 
  #   #coord_flip()
  #   
  #   }
  
  # if(sg != "Macrofyten"){
  #  p <- subdf %>%
  #   ggplot(aes(x = "",y = waarde)) +
  #   geom_boxplot(outlier.shape = 21) +
  #   # facet_wrap(.~parameter, scales = "free", nrow = length(unique(subdf$parameter))) +
  #   facet_wrap("parameter_eenheid_kort", scales = "free", nrow = 1) #+ 
  #   #coord_flip()
  # }
  
  p <- subdf %>%
    ggplot(aes(x = "",y = waarde)) +
    geom_boxplot(outlier.shape = 21) +
    # facet_wrap(.~parameter, scales = "free", nrow = length(unique(subdf$parameter))) +
    facet_wrap("parameter_eenheid_kort", scales = "free", nrow = 1)
  
  ##======== facetting afhankelijk van invoer =========
  
  # # alleen als alleen een jaar meegegeven is
  # if(!is.null(jaar) & is.null(locatie)){
  #   p = p + facet_grid(locatie ~ .)
  # }
  # 
  # # alleen als alleen een locatie meegegeven is
  # if(!is.null(locatie) & is.null(jaar)){
  #   p = p + facet_grid(jaar ~ .)
  # }
  # 
  # # alleen als een jaar en locatie meegegeven is
  # if(!is.null(jaar) & !is.null(locatie)){
  #   # no facetting necessary
  # }
  # 
  # # alleen als geen jaar en geen locatie meegegeven is
  # if(is.null(jaar) & is.null(locatie)){
  #   p = p + facet_grid(jaar ~ locname)
  # }
  
  ##======= stijl toevoegen =================================================
  
  p = p + plotstijl
  
  ## overwegen om plotly als uitvoer plot te gebruiken 
  ## NB plotly vraagt om een andere functie voor rendering 
  ## plotly facetting werkt niet goed in combinatie met boxplots - wordt dus niet gedaan
  
  # p <- plotly_build(p)
  
  # p %>% layout(autosize = F, width = 500, height = 500)
  
  ## to remove outliers from plot
  # p$x$data <- lapply(p$x$data, FUN = function(x){
  #   x$marker = list(opacity = 0)
  #   return(x)
  # })
  
  # p$layout$height = 300 * length(unique(subdf$parameter))
  # p$width <- 8 * 150
  # p$height <- 500
  
  
  ##======= uitvoer =========================================================  
  
  return(p)
  
}

#' Maak dataframe geschikt voor plotten van Z-waarden voor macrofyten
#' @param zlijst list uit abiotiekberekening
#' @return een tidy dataframe geschikt voor plotten
bereidZplotData <- function(z){
  
  # z <- zwaarden
  
  for(ii in  seq(length(z[[1]][[1]]))){
    ## ii <- 1
    for(jj in seq(length(z$Data[[1]][[ii]]$Z_value))){
      ## jj <- 1
      df <- z$Data[[1]][[ii]]$Z_value[[jj]]
      df$locname <- z$Data[[1]][[ii]]$locname
      df$smpcode <- z$Data[[1]][[ii]]$smpcode
      df$locX <- z$Data[[1]][[ii]]$locX
      df$locY <- z$Data[[1]][[ii]]$locY
      df$datesmp <- z$Data[[1]][[ii]]$datesmp %>% unname() %>% as.character()
      df$soorttype <- names(z$Data[[1]][[ii]]$Z_value)[jj]
      if(ii == 1) data <- df
      if(ii != 1) data <- data %>% bind_rows(df)
    }
  }
  
  data$datesmp <- as.POSIXct(data$datesmp, tz = "CET")
  data$jaar <- as.integer(format(data$datesmp, "%Y"))
  return(data)
}

groepeerZplotData <- function(plotData, sg, wt){
  
  ## sg <- "Macrofyten"
  ## plotData <- df.plotz
  ## sg <- "Diatomeeen"
  ## sg <- "Macrofauna_V2"
  ## wt <- "stromend"
  
  
  groepen_prep <- read_csv2('data/AqMaD_Indeling-parameters-in-groepen.csv') %>%
    dplyr::filter(soortgroep == sg) %>%
    filter(!is.na(kolomnaam))
  
  if(wt == "stilstaand"){
    colnames(groepen_prep)[which(colnames(groepen_prep) == "parametergroep")] <- "parametergroep_oud"
    colnames(groepen_prep)[which(colnames(groepen_prep) == "esf_stilstaand")] <- "parametergroep"
    groepen_prep[, which(colnames(groepen_prep)=="esf_stromend_cluster")] <- NULL
    groepen <- unique(groepen_prep)
  }
  
  if(wt == "stromend"){
    colnames(groepen_prep)[which(colnames(groepen_prep) == "parametergroep")] <- "parametergroep_oud"
    colnames(groepen_prep)[which(colnames(groepen_prep) == "esf_stromend_cluster")] <- "parametergroep"
    groepen_prep[, which(colnames(groepen_prep)=="esf_stilstaand")] <- NULL
    groepen <- unique(groepen_prep)
  }
  
  
  gegroepeerdeAbiotiekPlotData <- plotData %>% 
    left_join(groepen, by = c("Parameter" = "kolomnaam")) %>%
    mutate(groep = soortgroep)
  
  gegroepeerdeAbiotiekPlotData$Parameter <- as.factor(as.character(gegroepeerdeAbiotiekPlotData$Parameter))
  return(gegroepeerdeAbiotiekPlotData)
}

#' plot waarden berekend uit opnames
#' @param zdf dataframe met z-waarden
#' @param jaar jaar van opname
#' @param locatie locatie van opname
#' @return een plot object
#' 
#' 
#' 
#' 

plotZtijdseries <- function(zdf, locaties = NULL, ptype = NULL, pg = NULL){
  
  # functie voor het plotten van z-waarden, berekend uit de abiotiek en referentiewaarden
  # beginjaar en eindjaar halen uit data
  # gebruiker kan dit aanpassen met slider in ui
  # 
  # Input: dataframe met z-waarden
  # 
  # | COLOMNS  |  z | 
  # -----------------
  # | smocode  | 
  # | reftype  |
  # | extref   | 
  # | locname  | 
  # | loccode  | 
  # | datesmp  | 
  # | locX     |  
  # | locY     | 
  # | abiotparam
  # | z-waarde
  
  # zdf <- df.plotz2
  # macrofauna <- df.plotz2
  # locaties <- "Esse_Roermond"
  # locaties <- "Aa_Voorste Stroom onder brug" # , pg = "Nutrienten", ptype = "oeverplanten")
  # locaties <- "140216"
  # locaties <- "ALBL0002"
  # locaties <- "ALBL0013"
  # 
  # pg <- "Macro-ionen"
  # pg <- "Habitatgeschiktheid"
  # pg <- "Licht"
  # pg <- "Hydrologie & Morfologie"
  # ptype <- "oeverplanten"
  # ptype <- "Macrofauna"
  # pg <- "Nutrienten"
  # locaties <- "BENL0377"
  # plotZtijdseries(zdf = df.plotz2, locaties = "140216", pg = "Nutrienten", ptype = "oeverplanten")
  
  
  # stijl wordt later toegevoegd aan plot. Hier veranderen voor andere stijl.
  plotstijl <-
    theme(
      # axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.text.align = 0,
      legend.position = "right",
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      strip.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      panel.grid.minor.y = element_blank(),
      panel.grid.major = element_line(colour = "grey50"),
      panel.spacing = unit(1.5, "lines"))
  

  ##==== dataselectie =================================

  subdf <- zdf
    
  # selectie van tijdperiode

    # subdf <- zdf %>% 
    # filter(
    #   as.integer(format(datesmp, format = "%Y")) >= beginjaar &
    #     as.integer(format(datesmp, format = "%Y")) <= eindjaar
    # )

  # alleen als er een locatie meegegeven is
  if(!is.null(locaties)){
    subdf <- subdf %>% filter(locname %in% locaties)  # of locname
  }
  
  # alleen als er een parametergroep meegegeven is
  if(!is.null(pg)){
    subdf <- subdf %>% filter(parametergroep == pg) 
  }
  
  # keuze tussen waterplanten en oeverplanten
  if(!is.null(ptype)){
    subdf <- subdf %>% filter(soorttype == ptype)
  }
  
  ##======== basisplot ================================
  
  library(scales)
  
  colors <- c("Te hoog" = "salmon1", 
              "Hoog" = "khaki2",
              "Geen afwijking" = "darkseagreen1",
              "Laag" = "paleturquoise1",
              "Te laag" = "skyblue2",
              "Niet bekend" = "grey")
  
  min_date <- min(subdf$datesmp) - (60*60*24*7)
  max_date <- max(subdf$datesmp) + (60*60*24*7)
  
  schaduw <- data.frame(xmin = min_date, xmax = max_date, 
                        ymin_a = c(1.0, 0.5, -0.5, -1.0, -Inf), ymax_a = c(Inf, 1.0, 0.5, -0.5, -1.0), 
                        ymin_b = c(0.75, 0.5, -0.5, -0.75, -Inf), ymax_b = c(Inf, 0.75, 0.5, -0.5, -0.75),
                        fill = c("Te hoog", "Hoog", "Geen afwijking", "Laag", "Te laag"))
  
  p <- subdf %>%
    ggplot(aes(x = datesmp, y = Zwaarde)) 
  
  
  
     
  if(ptype %in% c("oeverplanten", "Diatomeeen", "Macrofauna", "Macrofauna_V2", "Vissen")){
          # p <- p + geom_rect(aes(xmin = min_date, xmax = max_date, ymin = 1.0,  ymax = Inf, fill = "Te hoog"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = 0.5,  ymax = 1.0, fill = "Hoog"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -0.5, ymax = 0.5, fill = "Geen afwijking"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -1.0, ymax = -0.5, fill = "Laag"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = -1.0, fill = "Te laag"), alpha = 0.75) 
          p <- p + geom_rect(data = schaduw, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin_a, ymax = ymax_a, fill = fill), alpha = 0.75)
          
          plot_label <- c(expression("Te hoog (">="1.0)"), "Hoog (0.5 - 1.0)", "Geen afwijking (-0.5 - 0.5)", "Laag (-1 - -0.5)", expression("Te laag ("<="-1.0)"))
  }
  
  if(ptype %in% c("waterplanten")){
          # p <- p + geom_rect(aes(xmin = min_date, xmax = max_date, ymin = 0.75,  ymax = Inf, fill = "Te hoog"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = 0.5,   ymax = 0.75, fill = "Hoog"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -0.5,  ymax = 0.5, fill = "Geen afwijking"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -0.75, ymax = -0.5, fill = "Laag"), alpha = 0.75) +
          #          geom_rect(aes(xmin = min_date, xmax = max_date, ymin = -Inf,  ymax = -0.75, fill = "Te laag"), alpha = 0.75) 
          p <- p + geom_rect(data = schaduw, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin_b, ymax = ymax_b, fill = fill), alpha = 0.75)
          
          plot_label <- c(expression("Te hoog (">="0.75)"), "Hoog (0.5 - 0.75)", "Geen afwijking (-0.5 - 0.5)", "Laag (-0.75 - -0.5)", expression("Te laag ("<="-0.75)"))
    }  
  
  p <- p + geom_line() +
    scale_x_datetime(labels = date_format("%d-%m-%y"), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(name = "Waarde oordeel", 
                      values = colors,
                      breaks = c("Te hoog", "Hoog", "Geen afwijking", "Laag", "Te laag"),
                      labels = plot_label) +
    geom_point(cex = 2) +
    facet_grid(parameter_eenheid_kort ~ .)
  
  p.plot <- p + plotstijl

  return(p.plot)
  
}



#' plot waarden berekend uit opnames
#' @param zdf dataframe met z-waarden
#' @param datum datum van opname
#' @param locatie locatie van opname
#' @return een plot object
plotZwaarden <- function(zdf, jaartal, ptype, locaties = NULL){
  
  # functie voor het plotten van z-waarden, berekend uit de abiotiek en referentiewaarden
  # beginjaar en eindjaar halen uit data
  # gebruiker kan dit aanpassen met slider in ui
  # 
  # Input: dataframe met z-waarden
  # 
  # | COLOMNS  |  z | 
  # -----------------
  # | smocode  | 
  # | reftype  |
  # | extref   | 
  # | locname  | 
  # | loccode  | 
  # | datesmp  | 
  # | locX     |  
  # | locY     | 
  # | abiotparam
  # | z-waarde
  
  # zdf <- df.plotz2
  # jaar <- 2004
  # jaartal <- 2007
  # ptype <- "waterplanten"
  # ptype <- "Diatomeeen"
  # locaties <- "X=178000_Y=484000"
  # locaties <- "ALBL0002"
  
  # stijl wordt later toegevoegd aan plot. Hier veranderen voor andere stijl.
  
  plotstijl <- theme(
      # axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.text.align = 0,
      axis.line = element_blank(),
      strip.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.ticks = element_blank())
      # axis.text = element_text(size=10))

  cols <- c("Te hoog" = "salmon1", 
            "Hoog" = "khaki2",
            "Geen afwijking" = "darkseagreen1",
            "Laag" = "paleturquoise1",
            "Te laag" = "skyblue2",
            "Niet bekend" = "grey")
  
  
  ## #Give score
  ## result$klasse = ""
  ## waarde = result$Zwaarde
  ## if(names_seperation[nr2] == "waterplanten"){
  ##   result$klasse[waarde > 0.75] <- "Te hoog"
  ##   result$klasse[waarde <= 0.75 & waarde > 0.5 & !(is.na(waarde))] <- "Hoog"
  ##   result$klasse[waarde <= 0.5 & waarde >= -0.5 & !(is.na(waarde))] <- "Geen afwijking"
  ##   result$klasse[waarde < -0.5 & waarde >= -0.75 & !(is.na(waarde))] <- "Laag"
  ##   result$klasse[waarde < -0.75 & !(is.na(waarde))] <- "Te laag"
  ## }else{
  ##   result$klasse[waarde > 1.0] <- "Te hoog"
  ##   result$klasse[waarde <= 1.0 & waarde > 0.5 & !(is.na(waarde))] <- "Hoog"
  ##   result$klasse[waarde <= 0.5 & waarde >= -0.5 & !(is.na(waarde))] <- "Geen afwijking"
  ##   result$klasse[waarde < -0.5 & waarde >= -1.0 & !(is.na(waarde))] <- "Laag"
  ##   result$klasse[waarde < -1.0 & !(is.na(waarde))] <- "Te laag"        
  ## }
  ## result$klasse[is.na(waarde)] <- "Niet bekend"
  
  ## debug
  # zdf <- df.plotz2
  # jaartal <- 2010
  # jaartal <- 2005
  # ptype <- "Macrofauna"
  
  ##==== dataselectie =================================
  
  # selectie van tijdperiode
  subdf <- zdf %>% 
    filter(jaar == jaartal) %>%
    filter(soorttype == ptype)
  
  # alleen als er een locatie meegegeven is
  if(!is.null(locaties)){
    subdf <- subdf %>% filter(locname %in% locaties)  # of loccode
  }
  
  ## pas de ordening van de levels van de klasse kolom aan, zodat de legenda netjes weergegeven wordt. 
  subdf$klasse <- as.factor(as.character(subdf$klasse))
  subdf$klasse <- factor(subdf$klasse, levels = c("Te hoog", "Hoog", "Geen afwijking", "Laag", "Te laag", "Niet bekend"))
  
  if(ptype %in% c("oeverplanten", "Diatomeeen", "Macrofauna", "Macrofauna_V2", "Vissen")){
       plot_label <- c(expression("Te hoog (">="1.0)"), "Hoog (0.5 - 1.0)", "Geen afwijking (-0.5 - 0.5)", "Laag (-1 - -0.5)", expression("Te laag ("<="-1.0)", "Niet bekend"))
  }
  
  if(ptype %in% c("waterplanten")){
      plot_label <- c(expression("Te hoog (">="0.75)"), "Hoog (0.5 - 0.75)", "Geen afwijking (-0.5 - 0.5)", "Laag (-0.75 - -0.5)", expression("Te laag ("<="-0.75)", "Niet bekend"))
  }  
  
  ##======== basisplot ================================
  
  
  p <- subdf %>%
    ggplot(aes(x = locname, y = Parameter, z = Zwaarde)) +
    geom_tile(aes(fill = klasse)) +
    geom_text(aes(label = round(Zwaarde,2)), size = 4.5) +
    scale_fill_manual(values = cols, name = "Klasse",
                      labels = plot_label, drop = FALSE) + 
    #facet_grid(parametergroep~., scales = "free_y", space = "free_y", strip.position = C("top"))
    facet_wrap("parametergroep", ncol = 1, scales = "free_y")
  
  p <- p + plotstijl
  
  
  ## bereken handmatig hoe groot elke plot moet zijn
  ## met facet_wrap krijg je anders dat elke facet dezelfde grootte is wordt
  ## facet_grid is onhandig omdat de titels van de facets dan alleen aan de zijkant weergegeven kunnen worden
  library(plyr)
  N = dlply(subdf, .(parametergroep), function(x) length(row.names(x)))
  g1 = ggplotGrob(p) 
  panels1 <- g1$layout$t[grepl("panel", g1$layout$name)]
  g1$heights[panels1] <- unit(N, "null")
  
  library(grid)
  grid.newpage()
  grid.draw(g1)
  
  return(g1)
  
}



#' plot locaties op kaart
#' @param zdf dataframe met z-waarden
#' @param datum datum van opname
#' @param locatie locatie van opname
#' @return een plot object
plotZwaardeLocaties <- function(zdf, zpar, zjaar, ptype){
  library(sp)
  # library(rgdal)     # reading shapefiles
  # library(rmapshaper)
  # https://gis.stackexchange.com/questions/206684/visualizing-large-datasets-of-polygons-with-leaflet
  
  # functie voor het op een kaart plotten van abiotiek, berekend uit de opname van waterplanten, macrofyten, diatomeen of beestjes
  # 
  # Input: dataframe met abiotiekwaarden
  # 
  # | COLUMNS  |  sample 1 /rec 1 | sample 1 / rec 2 | sample 2 /rec 1 | etc
  # ---------voorbeeldinvoer----------------------------------------------------------------
  # | smocode  | 
  # | reftype  |
  # | extref   | 
  # | locname  | 
  # | loccode  | 
  # | datesmp  | 
  # | locX     |  
  # | locY     |
  # | soortnaam 
  # | abiotparam
  # | waarde
  
  # zdf <- df.plotz2
  # zpar <- "Diepte"
  # zjaar <- 2007
  # ptype <- "waterplanten"
  
  
  ## waterlichamen <- readOGR(dsn = "maplayers", 
  ##                       layer = "OWL_20160824")
  ## leaflet(waterlichamen) %>% addPolygons(color = "#444444")
  ## object.size(waterlichamen)
  ## 
  ## simplified <- rmapshaper::ms_simplify(waterlichamen)
  ## object.size(simplified)
  
  df <- zdf %>%
    filter(Parameter == zpar) %>%
    filter(jaar == zjaar) %>%
    filter(soorttype == ptype) # %>%
    # group_by(locname, locX, locY, Parameter, klasse) # %>%
    # summarise(gemiddelde = mean(Zwaarde))
  
  coordinates(df) <- (~locX+locY)
  proj4string(df) <- CRS("+init=epsg:28992")
  #CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs")
  df.wgs <- spTransform(df, CRS("+proj=longlat +datum=WGS84"))
  
  # pal <- colorNumeric(palette = c("#0C2C84", "#41B6C4", "#FFFFCC"), domain = df.wgs@data$gemiddelde, na.color = "transparent")
  pal <- colorFactor(palette = c("lightsalmon", "khaki", "palegreen", "paleturquoise", "cornflowerblue", "gainsboro"), 
                     levels = c("Te hoog", "Hoog", "Geen afwijking", "Laag", "Te laag", "Niet bekend"))

  if(ptype %in% c("oeverplanten", "Diatomeeen", "Macrofauna", "Macrofauna_V2", "Vissen")){
    plot_label <- c("Te hoog (>=1.0)", "Hoog (0.5 - 1.0)", "Geen afwijking (-0.5 - 0.5)", "Laag (-1 - -0.5)", "Te laag (<=-1.0)", "Niet bekend")
  }
  
  if(ptype %in% c("waterplanten")){
    plot_label <- c("Te hoog (>= 0.75)", "Hoog (0.5 - 0.75)", "Geen afwijking (-0.5 - 0.5)", "Laag (-0.75 - -0.5)", "Te laag (<=-0.75)", "Niet bekend")
  }  
  
  # leaflet(df.wgs) %>%
  #   addTiles() %>%
  #   addCircleMarkers(color = "grey", 
  #                    fill = ~pal(klasse), opacity = 1,
  #                    fillColor = ~pal(klasse), 
  #                    fillOpacity = 1, weight = 1,
  #                    label = ~paste(locname, "z =", round(Zwaarde, 2))) %>%
  #   addLegend("bottomright", pal = pal, values = ~klasse,
  #             labels = plot_label,
  #             title = "Z-waarde",
  #             opacity = 1)
  
  leaflet(df.wgs) %>%
    addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png') %>%
    addCircleMarkers(color = "grey", 
                     fill = ~pal(klasse), opacity = 1,
                     fillColor = ~pal(klasse), 
                     fillOpacity = 1, weight = 1,
                     label = ~paste(locname, "z =", round(Zwaarde, 2))) %>%
    addLegend("bottomright", colors = c("lightsalmon", "khaki", "palegreen", "paleturquoise", "cornflowerblue", "gainsboro"), values = ~klasse,
              labels = plot_label,
              title = "Z-waarde",
              opacity = 1)

}



#' Tel het aantal facetten in een ggplot
#' @p ggplot met facetten
#' @return het aantal facetten in een ggplot
gg_facet_nrow <- function(p){
  p %>% ggplot2::ggplot_build()  %>%
    magrittr::extract2('layout')       %>%
    magrittr::extract2('layout') %>%
    magrittr::extract2('ROW')          %>%
    unique()                           %>%
    length()
}

#' Tel het aantal facetten in een ggplot
#' @p ggplot met facetten
#' @return het aantal facetten in een ggplot
gg_facet_ncol <- function(p){
  p %>% ggplot2::ggplot_build()  %>%
    magrittr::extract2('layout')       %>%
    magrittr::extract2('layout') %>%
    magrittr::extract2('COL')          %>%
    unique()                           %>%
    length()
}
