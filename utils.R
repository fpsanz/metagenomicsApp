jscode <- "shinyjs.refresh = function() { history.go(0); }"

conectarBBDD <- function(usuario){
  con <- DBI::dbConnect(SQLite(), dbname = "./ddbb/usuarios.db")
  resdf <- dbGetQuery(con, paste0("Select * from usuarios where usuario ='",usuario,"';") )
  dbDisconnect(con)
  return(resdf)
}

#############rescontrastes######################
rescontrastes <- function(otu, exper, variablesUsuario){
  variables <- names(exper)
  varsel <- variablesUsuario
  exper <- exper %>% unite( !!(paste0(varsel,collapse = ".")) , varsel, sep = ".", remove = F )
  if(length(varsel)==1){
    factores <- exper[ ,varsel ] %>% as.factor()
    varname <- varsel
  }else{
    factores <- exper[ ,paste0(varsel,collapse = ".") ] %>% as.factor()
    varname <- paste0(varsel,collapse = ".") 
  }
  otu <- as.data.frame(otu)
  samples <- as.character(exper$sampleId)
  # ordenar otu como exp1
  otu <- as.matrix( as.data.frame(otu) %>% select(as.character(exper$sampleId)))
  # dividir cada valor por su abundancia
  norm <- apply(otu, 2, sum)
  norm <- diag(1/norm)
  otuNorm <- otu %*% norm
  attr(otuNorm,"dimnames")[[2]] <- as.character(exper$sampleId)
  # extrar factores del tratamiento
  group <- factores
  samplesdf <- data.frame(group=group)
  rownames(samplesdf) <- samples
  #crear objeto phyloseq
  otuphyl <- phyloseq( otu_table(otu, taxa_are_rows = T ), sample_data( samplesdf ) )
  otuphylNorm <- phyloseq( otu_table(otuNorm, taxa_are_rows = T ), sample_data( samplesdf ) )
  #convertir a deseq2 con las dos tratamientos que se quiera
  # convertir a deseq
  diagdds12 = phyloseq_to_deseq2(otuphyl, ~ group)
  #estimar SizeFactor
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  geoMeans = apply(counts(diagdds12), 1, gm_mean)
  diagdds12 = estimateSizeFactors(diagdds12, geoMeans = geoMeans)
  #calcular abundancia diferencial
  diagdds12 = DESeq(diagdds12, fitType="local")
  
  listcontr <- combn(levels(group),2, simplify = F)
  res <- list()
  res <- lapply(listcontr, function(x){results(diagdds12, c("group", x ) ) } )
  names(res) <- lapply(listcontr, function(x){paste0(x, collapse = ".")})
  
  for(i in seq_len( length(res))) {
    muestrasContraste <- exper$sampleId[ exper[,varname] %in% listcontr[[i]] ]
    res[[i]] <- cbind( res[[i]], otu[, muestrasContraste]) %>% as.data.frame() %>% filter(baseMean != 0)
  }
  return(list(res = res, otuphyl=otuphyl, otuphylNorm = otuphylNorm) )
}

generateChoices <- function(exper){
  nombres <- names(exper)[which( names(exper) != "sampleId")]
  choices <- unlist(lapply(nombres, function(x){paste0(x,".",unique(exper[x])[[1]] ) } ))
  return(choices)  
  # exper2 <- exper
  # for(choice in chorizos){
  #   kkita <- unlist(strsplit(choice,"[.]"))
  #   exper2 <- exper2 %>% filter( !!rlang::sym(kkita[1]) == kkita[2] )
  # }
  # return(exper2$sampleId)
}

samples2Remove <- function(exper, selectcriteria){
  if(!is.null(selectcriteria)){
  exper2 <- exper
  for(choice in selectcriteria){
    kkita <- unlist(strsplit(choice,"[.]"))
    exper2 <- exper2 %>% filter( !!rlang::sym(kkita[1]) == kkita[2] )
  }}else{
    exper2 <- exper
    exper2$sampleId <- NULL
  }
  return(exper2$sampleId)
}

#########createSelectedVars#######################
createSelectedVars <- function(exper, variablesUsuario){
  varsel <- variablesUsuario
  exper <- exper %>% unite( !!(paste0(varsel,collapse = ".")) , varsel, sep = ".", remove = F )
  if(length(varsel)==1){
    factores <- exper[ ,varsel ] %>% as.factor()
    varname <- varsel
  }else{
    factores <- exper[ ,paste0(varsel,collapse = ".") ] %>% as.factor()
    varname <- paste0(varsel,collapse = ".") 
  }
  return(exper)
}

########abundanceBoxplot########################
abundanceBoxplot <- function(datos, variableUsuario){
  varsel <- paste0(variableUsuario,collapse = ".")
  otu <- datos$otufiltered
  exper <- datos$experfiltered
  grupos <- levels(factor( exper[,varsel] ))
  kk <- otu %>% as.data.frame %>% rownames_to_column(var="taxa")
  #samplesGrupo <- exper$sampleId[ variableUsuario ]
  samplesGrupo <- exper$sampleId
  topkk <- kk %>% rowwise %>% 
    mutate( mean = mean(c_across(samplesGrupo ))) %>%
    as.data.frame() %>% 
    top_n(wt = mean, n = 25) %>% 
    select(-mean)
  otulong <- topkk %>% 
    pivot_longer(cols = !starts_with("taxa"), names_to = "sample", values_to = "abundance")
  otulong <- left_join(otulong, exper, by = c("sample"="sampleId"))
  p <- otulong %>% 
    ggplot(aes(x=taxa, y=abundance,
               fill = as.factor(!!sym(varsel)) ,
               color= as.factor(!!sym(varsel))))  +
    geom_boxplot(alpha=0.4, width=0.5, position = position_dodge(width = 0.9))+
    scale_y_continuous(trans = "log2")+
    theme(axis.text.x = element_text(angle = 90),
          text = element_text(size=14))+
    labs(color ="Age", fill = "Age" )+
    ylab("Abundance (log2)")+
    facet_wrap(sym(varsel), ncol = 2)
  return(p)
}
## barplot group ##@###################
barplotGrupo <- function(datos, variableUsuario){
  varsel <- paste0(variableUsuario,collapse = ".")
  otuphylNorm <- datos$otuphylNorm
  p <- psmelt(otuphylNorm) %>% group_by(OTU, group) %>%
    summarise(Abundance = mean(Abundance))
  p$Abundance <- p$Abundance*100
  p <- p %>% mutate(menor01 = ifelse(Abundance<1, 1, 0)) %>%
    mutate(newOTU = ifelse(menor01 == 1, "ETC<1%", OTU) ) %>% 
    group_by(newOTU, group) %>% summarise(Abundance2 = sum(Abundance))
  names(p) <- c("OTU","group","Abundance")
  colores <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
  colorRamps <- scales::colour_ramp(colores)
  coloresRamp <- colorRamps(seq(0,1,length=length(unique(p$OTU)) ))
  p <- p %>% ggplot(aes_string(x = "group", y="Abundance", fill="OTU")) +
    geom_bar(stat = "identity", position = "stack", 
             color = "black", show.legend = T, width = 0.5, size=0.01 ) +
    scale_fill_manual(values= coloresRamp ) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "right",
          text = element_text(size=14),
          legend.key.size = unit(0.5, "cm") )+
    guides(fill = guide_legend(ncol=1)) 
  return(p)
}
##barplot samples ##########################
barplotSample <- function(datos, selectvariable, sampleSelection){
  varsel <- paste0(selectvariable, collapse = ".")
  otuphylNorm <- datos$otuphylNorm
  if(length(sampleSelection)>10){sampleSelection<-sampleSelection[1:10]}
  p <- psmelt(otuphylNorm) %>% filter(Sample %in% sampleSelection ) %>% 
    group_by(OTU, group, Sample) %>%
    summarise(Abundance = mean(Abundance))
  p$Abundance <- p$Abundance*100
  p <- p %>% mutate(menor01 = ifelse(Abundance<1, 1, 0)) %>%
    mutate(newOTU = ifelse(menor01 == 1, "ETC<1%", OTU) ) %>% 
    group_by(newOTU, group, Sample) %>% summarise(Abundance2 = sum(Abundance))
  names(p) <- c("OTU","group","Sample","Abundance")
  colores <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
  colorRamps <- scales::colour_ramp(colores)
  coloresRamp <- colorRamps(seq(0,1,length=length(unique(p$OTU)) ))
  p <- p %>% ggplot(aes_string(x = "Sample", y="Abundance", fill="OTU")) +
    geom_bar(stat = "identity", position = "stack", 
             color = "black", show.legend = T, width = 0.5, size=0.01 ) +
    scale_fill_manual(values= coloresRamp ) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "right",
          text = element_text(size=14),
          legend.key.size = unit(0.5, "cm") )+
    guides(fill = guide_legend(ncol=1)) 
  return(p)
}

