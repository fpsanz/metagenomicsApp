library(tidyverse)
load("./data/muleroControlesFamily.RData")

variables <- names(exper)
varsel <- variables[3] #esto sería las variables que elige el usuario
exper <- exper %>% unite( !!(paste0(varsel,collapse = ".")) , varsel, sep = ".", remove = F )
if(length(varsel)==1){
  factores <- exper[ ,varsel ] %>% as.factor()
  varname <- varsel
}else{
  factores <- exper[ ,paste0(varsel,collapse = ".") ] %>% as.factor()
  varname <- paste0(varsel,collapse = ".") 
}

####
# cargar fichero de abundancias
otu <- as.matrix(read.delim("../../analisisInvestigadores/20210625_mulero_metagenomica/controles/OTU_family.txt", row.names=1))
attr(otu, "dimnames")[[2]] <- gsub("^S[0-9].{2}_",
                                   "", 
                                   attr(otu, "dimnames")[[2]] ) %>%
  gsub("_v1|_v2", "" , .)
#eliminar 2 muestras
otu <- as.data.frame(otu)
otuF <- otu %>% select(-c("129","146"))
# generar vector id muestras igual que en exp1 y asignar a otu
samples <- as.character(exper$sampleId)
# ordenar otu como exp1
otu <- as.matrix( as.data.frame(otu) %>% select(as.character(exper$sampleId)))
# dividir cada valor por su abundancia
norm <- apply(otu, 2, sum)
norm <- diag(1/norm)
otuNorm <- otu %*% norm
attr(otuNorm,"dimnames")[[2]] <- as.character(exper$SampleId)
# extrar factores del tratamiento
group <- factores
samplesdf <- data.frame(group=group)
rownames(samplesdf) <- samples
#crear objeto phyloseq
library(phyloseq)
otuphyl <- phyloseq( otu_table(otu, taxa_are_rows = T ), sample_data( samplesdf ) )
otuphylNorm <- phyloseq( otu_table(otuNorm, taxa_are_rows = T ), sample_names( samplesdf ) )
#convertir a deseq2 con las dos tratamientos que se quiera
# convertir a deseq
diagdds12 = phyloseq_to_deseq2(otuphyl, ~ group)
#estimar SizeFactor
library(DESeq2)
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
  res[[i]] <- cbind( res[[i]], otu[, muestrasContraste]) %>% as.data.frame()
}

#######################
nombres <- names(exper)[which( names(exper) != "sampleId")]
choices <- unlist(lapply(nombres, function(x){paste0(x,".",unique(exper[x])[[1]] ) } ))
chorizos <- choices[c(7,9)]
exper2 <- exper
for(choice in chorizos){
  kkita <- unlist(strsplit(choice,"[.]"))
  exper2 <- exper2 %>% filter( !!rlang::sym(kkita[1]) == kkita[2] )
}
exper2





