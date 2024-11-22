
#---------------------------------------------------
LXimpala <- function(data_file,data_type){

#------安装R包--------------------------------------
installed_packs <- installed.packages()[,1]
packs <- c("openxlsx","dplyr","purrr","tidyverse","ggplot2")
not_packs <- packs[!packs %in% installed_packs]

if(length(not_packs)>0){
  packs_fun <- function(i){install.packages(i)}
  sapply(not_packs,packs_fun)
 }

lib_fun <- function(i){library(i,character.only = T)}
sapply(packs,lib_fun)

bio_pack <- c("limma")
not_bio <- bio_pack[!bio_pack %in% installed_packs]

if(length(not_bio)>0){
  bio_fun <- function(i){BiocManager::install(i)}
  sapply(not_bio,bio_fun)
}

lib_fun <- function(i){library(i,character.only = T)}
sapply(bio_pack,lib_fun)

#------建文件夹----------------------------------
dir_imp <- paste0("impala_analysis_results_",Sys.Date())
if(!dir.exists(dir_imp))
  dir.create(dir_imp)

#------读取文件-------------------------------
df <- read.csv(data_file)
kegg_pathways <- dplyr::filter(df,df$pathway_source=="KEGG")
kegg_pathways$pathway_name <- str_extract(kegg_pathways$pathway_name,".*(?= -)")

if(grepl("gene",data_type,ignore.case = T))
kegg_pathways <- dplyr::filter(kegg_pathways,num_overlapping_genes>0 & num_overlapping_metabolites>0)

if(grepl("pro",data_type,ignore.case = T)){
  kegg_pathways <- dplyr::filter(kegg_pathways,num_overlapping_genes>0 & num_overlapping_metabolites>0)
  names(kegg_pathways) <- c("pathway_name","pathway_source","num_overlapping_proteins","overlapping_proteins",
                            "num_all_pathway_proteins","P_proteins", "Q_proteins", "num_overlapping_metabolites",
                            "overlapping_metabolites","num_all_pathway_metabolites","P_metabolites","Q_metabolites",
                            "P_joint","Q_joint")
    }

if(grepl("gene",data_type,ignore.case = T)){
  file.txt <- "Gene-Metabolite KEGG Joint Pathways"
  title.txt <- "Gene-Metabolite Joint Pathways" } else {
       if(grepl("pro",data_type,ignore.case = T)){
           file.txt <- "Protein-Metabolite KEGG Joint Pathways"
          title.txt <- "Protein-Metabolite Joint Pathways"} else
              {file.txt <- "Metabolite KEGG Pathways"
                title.txt <- "Metabolite KEGG Pathways" }
     }

write.xlsx(kegg_pathways,paste0(dir_imp,"/",file.txt,".xlsx"))

if(nrow(kegg_pathways)>20){
  kegg_df <- kegg_pathways[c(1:20),]
  title_text <- paste("Top 20",title.txt)} else
  {kegg_df <- kegg_pathways
  title_text <- title.txt}

kegg_mytheme<-theme_bw()+
  theme(text=element_text(family = "sans",colour ="black",face="bold",size =10),
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 0.8,colour = "gray30"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.6,colour = "gray30"),
        axis.ticks.length = unit(1.5,units = "mm"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))+
  theme(panel.grid =element_line(colour="#dcdcdc",linewidth=0.2,linetype = "dashed"))

kegg_xytheme <-theme(axis.text.x = element_text(face="bold",color="black",size=10,angle =0,hjust=1))+
  theme(axis.text.y = element_text(face="bold",color="black",size=10))+
  theme(legend.text=element_text(face="bold",color="black",size=10))

if(grepl("gene",data_type,ignore.case = T))
  kegg_df$pvalue <- kegg_df$P_joint else
    if(grepl("pro",data_type,ignore.case = T))
      kegg_df$pvalue <- kegg_df$P_joint  else
        kegg_df$pvalue <- kegg_df$P_metabolites

kegg_pathways_plot <- ggplot(kegg_df)+
                      geom_point(aes(x = pvalue,fct_reorder(pathway_name,num_overlapping_metabolites),
                                  color=-log10(pvalue),size=num_overlapping_metabolites))+
  scale_color_gradient2(midpoint = 1,low = "blue",mid = "#ffcc00",high ="#dc143c")+
  labs(x = 'Pvalue', y = '',title=title_text)+
  labs(color="-log10(Pvalue)",size="Overlapping_metabolites")+
  kegg_mytheme+kegg_xytheme

ggsave(paste0(dir_imp,"/",file.txt,".png"),kegg_pathways_plot,width=1200, height =1000, dpi=150,units = "px")

print( paste0("The metabolite KEGG pathways cound be found in the folder of '",dir_imp,"'"))

}



