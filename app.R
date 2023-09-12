#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(repos = getOption("repos")["CRAN"])
options(shiny.sanitize.errors=FALSE)
library(shiny)
library(reshape2)
library(hablar)
library(data.table)
library(gridExtra)
library(RColorBrewer)
library(ggrepel)

# Define UI for application that draws a histogram

# Define UI for dataset viewer app ----
ui <- fluidPage(
  navbarPage("GRD",
             tabPanel("Home",
                      mainPanel(
                        h3("About GRD",align="left",
                           style ="font-family: 'times'; font-size: 24pt "),
                        p("GRD (Gene-Related Drugs) is a web server designed to access gene-related drug information from the Cancer Therapeutics Response Portal (CTRP) database. This comprehensive database contains valuable drug-response data and response profiles for 481 compounds across 860 cell lines, complemented by RNA-seq data. GRD offers three distinct types of analyses to empower users in their research.To initiate an analysis, users are required to submit a gene of interest to the 'Gene Related Drugs' module. This step enables users to visualize the relationship between the expression of the submitted gene and drug responses through a powerful tool known as the volcano plot.", style = "font-family: 'times'; font-si22pt",align="justify"),
                        img(src='volcano.jpg',height="50%",width="70%",style="display: block; margin-left: auto; margin-right: auto;"),
                        br(),
                        br(),
                         p("The 'Expression' module in GRD allows users to access the expression levels of the submitted gene across all tumor cell lines within the CTRP database. This information is presented through informative boxplot, illustrating the distribution of gene expression levels. It's important to note that all gene expression values are logarithmically transformed to the base 2 (log2) for enhanced data analysis and interpretation.", style = "font-family: 'times'; font-si22pt",align="justify"),
                        img(src='page.png',height="75%",position="absolute", width="100%"),
                        br(),
                        br(),
                         p("On the 'Fraction' page of GRD, users can gain insights into the proportion of responsive drugs by employing a specific calculation. This calculation involves determining the ratio of drugs with an absolute correlation coefficient exceeding 0.3 and a p-value less than 0.05 across various cell lines. These drugs are categorized into two distinct groups: the 'resistant group' for drugs with correlation coefficients greater than 0.3, and the 'sensitive group' for drugs with correlation coefficients less than -0.3.To effectively visualize this ratio distribution of relevant drugs between these two groups, GRD utilizes a Pyramid plot, which offers a clear and informative representation of the data.", style = "font-family: 'times'; font-si22pt",align="justify"),
                         img(src='fig2.png',height="75%",position="absolute", width="100%"),
                        br(),
                        br(),
                         p("Taken together, this server could aid researchers to find candidate drugs to regulate interested gene expression so that they could design an experiment to study its biological function in vitro. GRD is developed by Yongzi Chen, with all rights reserved. Please contact", span("yzchen@tmu.edu.cn", style = "color:blue"),"for any question or suggestion.", style = "font-family: 'times'; font-si22pt",align="justify"),
                        br(),
                        p("Citation: An Interactive Resource to Identify Cancer Genetic and Lineage Dependencies Targeted by Small Molecules, Basu, Bodycombe, Cheah, et al., Cell, 154, 1151-1161 (2013).",style = "font-family: 'times'; font-si22pt",align="justify"),
                        br(),
                        img(src='copyright.png',height="18%",position="absolute", width="100%"),
                         )
             ),
             tabPanel("Gene Related Drugs",
                      tags$head(
                        img(src='logo.png',height="auto",width="100%"),
                        tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))   ),
                      # Sidebar layout with a input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          #Input the gene 
                          textInput(inputId= "gene",
                                    label="Insert your gene"),
                          # Input: Selector for choosing dataset ----
                          selectInput(inputId = "CancerType",
                                      label = "Choose a Cancer Type:",
                                      choices = c("autonomic_ganglia","biliary_tract","bone","breast","central_nervous_system","endometrium","haematopoietic_and_lymphoid_tissue",
                                                  "kidney","large_intestine","liver","lung","oesophagus","ovary","pancreas","pleura","prostate","salivary_gland","skin","soft_tissue","stomach","thyroid","upper_aerodigestive_tract","urinary_tract","pan-cancer"         
                                      )),
                          sliderInput("mycoefficent", "correlation coefficient to use:", 
                                      min=0.3, max=1.0, value=0.3,step = 0.1),
                          # Input: Numeric entry for pvalue to use ----
                          selectInput(inputId = "mypvalue",
                                      label = "P-Value to use:",
                                      choices = c("0.05", "0.01")),
                          submitButton("Submit", icon=NULL),
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          h3("Introduction",align="left",
                             style ="font-family: 'times'; font-size: 24pt "),
                          p("To initiate an analysis, users should begin by submitting a gene of interest to the 'Gene Related Drugs' module. This action enables users to visualize the association between the expression of the submitted gene and drug response through the use of a volcano plot.Users have the flexibility to tailor their analysis by adjusting the correlation coefficient and p-value parameters to suit their specific needs.  It's important to note that ", span("each time users modify these parameters, they will need to resubmit their query", style = "color:blue"),"in order to generate the personalized plot.",
                             style = "font-family: 'times'; font-si22pt",align="justify"),
                          div(),
                          plotOutput("volcanoPlot"),
                          fluidRow(
                            column(3,uiOutput("downloadData")),
                            column(6,uiOutput("downloadtable")),
                          ),
                          tableOutput("table")
                          
                        )
                      )
             ),


 tabPanel("Expression",
          mainPanel(
            h3("Introduction",align="left",
               style ="font-family: 'times'; font-size: 24pt "),
            p("The 'Expression' module provides users with the ability to visualize the differential expression of the submitted gene across all tumor cell lines within the CTRP database. This information is presented using box plots, which effectively display the distribution of mRNA gene expression levels.It's worth noting that all gene expression values undergo log2 transformation. This enables users to gain a comprehensive understanding of how the gene's expression is distributed across various cancer cell lines, thus providing valuable insights into its behavior in different contexts.",
               style = "font-family: 'times'; font-si22pt",align="justify"),
            div(),
            plotOutput("summary2"),
            br(),
            br(),
            br()
          )
 ),
 tabPanel("Fraction",
          mainPanel(
            h3("Introduction",align="left",
               style ="font-family: 'times'; font-size: 24pt "),
            p("The module calculates the percentage of drugs exhibiting significant positive and negative correlations with the expression of a specified gene. These percentages define two distinct groups: the 'resistant group,' comprising drugs with correlation coefficients greater than 0.3, and the 'sensitive group,' consisting of drugs with correlation coefficients less than -0.3.To offer a clear visualization of the ratio distribution of related drugs between these two groups, the module employs a Pyramid plot, aiding users in gaining insights into the drug-gene relationships.",
              style = "font-family: 'times'; font-si22pt",align="justify"),
            div(),
            
            plotOutput("fraction1")
        #    plotOutput("fraction2")
          )
 )
)
)
Exp.CCL<-read.csv("Data/Exp_CCL.csv")
Drug.ccl<-read.csv("Data/DrugAUC.csv")
#print(colnames(Drug.ccl))
CCL.info<-read.csv("Data/CCLinfo.csv")
#DrugAUC<-read.csv("Data/v21.data.auc_sensitivities.csv")
#meta.compound<-read.csv("Data/v21.meta.per_compound.csv")

#geneID.exp.cclname<-read.csv("Data/geneID.exp.cclname.csv")
#rownames(geneID.exp.cclname)<-geneID.exp.cclname$ccl_name
Typelistraw<-split(CCL.info,CCL.info$ccle_primary_site)
Typelistraw[[length(Typelistraw)+1]]<-CCL.info
names(Typelistraw)[length(Typelistraw)]<-"pan_cancer"
Typelist<-Typelistraw[sapply(Typelistraw, function(x) dim(x)[1]) > 3]

rownames(Drug.ccl)<-Drug.ccl$cpd_name
source("volcanoplot.R")

server <- function(input, output) {
    plotvals <- reactiveValues(volcanoPlot=NULL,ordermydata=NULL)
    
  ######################generate volcanoplot#################  
    
    output$volcanoPlot <- renderPlot({
        CancerData <- switch(input$CancerType,
                      # "Other" = list(Typelist[["Other"]]),
                       "autonomic_ganglia"=list(Typelist[["autonomic_ganglia"]]),
                       "biliary_tract"=list(Typelist[["biliary_tract"]]),
                       "bone"=list(Typelist[["bone"]]),
                       "breast"=list(Typelist[["breast"]]),
                       "central_nervous_system"=list(Typelist[["central_nervous_system"]]),
                       "endometrium"=list(Typelist[["endometrium"]]),
                       "haematopoietic_and_lymphoid_tissue"=list(Typelist[["haematopoietic_and_lymphoid_tissue"]]),
                       "kidney"=list(Typelist[["kidney"]]),
                       "large_intestine"=list(Typelist[["large_intestine"]]),
                       "liver"=list(Typelist[["liver"]]),
                       "lung"=list(Typelist[["lung"]]),
                       "oesophagus"=list(Typelist[["oesophagus"]]),
                       "ovary"=list(Typelist[["ovary"]]),
                       "pancreas"=list(Typelist[["pancreas"]]),
                       "pleura"=list(Typelist[["pleura"]]),
                       "prostate"=list(Typelist[["prostate"]]),
                      # "salivary_gland"=list(Typelist[["salivary_gland"]]),
                       "skin"=list(Typelist[["skin"]]),
                       "soft_tissue"=list(Typelist[["soft_tissue"]]),
                       "stomach"=list(Typelist[["stomach"]]),
                       "thyroid"=list(Typelist[["thyroid"]]),
                       "upper_aerodigestive_tract"=list(Typelist[["upper_aerodigestive_tract"]]),
                       "urinary_tract"=list(Typelist[["urinary_tract"]]),
                       "pan-cancer"=list(Typelist[["pan_cancer"]])
                           )

        genepos<-which(Exp.CCL$gene_primary_name==input$gene)   
        sites<-Exp.CCL$gene_primary_name
        validate(
            need(input$gene %in% sites, 'An offical gene symbol required!')
        )

         Gene.Exp.CCL<-Exp.CCL[genepos,]
#####box plot for CTRP########################################
         destroyX = function(es) {
           f = es
           for (col in c(1:ncol(f))){ #for each column in dataframe
             if (startsWith(colnames(f)[col], "X") == TRUE)  { #if starts with 'X' ..
               colnames(f)[col] <- substr(colnames(f)[col], 2, 100) #get rid of it
             }
           }
           assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
         }
         Gene.Exp.CCL.X<-destroyX(Gene.Exp.CCL)
         rownames(CCL.info)<-CCL.info$ccl_name
         Gene.exp.ccl.info<-na.omit(CCL.info[colnames(Gene.Exp.CCL.X),])
         gene.name<- colnames(Gene.Exp.CCL.X[,-which(colnames(Gene.Exp.CCL.X)=="gene_primary_name")] )
         Gene.exp.ccl.info$geneexpr<-t(Gene.Exp.CCL.X[,gene.name])
         output$summary2 <- renderPlot({
           validate(
             need(input$gene %in% sites, 'An offical gene symbol required!')
           )
           PrimarySite<-as.factor(Gene.exp.ccl.info$ccle_primary_site)
           ggplot(Gene.exp.ccl.info,aes(x=geneexpr,y=as.factor(Gene.exp.ccl.info$ccle_primary_site)),
           ) +geom_boxplot(aes(fill = PrimarySite),position = position_dodge2(preserve = "total"))+ggtitle(paste("mRNA expression(RNAseq):",input$gene ,sep=""))+
             xlab("Log2 normalized expression")+ylab("")+theme(legend.position="right")+theme(legend.key.size = unit(0.5, "cm"))+
             theme(axis.text.x = element_text(face="bold",size = 12,hjust=1),
                   axis.text.y = element_text(face = "bold",size = 12), axis.title=element_text(size=12,face="bold"),
                   plot.title = element_text(color="black", size=14,hjust = 0.5, face="bold"))+
             guides (fill = guide_legend(ncol = 1)) 
           
         }
       )
         
         
#################################################
 
         meta.ccl<-CancerData[[1]]
         id<-intersect(intersect(meta.ccl$ccl_name,colnames(Drug.ccl)),colnames(Gene.Exp.CCL))#cell line ID
         Drug.ccl.cancer<-Drug.ccl[,id]
        # Gene.Exp.CCL.cancer<-Gene.Exp.CCL[,id]
         Gene.Exp.CCL.cancer<-Gene.Exp.CCL[,intersect(colnames(Drug.ccl),colnames(Gene.Exp.CCL))]
         Gene.Exp.CCL.cancer<-sapply(Gene.Exp.CCL.cancer,as.numeric)
         
         
         combine.Drug.ccl.cancer<-rbind(Gene.Exp.CCL.cancer,Drug.ccl.cancer)
         combine.Drug.ccl.cancer.3more<-combine.Drug.ccl.cancer[which(rowSums(!is.na(combine.Drug.ccl.cancer)) >3),]
         cor.result<-apply(combine.Drug.ccl.cancer.3more[-1, ], 1, cor.test, as.numeric(combine.Drug.ccl.cancer.3more[1,]), method="pearson")
         
         results<-data.frame(
           Drug=rownames(combine.Drug.ccl.cancer.3more[-1,]),
           estimate=sapply(cor.result, "[[", "estimate"),
           pvalue= sapply(cor.result, "[[", "p.value")
         )
         
         results<-na.omit(results)
         #volcano plot#########
         results$pvalueneglog10<--1*log10(results$pvalue)
         rownames(results)<-results$Drug
         
         mydata<-results
         mydata$logFC <-mydata$estimate
         mydata$padj <- mydata$pvalue
         
         args<-list(mydata)
        args$volcanotitle<- as.character(input$CancerType)
         args$cc <- input$mycoefficent
         args$pcutoff <- as.numeric(input$mypvalue)
         
         plotvals$volcanoPlot<-do.call(volcano_plot, args)
         
         ordermydata<-subset(mydata, select = c(Drug, estimate,pvalue,pvalueneglog10))
         plotvals$ordermydata<-ordermydata[order(abs(mydata$estimate),decreasing = TRUE),]
         output$table <- renderTable( {
           validate(
             need(input$gene %in% sites, '')
           )
           head(ordermydata,n=10,align = "c")
           })
         
         
####fraction boxplot##############################
         DrugAUC<-data.table(Drug.ccl)
         DrugAUCpre<-DrugAUC[,-1]
         
         up<-vector()
         down<-vector()

  destroyX(Drug.ccl)
  
  for(j in 1:length(Typelist)){
    combine.fra.Drug.ccl.cancer<-data.frame()
    fra.meta.ccl<-Typelist[[j]]
    fra.Drug.ccl<-Drug.ccl[,as.character(fra.meta.ccl$ccl_name)]
    colnames(fra.Drug.ccl)
    fra.Drug.ccl<-as.data.frame(fra.Drug.ccl)
    rownames(fra.Drug.ccl)<-Drug.ccl$cpd_name
    
    if(ncol(fra.Drug.ccl)>0)
    {
      combine.fra.Drug.ccl.cancer<-rbind(Gene.Exp.CCL.cancer,fra.Drug.ccl)
      combine.fra.Drug.ccl.cancer.3more<-combine.fra.Drug.ccl.cancer[which(rowSums(!is.na(combine.fra.Drug.ccl.cancer)) >3),]
      fra.cor.result<-apply(combine.fra.Drug.ccl.cancer.3more[-1, ], 1, cor.test, as.numeric(combine.fra.Drug.ccl.cancer.3more[1,]), method="pearson")
      
      fra.results<-data.frame(
        Drug=rownames(combine.fra.Drug.ccl.cancer.3more[-1,]),
        estimate=sapply(fra.cor.result, "[[", "estimate"),
        pvalue= sapply(fra.cor.result, "[[", "p.value")
      )
      
      fra.results<-na.omit(fra.results)
      #volcano plot#########
      fra.results$pvalueneglog10<--1*log10(fra.results$pvalue)
      rownames(fra.results)<-fra.results$Drug
      fra.data<-fra.results
      
      fra.data$logFC <-fra.data$estimate
      fra.data$padj <- fra.data$pvalue
      
      fra.data$sig[(fra.data$padj > 0.05|fra.data$padj=="NA")|(fra.data$logFC < 0.3)& fra.data$logFC > -0.3] <- "no"
      fra.data$sig[fra.data$padj <= 0.05 & fra.data$logFC >= 0.3] <- "up"
      fra.data$sig[fra.data$padj <= 0.05 & fra.data$logFC <= -0.3] <- "down"
      
      up[j]<-length(which(fra.data$sig=="up"))
      down[j]<-length(which(fra.data$sig=="down"))
    }
  }
  fractionUP<-up/481
  fractionDown<-down/481
  fractionDown<-as.data.frame(fractionDown)
  fractionDown$cancername<-names(Typelist)
  fractionUP<-as.data.frame(fractionUP)
  fractionUP$cancername<-names(Typelist)
  
  fractionUP$Class<-"resistant"
  fractionDown$Class<-"sensitive"
  colnames(fractionUP)[1]<-"fraction"
  colnames(fractionDown)[1]<-"fraction"
  colnames(fractionUP)[2]<-"PrimarySite"
  colnames(fractionDown)[2]<-"PrimarySite"
  fraction<-rbind(fractionUP,fractionDown)
  max<-round(max(fraction$fraction),2)+0.01
  
  
  output$fraction1 <- renderPlot({
    validate(
      need(input$gene %in% sites, 'An offical gene symbol required!')
    )
    ggplot(fraction, aes(x = PrimarySite, 
                         y = ifelse(Class == 'sensitive', fraction, -fraction),
                         fill = Class)) + 
      geom_bar(stat = 'identity') + coord_flip()+ 
      scale_fill_manual(values=c("#6495ED","#FFA500"))+
      scale_y_continuous(limits = c(-max,max),
                         breaks = seq(-max,max,0.06),
                         labels = abs) +
      labs(y = 'Fraction of responsive drugs')+theme(legend.key.size = unit(0.6, "cm"))+
      theme(axis.text.x = element_text(size = 10,hjust=1,face ="bold"),
            axis.text.y = element_text(size=10,face = "bold"),
            axis.title.x  = element_text(color="black", size=12,hjust = 0.5),
            axis.title.y  = element_text(color="black", size=12,hjust = 0.5))
  })
    
  ############table output########## 
  
  ########generate PDF download button######## 
  
  output$downloadData <- renderUI({
    req(input$gene)
    validate(
      need(input$gene %in% sites, '')
    )
    downloadButton('download_item', label = 'Download PDF') 
  })
  output$download_item <- downloadHandler(

    filename = function() {"plots.pdf"},
    content = function(file) {
      pdf(file, onefile = TRUE)
      grid.arrange(plotvals$volcanoPlot) 
      dev.off()
    }
  )
  #################table download button########################
  output$downloadtable <- renderUI({
     req(input$gene)
    validate(
      need(input$gene %in% sites, '')
    )
    downloadButton('download_table', label = 'Download whole results')
  })
  output$download_table <- downloadHandler(
    filename = function() {
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(plotvals$ordermydata, file, row.names = TRUE)
    }
  )
})
}

# Run the application 
shinyApp(ui = ui, server = server)
