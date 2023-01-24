# Note: volcano plot is designed to work with the CTRP dataset
# It may not work correctly with other data sets if their row order does 


           
volcano_plot <- function(data,volcanotitle, cc, pcutoff ) {
  data$sig[(data$padj > pcutoff|data$padj=="NA")|(data$logFC < cc)& data$logFC > -1*cc] <- "no"
  data$sig[data$padj <= pcutoff & data$logFC >= cc] <- "up"
  data$sig[data$padj <= pcutoff & data$logFC <= -1*cc] <- "down"
  #up[j]<-length(which(data$sig=="up"))
 # down[j]<-length(which(data$sig=="down"))
  
  
  
  #library(export)
  
  library(RColorBrewer)
  library(ggrepel)
  theme_set(theme_bw())
  par(mar=c(1,1,1,1))
  x_left_limits<-c(0.3,-1.6)
  x_right_limits<-c(0.4,1.3)
  y_limits<-max(-1*log10(data$padj))+3 
  p <- ggplot(data,aes(logFC,-1*log10(data$padj),
                       color = sig))+geom_point()+
    # xlim(-1.6,1.3) +
    labs(x="correlation R",y="-log10(pvalue)")+
    ggtitle(volcanotitle) + 
    theme(plot.title = element_text(hjust = 0.5))+
    # geom_text_repel(aes(label =ifelse(data$logFC >= 0.3 & data$padj <= 0.05|data$logFC <= -0.3 & data$padj <= 0.05,as.character(Drug),'')),
    geom_text_repel(aes(label =ifelse(data$logFC <= -1*cc & data$padj <= pcutoff ,as.character(data$Drug),'')),
                    box.padding = 0.3,
                    point.padding = -0.25,
                    # hjust=0,
                    # nudge_x = -0.35,
                    #  direction = "both",
                    color="purple",
                    # xlim = x_left_limits,
                    segment.size=0.02,
                    size=3,
                    #  force=1,
                    segment.color = 'grey50')
  p<-p+geom_text_repel(aes(label =ifelse(data$logFC >= cc & data$padj <= pcutoff,as.character(data$Drug),'')),
                       # box.padding   = 0.3,
                       #  point.padding = 1,
                       #   hjust=0,
                       #nudge_x = 0.5,
                       #  direction = "both",
                       color="purple",
                       # xlim = x_right_limits,
                       segment.size=0.02,
                       size=3,
                       segment.color = 'grey50')
  
  # geom_text(aes(label=ifelse(data$logFC >= 0.3 |data$logFC <= -0.3 ,as.character(Drug),'')),hjust=1,vjust=-1)
  
  p <- p + scale_color_manual(values =c(down="green",no="grey",up="red"))+
    geom_hline(yintercept=-log10(pcutoff),linetype=3)+
    geom_vline(xintercept=c(-1*cc,cc),linetype=3)
  p <- p +theme(panel.grid =element_blank())+
    #theme(axis.line = element_line(size=0))+ylim(0,y_limits)
    theme(axis.line = element_line(size=0))
  
  p <- p+guides(colour = FALSE)
  p <- p +theme(axis.text=element_text(size=10),axis.title=element_text(size=10))
  print(p)
  
  
      }
