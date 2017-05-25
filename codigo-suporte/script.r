library(plyr)
library(ggplot2)
library(scales) # to access break formatting functions
#library(pdf)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

#Alterar esse diretório para o que está meus dados
setwd("C:/Users/savio/Google Drive/estudo/PD360_paradigmas_programacao_paralela/exercicios/04_10_scheduling/pd36o-aula-011-pratica-for-scheduling-cod/src/codigo-suporte");

# Load data about OMP, CUDA, OMP_OFF. Mudar esse aqui também!!! <<<<<<<<<<
data = read.csv("C:/Users/savio/Google Drive/estudo/PD360_paradigmas_programacao_paralela/exercicios/04_10_scheduling/pd36o-aula-011-pratica-for-scheduling-cod/src/codigo-suporte/final.csv");
View(data)

#data <- subset(data,schedule=="DYNAMIC")

cdata <- ddply(data, c("exp","version", "schedule" , "chunk_size" , "num_threads" ,  "size_of_data"), summarise,
                   N    = length(chunk_size),
                   mean_orig = mean(ORIG),
                   mean_omp = mean(OMP),
                   
                   sd_orig = 2 * sd(ORIG),
                   sd_omp = 2 * sd(OMP),
                   
                   se_orig = sd_orig / sqrt(N),
                   se_omp = sd_omp / sqrt(N),
                   
                   mean_plot_value = 0.0,
                   se_plot_value = 0.0,
                   sd_plot_value = 0.0
)

View(cdata)

# Prepare column to plot.
cdata$mean_plot_value  <- ifelse(cdata$version == "OMP", cdata$mean_omp, ifelse(cdata$version == "OMP+OFF", cdata$mean_omp_off, cdata$mean_cuda))
cdata$sd_plot_value  <- ifelse(cdata$version == "OMP", cdata$sd_omp, ifelse(cdata$version == "OMP+OFF", cdata$sd_omp_off, cdata$sd_cuda))
cdata$se_plot_value  <- ifelse(cdata$version == "OMP", cdata$se_omp, ifelse(cdata$version == "OMP+OFF", cdata$se_omp_off, cdata$se_cuda))

#test <-subset(cdata, version == "OMP+OFF")
#write.csv(test, file = "gemm-execucoes-nao-alcancaram-ponto-decisao.csv")

#    exp     size_of_data   NI   NJ   NK  N  mean_orig mean_cuda      sd_orig    sd_cuda      se_orig
df_data = data.frame(x=cdata$num_threads, y=cdata$chunk_size, z=cdata$mean_orig, z_se=cdata$se_orig, z_sd=cdata$sd_orig, t=cdata$mean_plot_value, t_se=cdata$se_plot_value, t_sd=cdata$sd_plot_value, cat=cdata$version)
df_data$x = as.factor(df_data$x)
df_data$y = as.factor(df_data$y)

View(df_data)
# Chunk size 16.
# Chunk size 0 is CUDA version.
# df_plot_16 <- subset(df_data, y== 16 | y == 0)
df_plot_16 <- subset(df_data, y== 16)

View(df_plot_16)

# write.csv(df_plot, file = "chunk_size_evaluation_df_plot.csv")

# df_plot = read.csv("/dados/rogerio/USP/doutorado/prova-de-conceito/testes-prova-conceito/openmp-hook/experiments/chunk_size_evaluation/graph/chunk_size_evaluation_df_plot.csv");
# View(df_plot)

#pdf(filename="evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-16.pdf", width=1200, height=800);

p1 <- ggplot(df_plot_16, aes(x=x, y=t, fill=cat)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=t-t_sd, ymax=t+t_sd),
                size=.5,    # Thinner lines
                width=.4,
                position=position_dodge(.9)) + 
  xlab("Number of Threads") +
  ylab("Time(ms)") +
  #scale_fill_manual(name="Experiment", # Legend label, use darker colors
  #           breaks=c("OMP+OFF", "OMP"),
  #           labels=c("OMP+OFF", "OMP"), values=c("#494949", "#927080", "#B6B6B6")) +
  ggtitle("Offloading Analysis for gemm (Number of Threads with chunk_size = 16)\n") +
  # scale_y_continuous(trans='log') +
  scale_y_continuous() +
  # scale_y_log10() +
  theme_bw() +
  #theme(legend.position=c(0.89,0.70), legend.title=element_blank())
  theme(legend.position=c(0.9,0.89), legend.title=element_blank(), plot.title = element_text(size=20))

(p1 = p1 + scale_fill_grey(start = 0.9, end = 0.2))

multiplot(p1, cols=1)
dev.copy2pdf(file = "evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-16.pdf");
# dev.off ();

# Chunk size 32.
#df_plot_32 <- subset(df_data, y== 32 | y == 0)
df_plot_32 <- subset(df_data, y== 32)

View(df_plot_32)

# write.csv(df_plot, file = "chunk_size_evaluation_df_plot.csv")

# df_plot = read.csv("/dados/rogerio/USP/doutorado/prova-de-conceito/testes-prova-conceito/openmp-hook/experiments/chunk_size_evaluation/graph/chunk_size_evaluation_df_plot.csv");
# View(df_plot)

#pdf(filename="evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-32.pdf", width=1200, height=800);

p2 <- ggplot(df_plot_32, aes(x=x, y=t, fill=cat)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=t-t_sd, ymax=t+t_sd),
                size=.5,    # Thinner lines
                width=.4,
                position=position_dodge(.9)) + 
  xlab("Number of Threads") +
  ylab("Time(ms)") +
  #scale_fill_manual(name="Experiment", # Legend label, use darker colors
  #           breaks=c("OMP+OFF", "OMP"),
  #           labels=c("OMP+OFF", "OMP"), values=c("#494949", "#927080", "#B6B6B6")) +
  ggtitle("Offloading Analysis for gemm (Number of Threads with chunk_size = 32)\n") +
  # scale_y_continuous(trans='log') +
  scale_y_continuous() +
  # scale_y_log10() +
  theme_bw() +
  #theme(legend.position=c(0.89,0.70), legend.title=element_blank())
  theme(legend.position=c(0.9,0.89), legend.title=element_blank(), plot.title = element_text(size=20))

(p2 = p2 + scale_fill_grey(start = 0.9, end = 0.2))

multiplot(p2, cols=1)
dev.copy2pdf(file = "evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-32.pdf");
# dev.off ();

# Chunk size 64.
# df_plot_64 <- subset(df_data, y== 64 | y == 0)
df_plot_64 <- subset(df_data, y== 64)

View(df_plot_64)

# write.csv(df_plot, file = "chunk_size_evaluation_df_plot.csv")

# df_plot = read.csv("/dados/rogerio/USP/doutorado/prova-de-conceito/testes-prova-conceito/openmp-hook/experiments/chunk_size_evaluation/graph/chunk_size_evaluation_df_plot.csv");
# View(df_plot)
#pdf(filename="evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-32.pdf", width=1200, height=800);

p3 <- ggplot(df_plot_64, aes(x=x, y=t, fill=cat)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=t-t_sd, ymax=t+t_sd),
                size=.5,    # Thinner lines
                width=.4,
                position=position_dodge(.9)) + 
  xlab("Number of Threads") +
  ylab("Time(ms)") +
  #scale_fill_manual(name="Experiment", # Legend label, use darker colors
  #           breaks=c("OMP+OFF", "OMP"),
  #           labels=c("OMP+OFF", "OMP"), values=c("#494949", "#927080", "#B6B6B6")) +
  ggtitle("Offloading Analysis for gemm (Number of Threads with chunk_size = 64)\n") +
  # scale_y_continuous(trans='log') +
  scale_y_continuous() +
  # scale_y_log10() +
  theme_bw() +
  #theme(legend.position=c(0.89,0.70), legend.title=element_blank())
  theme(legend.position=c(0.9,0.89), legend.title=element_blank(), plot.title = element_text(size=20))

(p3 = p3 + scale_fill_grey(start = 0.9, end = 0.2))

multiplot(p3, cols=1)
#dev.copy2pdf(file = "evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-64.pdf");
# dev.off ();

# Chunk size 128.
#df_plot_128 <- subset(df_data, y== 128 | y == 0)
df_plot_128 <- subset(df_data, y== 128)

View(df_plot_128)

# write.csv(df_plot, file = "chunk_size_evaluation_df_plot.csv")

# df_plot = read.csv("/dados/rogerio/USP/doutorado/prova-de-conceito/testes-prova-conceito/openmp-hook/experiments/chunk_size_evaluation/graph/chunk_size_evaluation_df_plot.csv");
# View(df_plot)

#pdf(filename="evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-128.pdf", width=1200, height=800);

p4 <- ggplot(df_plot_128, aes(x=x, y=t, fill=cat)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=t-t_sd, ymax=t+t_sd),
                size=.5,    # Thinner lines
                width=.4,
                position=position_dodge(.9)) + 
  xlab("Number of Threads") +
  ylab("Time(ms)") +
  #scale_fill_manual(name="Experiment", # Legend label, use darker colors
  #           breaks=c("OMP+OFF", "OMP"),
  #           labels=c("OMP+OFF", "OMP"), values=c("#494949", "#927080", "#B6B6B6")) +
  ggtitle("Offloading Analysis for gemm (Number of Threads with chunk_size = 128)\n") +
  # scale_y_continuous(trans='log') +
  scale_y_continuous() +
  # scale_y_log10() +
  theme_bw() +
  #theme(legend.position=c(0.89,0.70), legend.title=element_blank())
  theme(legend.position=c(0.9,0.89), legend.title=element_blank(), plot.title = element_text(size=20))

(p4 = p4 + scale_fill_grey(start = 0.9, end = 0.2))

multiplot(p4, cols=1)
#dev.copy2pdf(file = "evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-128.pdf");
# dev.off ();

# Chunk size 256.
# df_plot_256 <- subset(df_data, y== 256 | y == 0)
df_plot_256 <- subset(df_data, y== 256)

View(df_plot_256)

# write.csv(df_plot, file = "chunk_size_evaluation_df_plot.csv")

# df_plot = read.csv("/dados/rogerio/USP/doutorado/prova-de-conceito/testes-prova-conceito/openmp-hook/experiments/chunk_size_evaluation/graph/chunk_size_evaluation_df_plot.csv");
# View(df_plot)

#pdf(filename="evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-256.pdf", width=1200, height=800);

p5 <- ggplot(df_plot_256, aes(x=x, y=t, fill=cat)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=t-t_sd, ymax=t+t_sd),
                size=.5,    # Thinner lines
                width=.4,
                position=position_dodge(.9)) + 
  xlab("Number of Threads") +
  ylab("Time(ms)") +
  #scale_fill_manual(name="Experiment", # Legend label, use darker colors
  #           breaks=c("OMP+OFF", "OMP"),
  #           labels=c("OMP+OFF", "OMP"), values=c("#494949", "#927080", "#B6B6B6")) +
  ggtitle("Offloading Analysis for gemm (Number of Threads with chunk_size = 256)\n") +
  # scale_y_continuous(trans='log') +
  scale_y_continuous() +
  # scale_y_log10() +
  theme_bw() +
  #theme(legend.position=c(0.89,0.70), legend.title=element_blank())
  theme(legend.position=c(0.9,0.89), legend.title=element_blank(), plot.title = element_text(size=20))

(p5 = p5 + scale_fill_grey(start = 0.9, end = 0.2))

multiplot(p5, cols=1)
#dev.copy2pdf(file = "evaluating-chunk_size-benchmark-gemm-data-extralarge_dataset-num_threads-1-a-24-dynamic-chunk_size-256.pdf")
# dev.off ();

