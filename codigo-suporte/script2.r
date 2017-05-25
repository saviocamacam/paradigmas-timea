library(plyr)
library(ggplot2)
library(scales) # to access break formatting functions
library(pdf)

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

# Insert rows in data frame.
# Usage: 
# r <- nrow(existingDF) + 1
# newrow <- c(2,3,4,5)
# insertRow(existingDF, newrow, r)
# insertRow(existingDF, c(9,6,3,1), 1)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# Função para Carregar e preparar os dados.
myLoadAndPrepareData<-function(p_experiment_name, p_benchmark_name, p_machine_name, p_csv_file_name)
{
  csv_file_name <- paste0("csv/", p_machine_name, "/", p_csv_file_name)
  # Load data about OMP, CUDA, OMP_OFF.
  data = read.csv(file=csv_file_name, header=TRUE, sep=",")
  View(data)  
  # Converte tempos de nanosegundos (ns) para segundos.
  # nano  1000000000 = 1+e09 (ns)
  # micro 1000000    = 1+e06 (us)
  # mili  1000       = 1+e03 (ms)
  # s     1          = 1+e00 (s)
  
  # data <- within(data, ORIG <- (ORIG/1e+09)*1e+03)
  # data <- within(data, OMP <- (OMP/1e+09)*1e+03)
  #data <- within(data, OMP_OFF <- (OMP_OFF/1e+09)*1e+03)
  #data <- within(data, CUDA_KERNEL1 <- (CUDA_KERNEL1/1e+09)*1e+03)
  #data <- within(data, CUDA_KERNEL2 <- (CUDA_KERNEL2/1e+09)*1e+03)
  #data <- within(data, CUDA_KERNEL3 <- (CUDA_KERNEL3/1e+09)*1e+03)
  #data <- within(data, DT_H2D <- (DT_H2D/1e+09)*1e+03)
  #data <- within(data, DT_D2H <- (DT_D2H/1e+09)*1e+03)
  
  cdata <- ddply(data, c("exp", "version", "schedule", "chunk_size" , "num_threads" , "size_of_data" , "NI" , "NJ" , "NK"), summarise,
                     N    = length(chunk_size),
                     mean_orig = mean(ORIG),
                     mean_omp = mean(OMP),
                     mean_omp_off = mean(OMP_OFF),
                     mean_cuda_kernel_1 = mean(CUDA_KERNEL1),
                     mean_cuda_kernel_2 = mean(CUDA_KERNEL2),
                     mean_cuda_kernel_3 = mean(CUDA_KERNEL3),
                     mean_dt_h2d = mean(DT_H2D),
                     mean_dt_d2h = mean(DT_D2H),
                     # mean_cuda = mean_cuda_kernel_1 + mean_cuda_kernel_2 + mean_cuda_kernel_3 + mean_dt_h2d + mean_dt_d2h,
                     mean_cuda = mean(CUDA_KERNEL1 + CUDA_KERNEL2 + CUDA_KERNEL3 + DT_H2D + DT_D2H),
                     
                     sd_orig = 2 * sd(ORIG),
                     sd_omp = 2 * sd(OMP),
                     sd_omp_off = 2 * sd(OMP_OFF),
                     sd_cuda_kernel_1 = 2 * sd(CUDA_KERNEL1),
                     sd_cuda_kernel_2 = 2 * sd(CUDA_KERNEL2),
                     sd_cuda_kernel_3 = 2 * sd(CUDA_KERNEL3),
                     sd_dt_h2d = 2 * sd(DT_H2D),
                     sd_dt_d2h = 2 * sd(DT_D2H),
                     # sd_cuda = sd_cuda_kernel_1 + sd_cuda_kernel_2 + sd_cuda_kernel_3,
                     sd_cuda = 2 * sd(CUDA_KERNEL1 + CUDA_KERNEL2 + CUDA_KERNEL3 + DT_H2D + DT_D2H),
                     
                     se_orig = sd_orig / sqrt(N),
                     se_omp = sd_omp / sqrt(N),
                     se_omp_off = sd_omp_off / sqrt(N),
                     se_cuda_kernel_1 = sd_cuda_kernel_1 / sqrt(N),
                     se_cuda_kernel_2 = sd_cuda_kernel_2 / sqrt(N),
                     se_cuda_kernel_3 = sd_cuda_kernel_3 / sqrt(N),
                     # se_cuda = se_cuda_kernel_1 + se_cuda_kernel_2 + se_cuda_kernel_3,
                     se_cuda = sd_cuda / sqrt(N),
                     se_dt_h2d = sd_dt_h2d / sqrt(N),
                     se_dt_d2h = sd_dt_d2h / sqrt(N),
                     
                     mean_plot_value = 0.0,
                     se_plot_value = 0.0,
                     sd_plot_value = 0.0,
                     sum_work_finish_before_offload_decision = sum(WORK_FINISHED_BEFORE_OFFLOAD_DECISION),
                     sum_reach_offload_decision_point = sum(REACH_OFFLOAD_DECISION_POINT),
                     sum_decided_by_offloading = sum(DECIDED_BY_OFFLOADING),
                     sum_made_the_offloading = sum(MADE_THE_OFFLOADING)
  )
  
  View(cdata)
  
  # Prepare column to plot.
  cdata$mean_plot_value  <- ifelse(cdata$version == "OMP", cdata$mean_omp, ifelse(cdata$version == "OMP+OFF", cdata$mean_omp_off, cdata$mean_cuda))
  cdata$sd_plot_value  <- ifelse(cdata$version == "OMP", cdata$sd_omp, ifelse(cdata$version == "OMP+OFF", cdata$sd_omp_off, cdata$sd_cuda))
  cdata$se_plot_value  <- ifelse(cdata$version == "OMP", cdata$se_omp, ifelse(cdata$version == "OMP+OFF", cdata$se_omp_off, cdata$se_cuda))
  
  # Comparações de CUDA com OpenMP.
  # cuda_and_omp_with_num_threads_1_and_chunksize_16 <-subset(cdata, version == "CUDA" | (version == "OMP" & num_threads == 1 & chunk_size == 16))
  # View(cuda_and_omp_with_num_threads_1_and_chunksize_16)
  # write.csv(cuda_and_omp_with_num_threads_1_and_chunksize_16, file = "cuda-omp-all-data-sizes-and-chunk-sizes-evaluation-cuda_and_omp_with_num_threads_1_and_chunksize_16.csv")
  # head(cuda_and_omp_with_num_threads_1_and_chunksize_16)
  
  
  #    exp     size_of_data   NI   NJ   NK  N  mean_orig mean_cuda      sd_orig    sd_cuda      se_orig
  df_data <<- data.frame(x=cdata$num_threads, y=cdata$chunk_size, z=cdata$NI, w=cdata$mean_orig, w_se=cdata$se_orig, w_sd=cdata$sd_orig, t=cdata$mean_plot_value, t_se=cdata$se_plot_value, t_sd=cdata$sd_plot_value, cat=cdata$version)
  # df_data$x = as.factor(df_data$x)
  # df_data$y = as.factor(df_data$y)
  df_data$z = as.factor(df_data$z)
  
  df_data
  
  View(df_data)
}


# Função para Plotar gráfico filtrando por num_threads e chunk_size.
# Eixo x: tamanho dos dados.
# Comparar CUDA (num_threads: 1, chunk_size: nenhum) com as execuções OMP(num_threads: 1, 2, ..., chunk_size: 16, 32, 64, 128, 256)
# define a simple function
myFilterAndPlot<-function(p_experiment_name, p_benchmark_name, p_machine_name, p_machine_arch, p_schedule, p_num_threads, p_chunk_size)
{
  graph_title <- paste0("OpenMP Analysis for ", p_benchmark_name)
  graph_parameters <- paste0("(Number of Threads: ", p_num_threads,", with chunk_size: ", p_chunk_size,")")
  
  graph_platform_details <- paste0("exp: ", p_experiment_name, ", machine: ", p_machine_name, ", arch: ", p_machine_arch, ", schedule: ", p_schedule)
  
  plot.title <- paste0(graph_title," ", graph_parameters)
  plot.subtitle <- paste0(graph_platform_details)
  
  graph_file_name <- paste0("graph/", p_machine_name, "/", "graph-omp-", p_experiment_name,"-benchmark-",p_benchmark_name,"-machine-",p_machine_name,"-arch-",p_machine_arch,"-schedule-", p_schedule,"-num_threads-",p_num_threads,"-chunk_size-",p_chunk_size,".pdf")
  
  dir.create(paste0("csv/", p_machine_name, "/generated-csv/"), showWarnings = FALSE)
  csv_file_name <- paste0("csv/", p_machine_name, "/generated-csv/", "data-omp-", p_experiment_name,"-benchmark-",p_benchmark_name,"-machine-",p_machine_name,"-arch-",p_machine_arch,"-schedule-", p_schedule,"-num_threads-",p_num_threads,"-chunk_size-",p_chunk_size,".csv")
  
  # where num_threads = 1 and chunk_size = 64
  df_plot_num_threads_1_chunk_size_64 <- subset(df_data, (cat == "OMP" & x == p_schedule & y == p_num_threads & z == p_chunk_size) | (cat == "OMP" & x == p_num_threads & y == p_chunk_size))
  
  View(df_plot_num_threads_1_chunk_size_64)

  # pdf(file = "teste.pdf", width = 800, height = 600)
  
  par(cex.axis=1, cex.lab=2, cex.main=1.2, cex.sub=1)
  
  p1 <- ggplot(df_plot_num_threads_1_chunk_size_64, aes(x=z, y=t, width=500, fill=cat)) + 
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(x=z,ymin=t-t_sd, ymax=t+t_sd),
                  size=.5,    # Thinner lines
                  width=.4,
                  position=position_dodge(.9)) +
    xlab("Size of Data") +
    ylab("Time(ns)") +
    
    ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) +
    scale_y_continuous(trans='log') +
    
    scale_x_discrete(limits = c(0,32,64,128,256,512,1024,2048,4096,8192)) +
    theme_bw() +
    
    theme(legend.position=c(0.05,0.9), legend.title=element_blank(), plot.title = element_text(size=20))
  
  (p1 = p1 + scale_fill_grey(start = 0.9, end = 0.2))
    
  multiplot(p1, cols=1)
  dev.copy2pdf(file = paste0(graph_file_name));
  # dev.off ();

  #Escreve os dados para um csv.
  write.csv(df_plot_num_threads_1_chunk_size_64, file = paste0(csv_file_name))
}


# Programa principal.
machine_arch <- vector(mode="list", length=3)
names(machine_arch) <- c("pilipili2", "ragserver", "hppca-pacgpu-server01")
machine_arch[[1]] <- "ivy_bridge"; 
machine_arch[[2]] <- "nehalem";
machine_arch[[3]] <- "sandy-bridge-ep"

# Define os dados do experimento.
work_dir_name_base <- paste0("/dados/rogerio/USP/doutorado/prova-de-conceito/testes-prova-conceito/openmp-hook/experiments")
experiment_name <- paste0("all-data-sizes-and-chunk-sizes-evaluation-openmp-vs-openmp-offloading")

# Define o work directory.
work_dir_name <- paste0(work_dir_name_base,"/",experiment_name)

print(paste0("Working on ", work_dir_name))

setwd(work_dir_name);

# call the function with that values.
# num_threads: 24 22 20 18 16 14 12 10 8 6 4 2 1
# chunk_size: 16 32 64 128 256
num_threads_configs <- c(24,22,20,18,16,14,12,10,8,6,4,2,1)
chunk_size_configs <- c(16,32,64,128,256)

# Experimento ragserver.
benchmark_name <- paste0("gemm")
machine_name <- paste0("ragserver")
csv_data_file = "gemm-ragserver-omp-all-data-sizes-and-chunk-sizes-evaluation-09-11-2016-12-16-41-and-gemm-ragserver-openmp-offloading-all-data-sizes-and-chunk-sizes-evaluation-17-11-2016-02-32-35-joined-processed-final.csv"
                
# Carrega os dados.
myLoadAndPrepareData(experiment_name, benchmark_name, machine_name, csv_data_file)

# myFilterAndPlot(1, 32)
myFilterAndPlot(experiment_name, benchmark_name, machine_name, machine_arch[machine_name], p_num_threads = 4, p_chunk_size = 128)

# Filtra e Plota os gráficos.
for (nt in num_threads_configs) {
  for (cs in chunk_size_configs) {
    print(paste("Executing for: ", nt, ", ", cs))
    # myFilterAndPlot(nt, cs) 
    myFilterAndPlot(experiment_name, benchmark_name, machine_name, machine_arch[machine_name], p_num_threads = nt, p_chunk_size = cs)
  }  
}

# Experimento pilipili2.
benchmark_name <- paste0("gemm")
machine_name <- paste0("pilipili2")
csv_data_file = "gemm-pilipili2-omp-all-data-sizes-and-chunk-sizes-evaluation-11-08-2016-18-44-48-and-gemm-pilipili2-openmp-offloading-all-data-sizes-and-chunk-sizes-evaluation-17-11-2016-13-15-25-joined-processed-final.csv"
                 
# Carrega os dados.
myLoadAndPrepareData(experiment_name, benchmark_name, machine_name, csv_data_file)

# myFilterAndPlot(1, 32)
myFilterAndPlot(experiment_name, benchmark_name, machine_name, machine_arch[machine_name], p_num_threads = 22, p_chunk_size = 128)

# Filtra e Plota os gráficos.
for (nt in num_threads_configs) {
  for (cs in chunk_size_configs) {
    print(paste("Executing for: ", nt, ", ", cs))
    # myFilterAndPlot(nt, cs) 
    myFilterAndPlot(experiment_name, benchmark_name, machine_name, machine_arch[machine_name], p_num_threads = nt, p_chunk_size = cs)
  }  
}