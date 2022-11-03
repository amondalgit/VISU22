library(ggplot2)
library(gridExtra)
library(reshape2)
library(ggpubr)
theme_set(theme_bw())
###################################################
theme_set(theme_bw(20))

# creating empty dataframe
cumulative_df = data.frame(means = numeric(0), 
                samp_size = character(0), lambda = character(0))

# data generating function
problem_1 = function(lambda, sample_size, sim_size){
  samplings_mean_vec = replicate(sim_size,mean(rpois(sample_size,lambda)))
  samplings_mean_df = data.frame(samplings_mean_vec, 
        rep(paste("Sample size :", sample_size,sep = " "),sim_size), 
        rep(paste("Lambda :",lambda,sep = " "),sim_size))
  return(samplings_mean_df)
}

lambdas = c(0.7, 1.7)  # two different lambda values

test_cases = c(010,030,100,300) # different samples sizes

simulation_size = 5000 # no of smaplings for each lambda and each sample size

# populating dataframe with data
for (lambda in lambdas){
  for (test in test_cases){
    cumulative_df = rbind(cumulative_df, problem_1(lambda,test,simulation_size))
  }
}
colnames(cumulative_df) = c('means', 'samp_size', 'lambda')

# Histograms
ggplot(cumulative_df)+aes(x=means)+
  geom_histogram(color="white", fill="black",bins=22)+
  geom_vline(xintercept = 0.7, linetype="dashed", color="orange",size=1)+
  geom_vline(xintercept = 1.7, linetype="dashed", color="blue",size=1)+
  facet_grid(lambda ~ samp_size, scales = "free")+
  theme(axis.line = element_line(colour = "darkblue", size = 1, 
                                 linetype = "solid"), 
        axis.title.x = element_text(face="bold",size=15), 
        axis.title.y = element_text(face="bold",size=15))+
  geom_text(aes(0.6,2000,label = 0.7, hjust=-1))+
  geom_text(aes(1.6,2000,label = 1.7, hjust=-1))+
  labs(title = "Histograms corresponding to all test-cases",x = "Sample Means")

head(cumulative_df[cumulative_df$lambda=='Lambda : 0.7',])

# Q-Q Plots
for (lambda in lambdas){
  df = cumulative_df[cumulative_df$lambda==paste('Lambda :',lambda,sep = ' '),]
  
  print(ggplot(data=df, aes(sample = means))+
        geom_qq()+
        geom_qq_line(color = "blue")+
        facet_wrap(vars(samp_size), scales = "free_y",nrow = 1)+
        theme(axis.line = element_line(colour = "darkblue", size = 1, 
                      linetype = "solid"), 
              axis.title.x = element_text(face="bold",size=15), 
              axis.title.y = element_text(face="bold",size=15))+
        labs(title = 
            paste('Q-Q Plots corresponding to Lambda =',lambda,sep = ' '),
            x = "Theoretical Quantiles", 
            y = "Sample Quantiles"))
}

############################################

problem_2 = function(){
  jj_earnings_df = data.frame(time = time(JohnsonJohnson), 
                              earnings = JohnsonJohnson)
  
  line = ggplot(data = jj_earnings_df, aes(x = time, y = earnings)) + 
    geom_line(color = "#00AFBB", size = 1)+
    geom_smooth()+
    theme(axis.line = element_line(colour = "black", size = 1, 
                                   linetype = "solid"), 
          axis.title.x = element_text(face="bold",size=12), 
          axis.title.y = element_text(face="bold",size=12))+
    labs(title = "Time Series plot",
         subtitle = "Quarterly earnings from 1960 - 1980",
         x = "Time", 
         y = "Earnings($)")
  
  log = ggplot(data = jj_earnings_df, aes(x = time, y = earnings)) + 
    geom_line(color = "red", size = 1) + scale_y_continuous(trans = "log")+
    geom_smooth(method = "lm")+
    theme(axis.line = element_line(colour = "black", size = 1, 
                                   linetype = "solid"), 
          axis.title.x = element_text(face="bold",size=12), 
          axis.title.y = element_text(face="bold",size=12))+
    labs(title = "Log-scaled Time Series plot",
         subtitle = "Quarterly earnings from 1960 - 1980",
         x = "Time", 
         y = "log(Earnings)")
  
  figure = ggarrange(line, log, ncol = 2)
  return(figure)
}
print(problem_2())

############################################

problem_3 = function(){
  chickwts_df = data.frame(chickwts)
  
  #
  print(ggplot(chickwts_df, aes(x = feed, y = weight, fill=feed))+
          aes(color = feed)+
          geom_boxplot(alpha=0.3)+
          theme(axis.line = element_line(colour = "black", size = 1, 
                                         linetype = "solid"), 
                axis.title.x = element_text(face="bold",size=15), 
                axis.title.y = element_text(face="bold",size=15),
                axis.text.x = element_text(face = "bold",size = 12),
                axis.text.y = element_text(face = "bold",size = 12)))
  
  # Probability densities
  print(ggplot(chickwts_df)+
          geom_density(aes(y=weight, fill="f8766d"))+
          facet_grid(~feed)+
          theme(axis.line = element_line(colour = "black", size = 1, 
                                         linetype = "solid"), 
                axis.title.x = element_text(face="bold",size=15), 
                axis.title.y = element_text(face="bold",size=15),
                axis.text.x = element_text(angle = -90,face = "bold",size = 10),
                axis.text.y = element_text(face = "bold",size = 10),
                legend.position="none")+
          labs(title = "Probability of weight of chicks depending on feed",
               x = "Probability Density", 
               y = "Weight(g)"))
}
problem_3()

############################################
time = data.frame(time = time(EuStockMarkets))
data = data.frame(EuStockMarkets)
n = nrow(data)

for(i in 1:4){
  temp_dr = c(0)
  temp_inv = c(1000)
  for(j in 2:n){
    k = log(data[j,i]/data[j-1,i])
    temp_dr = c(temp_dr,k)
    temp_inv = c(temp_inv,temp_inv[j-1]*exp(k))
  }

  if(i==1){
    dr_df = data.frame(IDX_dr = temp_dr)
    investment_df = data.frame(IDX_inv = temp_inv)
  }
  else{
    dr_df[paste(names(data)[i], 'dr', sep = '_')] = data.frame(temp_dr)
    investment_df[paste(names(data)[i], 'inv', sep = '_')] = data.frame(temp_inv)
  }
}

inv_melt=melt(cbind(time,investment_df),id=c('time'),value.name = "value")

pt_melt = melt(cbind(time, data), id = c("time"), value.name = "value")

dr_melt = melt(cbind(time, dr_df), id = c("time"), value.name = "value")

print(ggplot(pt_melt)+aes(x=time, y=value)+
  geom_line(aes(color=variable),size=1)+
  facet_grid(vars(variable))+
  labs(title = "Performance on closing value of various Indices(P_t)", 
       y = "Closing value")+
  theme(axis.line = element_line(colour = "black", size = 0.6, 
                                 linetype = "solid"), 
        axis.title.x = element_text(face="bold",size=15), 
        axis.title.y = element_text(face="bold",size=15),
        axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12),
        legend.position = "none"))



print(ggplot(dr_melt)+aes(x=time, y=value)+
  geom_line()+
  geom_point(alpha=0.3,aes(colour=value>0))+
  facet_grid(vars(variable))+
  labs(title = "Performance on daily return of various Indices(r_t)", 
       y = "log(ratio of consecutive day values)")+
  theme(axis.line = element_line(colour = "black", size = 0.6, 
                                 linetype = "solid"), 
        axis.title.x = element_text(face="bold",size=15), 
        axis.title.y = element_text(face="bold",size=15),
        axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12)))

print(ggplot(pt_melt)+aes(x=value)+
  geom_histogram(aes(color=variable),bins=50)+
  facet_grid(vars(variable))+
  labs(title = 
         "Deviation of closing value of various indices from its centrality", 
       y = "Count",x="Closing Value")+
  theme(axis.line = element_line(colour = "black", size = 0.6, 
                                 linetype = "solid"), 
        axis.title.x = element_text(face="bold",size=15), 
        axis.title.y = element_text(face="bold",size=15),
        axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12),
        legend.position = "none"))

print(ggplot(dr_melt)+aes(x=value)+
  geom_histogram(aes(color=variable),bins=40)+
  facet_grid(vars(variable))+
  geom_vline(xintercept = 0, linetype="dashed", color="orange",size=1)+
  labs(title = 
         "Count of daily return of various indices", 
       y = "Count",x="Daily return")+
  theme(axis.line = element_line(colour = "black", size = 0.6, 
                                 linetype = "solid"), 
        axis.title.x = element_text(face="bold",size=15), 
        axis.title.y = element_text(face="bold",size=15),
        axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12),
        legend.position = "none"))

print(ggplot(inv_melt, aes(x = time, y = value)) + 
  geom_line(aes(color = variable, linetype = "solid"),size=1)+
  labs(title = 
         "Performance comparision of various Indices b/w 1991-'99", 
       y = "Absolute Monetery value($)",x="Time")+
  theme(axis.line = element_line(colour = "black", size = 0.6, 
                                 linetype = "solid"), 
        axis.title.x = element_text(face="bold",size=13), 
        axis.title.y = element_text(face="bold",size=13),
        axis.text.x = element_text(face = "bold",size = 12),
        axis.text.y = element_text(face = "bold",size = 12)))