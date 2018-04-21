#Analysis of WW Rising Main Data
#APH 23/09/2017

library(ggplot2)
###set working directory
wd_local<-c('C:/Users/Alex/Documents/R/WWL/WW Rising Mains')
wd_output<-c('C:/Users/Alex/Documents/R/WWL/WW Rising Mains/Outputs2/')

setwd(wd_local)

dat <- read.csv("RisingMain_byPS_2.csv", na.strings = c("", "NA"))
dat <- dat[ , grepl("_flag|length|user_text_30|user_text_35", names(dat))]

column <- names(dat)
column <- column[-c(13,21)]
pumpstation <- unique(factor(dat$user_text_30))

for(colu in column){
  if(colu == "length" | colu == "user_text_30" | colu == "user_text_30_flag" | colu == "user_text_35_flag"| colu == "user_text_35")
  {
    next
  }
  else
  new_dat <- dat[0,]
  dat0 <- dat[,c(paste(colu), "length", "user_text_30")]
#  if(colu == "user_text_35")
#    {
#    ggplot(data = dat0, aes_string(x = "user_text_30", y = colu)) + coord_flip() +
#      labs(title = "Repairs Count", y = "# of Repairs", x = "Pumpstation") +
#      ggsave(filename = paste(wd_output, paste(colu, "Repairs", sep = "_"), sep = "/"), width = 5, height = 7, units = "in")
#    }
#  else
  for(station in pumpstation){
    dat1 <- subset(dat0, dat0$user_text_30 == station)
    if(sum(dat1$length) > 1000)
    {
      dat1$splitter = 1000
    }
    else if((sum(dat1$length) <= 1000) & (sum(dat1$length) > 250))
    {
      dat1$splitter = 500
    }
    else
    {
      dat1$splitter = 100
    }
    new_dat <- rbind(new_dat, dat1)  
  }
  dat1000 <- subset(new_dat, new_dat$splitter == 1000)
  dat500 <- subset(new_dat, new_dat$splitter == 500)
  dat100 <- subset(new_dat, new_dat$splitter == 100)
  
    ggplot(data = dat1000, aes_string(x = "user_text_30", y = "length", fill = colu)) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = c("ASB" = "green", "CONF" = "blue", "HARM" = "red", "IF" = "red", "INM" = "olivedrab", "JB" = "gray",
                                 "#D" = "gray40", rep("gray", 50)), na.value = "gray") +
    coord_flip() +
    labs(title = paste(colu, "by Length > 1000 m", sep = " "), y = "Length (m)", x = "Pumpstation") +
    ggsave(filename = paste(wd_output, paste(colu, "1000.pdf", sep = "_"), sep = "/"), width = 5, height = 7, units = "in")
  
    ggplot(data = dat500, aes_string(x = "user_text_30", y = "length", fill = colu)) + 
    geom_bar(stat = "identity") + 
    coord_flip() +
    scale_fill_manual(values = c("ASB" = "green", "CONF" = "blue", "HARM" = "red", "IF" = "red", "INM" = "olivedrab", "JB" = "gray", "#D" = "gray40",
                                 "#D" = "gray40", rep("gray", 50)), na.value = "gray") +
    labs(title = paste(colu, "by Length >250, <= 1000 m", sep = " "), y = "Length (m)", x = "Pumpstation") +
    ggsave(filename = paste(wd_output, paste(colu, "250-1000.pdf", sep = "_"), sep = "/"), width = 7, height = 7, units = "in")
  
    ggplot(data = dat100, aes_string(x = "user_text_30", y = "length", fill = colu)) + 
    geom_bar(stat = "identity") + 
    coord_flip() +
    scale_fill_manual(values = c("ASB" = "green", "CONF" = "blue", "HARM" = "red", "IF" = "red", "INM" = "olivedrab", "JB" = "gray", "#D" = "gray40",
                                 "#D" = "gray40", rep("gray", 50)), na.value = "gray") +
    labs(title = paste(colu, "by Length <= 250 m", sep = " "), y = "Length (m)", x = "Pumpstation") +
    ggsave(filename = paste(wd_output, paste(colu, "250.pdf", sep = "_"), sep = "/"), width = 5, height = 7, units = "in")
}


