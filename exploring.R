# what do we think about including the 
#  plant_data <- plant_data[plant_data$season == "winter", ]
# line in summarize_plant_data (arg being "season")?
# 
#  abundance -> p/a (including plant species names, etc)
# lines in summarize_plant_data (option for output being "presence")?
#   same for rodents tbh


# i need to align season with month?

library(portalr)
download_observations()

# functions

#convert abundance to presence
abp <- function(x){
  x[x>0] <- 1
  x
}
#coeff of var
cv <- function(x, digits = 1){
  x <- na.omit(x)
  out <- sd(x) / mean(x)
  round(out, digits)
}
#operating under assumption of plant name codes being two sets of 4 lowercase
# letters with a space in the middle (replacing . in cases of data.frame use)
a_plant_spp <- function(x){
  x <- gsub("\\.", " ", x)
  grepl("[a-z]{4} [a-z]{4}", x)
}


# winter annuals
wa_quadrat <- summarize_plant_data(level = "quadrat",
                                     type = "winter-annual", 
                                     correct_sp = TRUE,
                                     unknowns = FALSE, shape = "crosstab", 
                                     output = "abundance")
wa_quadrat <- wa_quadrat[wa_quadrat$season == "winter", ]
spp_col <- a_plant_spp(colnames(wa_quadrat))
wa_quadrat[ , spp_col] <- apply(wa_quadrat[ , spp_col], 2, abp)
wa_quadrat$nspp <- apply(wa_quadrat[ , spp_col], 1, sum)

wa_plot <- summarize_plant_data(level = "plot",
                                  type = "winter-annual", correct_sp = TRUE,
                                  unknowns = FALSE, shape = "crosstab", 
                                  output = "abundance")
wa_plot <- wa_plot[wa_plot$season == "winter", ]
spp_col <- a_plant_spp(colnames(wa_plot))
wa_plot[ , spp_col] <- apply(wa_plot[ , spp_col], 2, abp)
wa_plot$nspp <- apply(wa_plot[ , spp_col], 1, sum)


# summer annuals
sa_quadrat <- summarize_plant_data(level = "quadrat",
                                     type = "summer-annual", 
                                     correct_sp = TRUE,
                                     unknowns = FALSE, shape = "crosstab", 
                                     output = "abundance")
sa_quadrat <- sa_quadrat[sa_quadrat$season == "summer", ]
spp_col <- a_plant_spp(colnames(sa_quadrat))
sa_quadrat[ , spp_col] <- apply(sa_quadrat[ , spp_col], 2, abp)
sa_quadrat$nspp <- apply(sa_quadrat[ , spp_col], 1, sum)

sa_plot <- summarize_plant_data(level = "plot",
                                  type = "summer-annual", correct_sp = TRUE,
                                  unknowns = FALSE, shape = "crosstab", 
                                  output = "abundance")
sa_plot <- sa_plot[sa_plot$season == "summer", ]
spp_col <- a_plant_spp(colnames(sa_plot))
sa_plot[ , spp_col] <- apply(sa_plot[ , spp_col], 2, abp)
sa_plot$nspp <- apply(sa_plot[ , spp_col], 1, sum)

#perennials
p_quadrat <- summarize_plant_data(level = "quadrat",
                                     type = "perennial", 
                                     correct_sp = TRUE,
                                     unknowns = FALSE, shape = "crosstab", 
                                     output = "abundance")
spp_col <- a_plant_spp(colnames(p_quadrat))
p_quadrat[ , spp_col] <- apply(p_quadrat[ , spp_col], 2, abp)
p_quadrat$nspp <- apply(p_quadrat[ , spp_col], 1, sum)

p_plot <- summarize_plant_data(level = "plot",
                                  type = "perennial", correct_sp = TRUE,
                                  unknowns = FALSE, shape = "crosstab", 
                                  output = "abundance")
spp_col <- a_plant_spp(colnames(p_plot))
p_plot[ , spp_col] <- apply(p_plot[ , spp_col], 2, abp)
p_plot$nspp <- apply(p_plot[ , spp_col], 1, sum)


# plot treatment assignments

plot_treatments <- read.csv("plot_treatments.csv")


# plotting paired comparisons for winter and summer annuals

# control_removal vs removal_removal

par(mfrow = c(2,1), mar = c(2, 3, 1, 1), bty = "L")

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "control_removal"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.7, 0.7))
  }
  if(plot_treatments$treatment[i] == "removal_removal"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.3, 0.7))
  }
}
text(rep(1980,2), c(23, 25), c("control_removal", "removal_removal"),
     col = rgb(0, c(0.7, 0.3), 0.7), adj = 0, cex = 0.8)

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "control_removal"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.7, 0))
  }
  if(plot_treatments$treatment[i] == "removal_removal"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.3, 0))
  }
}
text(rep(1980,2), c(23, 25), c("control_removal", "removal_removal"),
     col = rgb(0.7, c(0.7, 0.3), 0), adj = 0, cex = 0.8)



# control_krat vs krat_krat

par(mfrow = c(2,1), mar = c(2, 3, 1, 1), bty = "L")

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "control_krat"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.7, 0.7))
  }
  if(plot_treatments$treatment[i] == "krat_krat"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.3, 0.7))
  }
}
text(rep(1980,2), c(23, 25), c("control_krat", "krat_krat"),
     col = rgb(0, c(0.7, 0.3), 0.7), adj = 0, cex = 0.8)

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "control_krat"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.7, 0))
  }
  if(plot_treatments$treatment[i] == "krat_krat"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.3, 0))
  }
}
text(rep(1980,2), c(23, 25), c("control_krat", "krat_krat"),
     col = rgb(0.7, c(0.7, 0.3), 0), adj = 0, cex = 0.8)


# krat_control vs control_control

par(mfrow = c(2,1), mar = c(2, 3, 1, 1), bty = "L")

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "krat_control"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.7, 0.7))
  }
  if(plot_treatments$treatment[i] == "control_control"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.3, 0.7))
  }
}
text(rep(1980,2), c(23, 25), c("krat_control", "control_control"),
     col = rgb(0, c(0.7, 0.3), 0.7), adj = 0, cex = 0.8)

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "krat_control"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.7, 0))
  }
  if(plot_treatments$treatment[i] == "control_control"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.3, 0))
  }
}
text(rep(1980,2), c(23, 25), c("krat_control", "control_control"),
     col = rgb(0.7, c(0.7, 0.3), 0), adj = 0, cex = 0.8)


# removal_control vs control_control

par(mfrow = c(2,1), mar = c(2, 3, 1, 1), bty = "L")

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "removal_control"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.7, 0.7))
  }
  if(plot_treatments$treatment[i] == "control_control"){
    plot_i <- na.omit(wa_plot[wa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0, 0.3, 0.7))
  }
}
text(rep(1980,2), c(23, 25), c("removal_control", "control_control"),
     col = rgb(0, c(0.7, 0.3), 0.7), adj = 0, cex = 0.8)

plot(1,1, ylim = c(0,28), xlim=c(1980,2020), type="n", las = 1)
abline(v = 2015, lwd = 2, lty = 2, col = rgb(0.5, 0.5, 0.5))
for(i in 1:24){
  if(plot_treatments$treatment[i] == "removal_control"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.7, 0))
  }
  if(plot_treatments$treatment[i] == "control_control"){
    plot_i <- na.omit(sa_plot[sa_plot$plot == i, ])
    year <- plot_i$year
    nspp <- plot_i$nspp
    points(year, nspp, type = "l", lwd = 2, col = rgb(0.7, 0.3, 0))
  }
}
text(rep(1980,2), c(23, 25), c("removal_control", "control_control"),
     col = rgb(0.7, c(0.7, 0.3), 0), adj = 0, cex = 0.8)

