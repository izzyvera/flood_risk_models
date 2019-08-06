#setwd("pathname")

library(ggplot2)
library(ggpmisc)
library(gridExtra)

s1 <- read.csv("05054000_new.csv")
s2 <- read.csv("05082500_new.csv")
s1$Date <- as.Date(s1$Date, "%m/%d/%y")
s2$Date <- as.Date(s2$Date, "%m/%d/%y")

df1 <- data.frame(Date = s1$Date, Gage_height = s1$Gage.height..Mean., Site = "05054000")
df2 <- data.frame(Date = s2$Date, Gage_height = s2$Gage.height..Mean., Site = "05082500")
df <- rbind(df1, df2)

ggplot(data = df, aes(x = Date, y = Gage_height, color = Site)) +
  geom_line(size = 1) +
  theme(legend.position = c(0.07, 0.9))

gs1 <- ggplot(data = df1, aes(x = Date, y = Gage_height)) +
  geom_line(size = 1, color = "#FC4E07") +
  geom_hline(yintercept = 18, color = "black", size = 1.2, linetype = "dashed") +
  geom_text(aes(x = s1$Date[1], y = 18.7, label = "Minor : 18", hjust = , size = 5)) +
  geom_hline(yintercept = 25, color = "black", size = 1.2, linetype = "dashed") +
  geom_text(aes(x = s1$Date[1], y = 25.7, label = "Moderate : 25", hjust = 0.38, size = 5)) +
  geom_hline(yintercept = 30, color = "black", size = 1.2, linetype = "dashed") +
  geom_text(aes(x = s1$Date[1], y = 30.7, label = "Major : 30", hjust = 0.53, size = 5)) +
  theme(legend.position = "none") +
  annotate("rect", xmin = as.Date("2009-01-01"), xmax = as.Date("2009-12-31"), ymin = 13, ymax = 42, alpha = .2)

gs2 <- ggplot(data = df2, aes(x = Date, y = Gage_height)) +
  geom_line(size = 1, color = "#00AFBB") +
  geom_hline(yintercept = 28, color = "black", size = 1.2, linetype = "dashed") +
  geom_text(aes(x = s2$Date[1], y = 28.7, label = "Minor : 28", hjust = 0.5, size = 5)) +
  geom_hline(yintercept = 40, color = "black", size = 1.2, linetype = "dashed") +
  geom_text(aes(x = s2$Date[1], y = 40.7, label = "Moderate : 40", hjust = 0.38, size = 5)) +
  geom_hline(yintercept = 46, color = "black", size = 1.2, linetype = "dashed") +
  geom_text(aes(x = s2$Date[1], y = 46.7, label = "Major : 46", hjust = 0.52, size = 5)) +
  theme(legend.position = "none") +
  annotate("rect", xmin = as.Date("2009-01-01"), xmax = as.Date("2009-12-31"), ymin = 15, ymax = 50, alpha = .2)

grid.arrange(gs1, gs2, nrow = 2)

# 09 - 10
# s1
df1 <- s1[(s1$Date >= "2009-01-01") & (s1$Date <= "2009-12-31"),]
df1 <- data.frame(df1$Date, scale(df1[c(3:ncol(df1))]))

s1.max.gage.date <- df1$df1.Date[which.max(df1$Gage.height..Mean.)]
s1.max.gage <- max(df1$Gage.height..Mean.)

s1.max.turbidity.date <- df1$df1.Date[which.max(df1[df1$df1.Date<="2009-04-01",]$Turbidity..Mean.)]
s1.max.turbidity <- max(df1[df1$df1.Date<="2009-04-01",]$Turbidity..Mean.)

s1.min.oxygen.date <- df1$df1.Date[which.min(df1[df1$df1.Date<="2009-04-01",]$Dissolved.oxygen..Mean.)]
s1.min.oxygen <- min(df1[df1$df1.Date<="2009-04-01",]$Dissolved.oxygen..Mean.)

s1.min.spec.date <- df1$df1.Date[which.min(df1[df1$df1.Date<="2009-04-01",]$Specific.conductance..Mean.)]
s1.min.spec <- min(df1[df1$df1.Date<="2009-04-01",]$Specific.conductance..Mean.)

s1.min.ph.date <- df1$df1.Date[which(df1[df1$df1.Date<="2009-04-01",]$Water.pH..Median. == min(df1[df1$df1.Date<="2009-04-01",]$Water.pH..Median.))[3]]
s1.min.ph <- min(df1[df1$df1.Date<="2009-04-01",]$Water.pH..Median.)


g1 <- ggplot(df1, aes(x = df1.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Turbidity..Mean., colour = "Turbidity"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#FC4E07", "black")) +
  geom_segment(aes(x = as.Date("2009-03-01"), y = 3.5, xend = as.Date("2009-03-24"), yend = 3.1), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-02-20"), y = 3.7, label = s1.max.turbidity.date)) +
  geom_segment(aes(x = as.Date("2009-04-20"), y = 4.5, xend = as.Date("2009-03-30"), yend = 3.9), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-05-08"), y = 4.6, label = s1.max.gage.date)) +
  xlab("Date") +
  ylab("Scaled value")

g2 <- ggplot(df1, aes(x = df1.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Dissolved.oxygen..Mean., colour = "Dissolved oxygen"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("black", "#FC4E07")) +
  geom_segment(aes(x = as.Date("2009-03-01"), y = -1.8, xend = as.Date("2009-03-24"), yend = -2.5), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-02-20"), y = -1.6, label = s1.min.oxygen.date)) +
  geom_segment(aes(x = as.Date("2009-04-20"), y = 4.5, xend = as.Date("2009-03-30"), yend = 3.9), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-05-08"), y = 4.6, label = s1.max.gage.date)) +
  xlab("Date") +
  ylab("Scaled value")

g3 <- ggplot(df1, aes(x = df1.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Water.temperature..Mean., colour = "Water temperature"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#FC4E07", "black")) +
  xlab("Date") +
  ylab("Scaled value")

g4 <- ggplot(df1, aes(x = df1.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Specific.conductance..Mean., colour = "Specific conductance"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#FC4E07", "black")) +
  geom_segment(aes(x = as.Date("2009-03-01"), y = -1.6, xend = as.Date("2009-03-24"), yend = -2.2), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-02-20"), y = -1.4, label = s1.min.oxygen.date)) +
  geom_segment(aes(x = as.Date("2009-04-20"), y = 4.5, xend = as.Date("2009-03-30"), yend = 3.9), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-05-08"), y = 4.6, label = s1.max.gage.date)) +
  xlab("Date") +
  ylab("Scaled value")

g5 <- ggplot(df1, aes(x = df1.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Water.pH..Median., colour = "Water pH"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#FC4E07", "black")) +
  geom_segment(aes(x = as.Date("2009-04-23"), y = -2.1, xend = as.Date("2009-03-28"), yend = -2.1), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-05-12"), y = -2.1, label = s1.min.ph.date)) +
  geom_segment(aes(x = as.Date("2009-04-20"), y = 4.5, xend = as.Date("2009-03-30"), yend = 3.9), size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = as.Date("2009-05-08"), y = 4.6, label = s1.max.gage.date)) +
  xlab("Date") +
  ylab("Scaled value")

grid.arrange(g1,g2,g4,g5,nrow = 4)

ggd1 <- ggplot(df1, aes(x = df1.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Discharge..Mean., colour = "Discharge"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("black", "#FC4E07")) +
  xlab("Date") +
  ylab("Scaled value")

# s2
df2 <- s2[(s2$Date >= "2009-01-01") & (s2$Date <= "2009-12-31"),]
df2 <- data.frame(df2$Date, scale(df2[c(3:ncol(df2))]))

ggd2 <- ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Discharge..Mean., colour = "Discharge"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("black", "#00AFBB")) +
  xlab("Date") +
  ylab("Scaled value")

grid.arrange(ggd1, ggd2, nrow = 2)

ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Turbidity..Mean., colour = "Turbidity"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#00AFBB", "black")) +
  xlab("Date") +
  ylab("Scaled value")

ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Dissolved.oxygen..Mean., colour = "Dissolved oxygen"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("black", "#00AFBB")) +
  xlab("Date") +
  ylab("Scaled value")

ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Water.temperature..Mean., colour = "Water temperature"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#00AFBB", "black")) +
  xlab("Date") +
  ylab("Scaled value")

ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Specific.conductance..Mean., colour = "Specific conductance"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#00AFBB", "black")) +
  xlab("Date") +
  ylab("Scaled value")

ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Water.pH..Median., colour = "Water pH"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#00AFBB", "black")) +
  xlab("Date") +
  ylab("Scaled value")

ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  geom_line(aes(y = Discharge..Mean., colour = "Discharge"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("black", "#00AFBB")) +
  xlab("Date") +
  ylab("Scaled value")

ggplot(df2, aes(x = df2.Date)) +
  geom_line(aes(y = Gage.height..Mean., colour = "Gage height"), size = 1) +
  #geom_line(aes(y = Temperature.Avg, colour = "Temperature"), size = 1) +
  #geom_line(aes(y = Humidity.Max, colour = "Humidity"), size = 1) +
  #geom_line(aes(y = Wind.Speed.Max, colour = "Wind speed"), size = 1) +
  #geom_line(aes(y = Pressure.Max, colour = "Pressure"), size = 1) +
  geom_line(aes(y = Precipitation.Avg, colour = "Precipitation"), size = 1) +
  theme(legend.position = c(0.07, 0.9)) +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = c("#00AFBB", "black")) +
  xlab("Date") +
  ylab("Scaled value")