# Load your libraries here
library(quantmod)
library(ggplot2)
# Get your data here
palt <- getSymbols("PALT",
                 src = "yahoo",
                 from = "2018-01-01",
                 to = "2021-01-01",
                 auto.assign = FALSE)
# Explore your data here
head(palt)
tail(palt)
summary(palt)
str(palt)
# Visualise your data here
scale_x_date(date_labels = "%b %y", date_breaks = "2 months")
ggtitle("Paltalk, Inc. prices series")
xlab("Date")
ylab("Price")
theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
ggplot(data = palt, aes(x = index(palt), y = palt[,6])) +
      geom_line(color = "darkblue") +
      ggtitle("Paltalk, Inc. prices series") +
      xlab("Date") +
      ylab("Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "2 months")
# Calculate the moving averages here
palt_ <- subset(palt, index(palt) >= "2018-01-01")

palt_ma10 <- rollmean(palt_[,6], 10, fill = list(NA, NULL, NA), align = "right")
palt_ma30 <- rollmean(palt_[,6], 30, fill = list(NA, NULL, NA), align = "right")

palt_$ma10 <- coredata(palt_ma10)
palt_$ma30 <- coredata(palt_ma30)
# Plot the calculated averages here
ggplot(data = palt_, aes(x = index(palt_))) +
  geom_line(aes(y = palt_[,6], color = "PALT")) +
  geom_line(aes(y = palt_$ma10, color = "MM10")) +
  geom_line(aes(y = palt_$ma30, color = "MM30")) +
  ggtitle("Paltalk, Inc. prices series") +
  xlab("Date") +
  ylab("Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
  scale_colour_manual("Series", values=c("PALT"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))
# Compute the returns here
palt_return <- diff(log(palt[,6]))
palt_return <- palt_return[-1,]
summary(palt_return)
# Visualise your computed returns here
ggplot(data = palt_return, aes(x = index(palt_return), y = palt_return)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("Paltalk, Inc. returns series") +
  xlab("Date") + ylab("Return") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months")