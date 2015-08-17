library( ReporteRs )
library( ggplot2)

# get data
source("data_pre_processing.R")
data <- data_pre_processing()

#################### visualisation of data analysis ############################
# Creation of mydoc, a mydocx object
html = bsdoc( title = 'Data_visualisation' )
html = addTitle(html, value = "Report", 1, par.properties = parCenter())

# add a title to the section
html = addTitle( html, value = "Relations between variables:", 2 )

# add a tittle for the following matrix
html = addParagraph( html, value = "Correlation matrix", par.properties = parCenter() )

# corrrelation matrix
matrix.cor <- cor(data[,.(date = as.numeric(date), orders, drivers_available)])

# add a correlation matrix into html
html = addFlexTable( html, vanilla.table(matrix.cor, add.rownames = TRUE), par.properties = parCenter() )


# plot how orders evolute along time
p <- ggplot( data, aes(date, orders)) +
      geom_line(colour = "blue") +
      ggtitle("Orders evolution along time")

# add a plot into html
html = addPlot( html,
                function() print(p),
                width = 9, height = 7
                )

# plot how drivers_available evolute along time
p <- ggplot( data, aes(date, drivers_available)) +
  geom_line(colour = "blue") +
  ggtitle("Available drivers evolution along time")

# add a plot into html
html = addPlot( html,
                function() print(p),
                width = 9, height = 7
              )


# orders evolution along available drivers
avg.dri <- data[, .(qty = mean(orders)), by = .(drivers_available)]
avg.dri <- avg.dri[order(drivers_available, decreasing = FALSE )]

p <- ggplot(avg.dri, aes(drivers_available, qty)) +
  geom_point(colour = "blue") +
  ggtitle("Orders evolution along available drivers") +
  ylab("orders")

# add a plot into html
html = addPlot( html,
                function() print(p),
                width = 9, height = 9
)



########################### Tuning model ############################
# add a title to the section
html = addTitle( html, value = "Construction of prediction model:", 2 )

# predict with polynomial regression eand plot 
lr <- lm(orders ~ as.numeric(date) + I((as.numeric(date))^2) +
           I((as.numeric(date))^3) + drivers_available +
           I(drivers_available^2) + I(drivers_available^3) +
           I(drivers_available^4) + I(drivers_available^5),
         data)
# store primary predictions in data frame 
data$pred <- lr$fitted.values
# calculate mean cost
cost <- sqrt(mean((data$pred - data$orders)^2))

p <- ggplot(data, aes(x=date)) +
  geom_line(aes(y=orders, colour="Actual orders")) +
  geom_line(aes(y=pred, colour="Prediction")) +
  scale_colour_discrete("") +
  ggtitle("Actual orders VS Forecast orders") +
  ylab("orders")
  

# add a plot into html
html = addPlot( html,
                function() print(p),
                width = 9, height = 7
)

html = addParagraph(html, value = paste("Mean cost:", cost), par.properties = parCenter())

# add a paragraph
html = addParagraph( html, value = "According to the graph above, we can observe that our predictions fit roughly the actual orders.
                     But there are still some peaks of falls in red (actual orders). Due to this matter, we assume that these peaks
                     might be related to circumustances (the only varialble left). Therefore, a calculation of prediction errors is
                     done as follows: ",
                     par.properties = parCenter() )

# calculate average error by circumstance
err.circ <- data[, .(error_average = mean((orders - pred)/orders)), by=.(circumstance) ]

# add an error matrix into html
html = addFlexTable( html, vanilla.table(err.circ), par.properties = parCenter() )

# add a paragraph
html = addParagraph( html, value = "From this table, we find that the errors for dry and rainy circumstance are not huge but they are both
                                    negative. In comparison, the one of very_rainy weather is positive and much larger (6 times) than them
                                    and the error of strike circumstance is even larger (10 times) than them. Depending to this fact, 
                                    we assume that different circumstance should be processed differently.",
                     par.properties = parCenter() )

circs <- unique(data[, circumstance])

# add correlation matrix of each circumstance into report
for (i in 1:length(circs)) {
  circ <- circs[i]
  matrix.cor.tmp <- cor(data[circumstance==circ, .( date = as.numeric(date), orders, drivers_available)])
  # add an error matrix into html
  html = addParagraph( html, paste("Correlation matrix -", circ), par.properties = parCenter() )
  html = addFlexTable( html, vanilla.table(matrix.cor.tmp, add.rownames = TRUE), par.properties = parCenter() )
}


# initialise a cost matrix
matrix.cost <- matrix(nrow=1, ncol=4, dimnames = list(c("cost"), circs))
list.lr <- list()

# predict with polynomial linear regression and plot for each circumstance 
for (i in 1:length(circs)) {
  circ <- circs[i]
  lr <- lm(orders ~ as.numeric(date) + I((as.numeric(date))^2) +
             I((as.numeric(date))^3) + drivers_available +
             I(drivers_available^2) + I(drivers_available^3) +
             I(drivers_available^4) + I(drivers_available^5),
           data[circumstance==circ])
  list.lr[[circ]] <- lr
  # store primary predictions in data frame 
  data[circumstance==circ, pred1 := lr$fitted.values]
  # calculate mean cost
  cost <- sqrt(mean((data[circumstance==circ]$pred1 - data[circumstance==circ]$orders)^2))
  matrix.cost["cost", circ] <- cost
}

# calculate mean cost
cost <- sqrt(mean((data$pred1 - data$orders)^2))
# plot actual orders and predictions
p <- ggplot(data, aes(x=date)) +
  geom_line(aes(y=orders, colour="Actual orders")) +
  geom_line(aes(y=pred1, colour="Prediction")) +
  scale_colour_discrete("") +
  ggtitle("Actual orders VS Forecast orders") +
  ylab("orders")


# add a plot into html
html = addPlot( html,
                function() print(p),
                width = 9, height = 7
)
# plot errors of predictions
p <- ggplot( data, aes(date, (orders-pred1)^2)) +
  geom_line(colour = "red") +
  ylab("error") +
  ggtitle("Errors evolution along time")

# add a plot into html
html = addPlot( html,
                function() print(p),
                width = 9, height = 7
)

html = addParagraph(html, value = "Mean cost per circumnstance", par.properties = parCenter())
html = addFlexTable( html, vanilla.table(matrix.cost), par.properties = parCenter() )

html = addParagraph(html, value = paste("Mean cost:", cost), par.properties = parCenter())

html = addParagraph(html,
                    value = "With constructing different models for different circumnstances, the cost is decreased. This means the performance
                             gets better. Therefore, we are constructing one model per circumstance and each model is based on polynomial
                             regression with two varialbles (date & drivers_avaiable). This model can be used for forecasting the orders of
                             future dates.",
                    par.properties = parCenter())

# write the doc 
writeDoc( html, file = "examples/htmloutput/data_visualisation.html" )
