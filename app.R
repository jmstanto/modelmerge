#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rpart)

mergedModelPerf <- NULL
dataSet <- data.frame(state.x77, check.names = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Merging Predictive Models (U.S. States Version)"),

    # Sidebar with a slider input for number of models to run 
    sidebarLayout(
        sidebarPanel(
            selectInput("outcomeVar",
                        "Outcome Variable Name:",
#                        colnames(mtcars)[c(-2,-6)],
                        colnames(dataSet),
                        selected="Life.Exp"),
            
            sliderInput("sampSize",
                        "Sample Size:",
                        min = 16,
                        max = 128,
                        value = nrow(dataSet)),
            
            sliderInput("numModels",
                        "Number of models to merge:",
                        min = 2,
                        max = 50,
                        value = 2),
            
            helpText("This application merges predictive models by averaging the predictions from two or more training samples.", 
                     "Choose an outcome variable to predict from the list above.", 
                     "Then choose how many models to merge.", 
                     "In the graph at right, observe the predictions of each merged model on the Y-axis", 
                     "against the original criterion variable on the X-axis (represented by grey dots).",
                     "Red circles show the predictions of the merged model. Model performance is reported",
                     "under the graph. Try to create a combination of sample size and the number of",
                     "merged models that outperforms the baseline. Please remember to complete the brief survey. Thank you!")
            
        ),

        # Show a plot of the various models
        mainPanel(
           plotOutput("predPlot"),
           textOutput("modelPerf"),
           tags$iframe(src="https://syracuseuniversity.qualtrics.com/jfe/form/SV_a5Zd4FmIFBaVNzv", height="450px", width="450px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$predPlot <- renderPlot({
        
        # Set up for multiple runs of rpart
        numRuns <- 3
        modelList <- vector("list", numRuns)
        predictMatrix <- NULL
        
        # set.seed(1) # For repeatable results, use this instead
        set.seed(proc.time()[3]) # Sets a random seed based on how long the app has been running
        
#        predFormula <- as.formula(paste(input$outcomeVar," ~ cyl + wt"))
        predFormula <- as.formula(paste(input$outcomeVar," ~ ."))
        
        for (i in 1:input$numModels) {
            # Draw a sample from the data set
            tempSamp <- dataSet[sample(1:nrow(dataSet), size=input$sampSize, replace=TRUE), ]
            # Generate the rpart model
            modelOut <- rpart(predFormula, data=tempSamp, method="anova", model=F)
            # Save the results of the model
            modelList[[i]] <- modelOut
            # Use the model to predict
            tempPred <- predict(modelOut, dataSet)
            # Save the predictions
            predictMatrix <- cbind(predictMatrix, tempPred)
        }
        
        # Set up the plot with points from the first model
        plot(dataSet[[input$outcomeVar]], predictMatrix[,1], col="grey", xlab = "Criterion", ylab="Model")
        # Plot the remaining models
        for (i in 2:input$numModels) {
            points(dataSet[[input$outcomeVar]], predictMatrix[,i], col="grey")
        }
        # Finally, plot the average predictions in red
        points(dataSet[[input$outcomeVar]], rowMeans(predictMatrix), col="red", cex=2)
        
        mergedModelPerf <<- cor(dataSet[[input$outcomeVar]], rowMeans(predictMatrix))^2
        
    })
    
    output$modelPerf <- renderText({ 
        predFormula <- as.formula(paste(input$outcomeVar," ~ ."))
        temp1 <- round(summary(lm(predFormula, data=dataSet))$adj.r.squared, 3)
        temp2 <- round(mergedModelPerf, 3)
        temp3 <- input$numModels
        temp4 <- input$sampSize
        paste("R-Squared for ",temp3," models with sample size n=",temp4,". Baseline: ", temp1, ", Merged Models: ", temp2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
