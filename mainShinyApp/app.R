# Load required packages
require(shiny)
require(shinythemes)
require(tidyverse)
require(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Status summary of Celtic Sea stocks in 2020"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "group", label = h3("Select group to plot:"),
                        choices = list(Total = "Total", Benthic = "Benthic", Crustacean = "Crustacean",
                                       Demersal = "Demersal", Elasmobranch = "Elasmobranch", Pelagic = "Pelagic"),
                        selected = "Total"),
        width = 2),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "finalPlot", height = "800px", width = "1500px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Read CSV data
    allData <- read.csv(file = "Advice+Data_Analyst_Assignment-Data.csv")

    data_grouped <- allData %>%

        # Convert FisheriesGuild as a factor variable
        mutate(fisheries = factor(x = tolower(FisheriesGuild),
                                  levels = c("total", "benthic", "crustacean", "demersal", "elasmobranch", "pelagic"),
                                  labels = c("Total", "Benthic", "Crustacean", "Demersal", "Elasmobranch", "Pelagic")),
               frequency = value)


    # Create a reactive data for MSY info
    data_msy <- reactive({
        data_grouped %>%

            # Filter rows just for MSY info
            dplyr::filter(tolower(Variable) != "sbl" & tolower(lineDescription) == "msy") %>%

            # Create a new data frame converting categorical variables to factors
            transmute(fisheries = fisheries,
                      variable = factor(x = tolower(Variable),
                                        levels = c("fishingpressure", "stocksize"),
                                        labels = c("Fishing pressure", "Stock size")),
                      situation = factor(x = tolower(colour),
                                         levels = c("green", "red", "grey"),
                                         labels = c("F <= Fmsy | B >= Bmsy",
                                                    "F > Fmsy | B < Bmsy",
                                                    "Unknown Ref. Points")),
                      frequency = frequency) %>%

            # Group by fisheries and variable...
            group_by(fisheries, variable) %>%

            # ...for calculating relative values of frequency
            mutate(relative = round(frequency/sum(frequency)*100, 2))
    })

    # Create a reactive data for PA info
    data_pa <- reactive({
        data_grouped %>%

            # Filter rows just for PA info
            filter(tolower(Variable) != "sbl" & tolower(lineDescription) == "pa") %>%

            # Create a new data frame converting categorical variables to factors
            transmute(fisheries = fisheries,
                      variable = factor(x = tolower(Variable),
                                        levels = c("fishingpressure", "stocksize"),
                                        labels = c("Fishing pressure", "Stock size")),
                      situation = factor(x = tolower(colour),
                                         levels = c("green", "orange", "red", "grey"),
                                         labels = c("F < Fpa | B >= Bpa",
                                                    "Fpa < F < Flim | Bpa < B < Blim",
                                                    "F > Flim | B < Blim",
                                                    "Unknown Ref. Points")),
                      frequency = frequency) %>%

            # Group by fisheries and variable...
            group_by(fisheries, variable) %>%

            # ...for calculating relative values of frequency
            mutate(relative = round(frequency/sum(frequency)*100, 2))
    })

    # Create a reactive data for SBL info
    data_sbl <- reactive({
        data_grouped %>%

            # Filter rows just for SBL info
            filter(tolower(Variable) == "sbl") %>%

            # Create a new data frame converting categorical variables to factors
            transmute(fisheries = fisheries,
                      situation = factor(x = tolower(colour),
                                         levels = c("grey", "red", "green"),
                                         labels = c("Unknown Ref. Points",
                                                    "F > Fpa & SSB > Bpa",
                                                    "F <= Fmsy | SSB <= Bmsy")),
                      frequency = frequency) %>%

            # Group by fisheries and variable...
            group_by(fisheries) %>%

            # ...for calculating relative values of frequency
            mutate(relative = round(frequency/sum(frequency)*100, 2))
    })


    # Define limits of Y axis
    ylim <- c(0, 100)

    # Define interval of Y axis
    y_interval <- 20

    # Define the vector of breaks for Y axis
    yBreaks <- seq(from = ylim[1], to = ylim[2], by = y_interval)

    # Define general text size
    textSize <- 20

    # Start rendering of plot
    output$finalPlot <- renderPlot({

        # Define color for MSY plot
        color <- c("olivedrab3", "firebrick2", "gray60")

        plot_msy <- data_msy() %>%

            # Filter data depeding on selected group
            dplyr::filter(fisheries == as.character(input$group)) %>%

            # Start ggplot of bars
            ggplot(aes(x = variable, y = relative, fill = situation)) +

            # Define plot type (stacked bars)
            geom_col(position = "stack") +

            # Rotate plot (horizontal bars)
            coord_flip() +

            # Avoid to drop empty levels for situation variable
            scale_x_discrete(drop = FALSE) +

            # Define some aspects of legend of colors
            scale_fill_manual("legend", values = color, drop = FALSE,
                              guide = guide_legend(reverse = TRUE)) +

            # Define some aspects of Y axis
            scale_y_continuous(limits = ylim + c(0, 0.1),
                               breaks = yBreaks, labels = paste0(yBreaks, "%"),
                               expand = c(0, 0), position = "right") +

            # Define general aspects for plot
            theme(legend.position = "top", legend.title = element_blank(),
                  axis.title = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA),
                  text = element_text(size = textSize),
                  plot.margin = unit(x = c(0.5, 0.75, 0.5, 0), units = "cm"))


        # Define color for PA plot
        color <- c("olivedrab3", "orange", "firebrick2", "gray60")

        plot_pa <- data_pa() %>%

            # Filter data depeding on selected group
            dplyr::filter(fisheries == as.character(input$group)) %>%

            # Start ggplot of bars
            ggplot(aes(x = variable, y = relative, fill = situation)) +

            # Define plot type (stacked bars)
            geom_col(position = "stack") +

            # Rotate plot (horizontal bars)
            coord_flip() +

            # Avoid to drop empty levels for situation variable
            scale_x_discrete(drop = FALSE) +

            # Define some aspects of legend of colors
            scale_fill_manual("legend", values = color, drop = FALSE,
                              guide = guide_legend(reverse = TRUE)) +

            # Define some aspects of Y axis
            scale_y_continuous(limits = ylim + c(0, 0.1),
                               breaks = yBreaks, labels = paste0(yBreaks, "%"),
                               expand = c(0, 0)) +

            # Define general aspects for plot
            theme(legend.position = "bottom", legend.title = element_blank(),
                  axis.title = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA),
                  text = element_text(size = textSize), legend.text = element_text(size = 12),
                  plot.margin = unit(x = c(0.5, 0.75, 0.5, 0), units = "cm"))



        # Define color for SBL plot
        color <- c("gray60", "firebrick2", "olivedrab3")

        plot_sbl <- data_sbl() %>%

            # Filter data depeding on selected group
            dplyr::filter(fisheries == as.character(input$group)) %>%

            # Start ggplot of bars
            ggplot(aes(x = situation, y = relative, fill = situation)) +

            # Define plot type (dodge bars)
            geom_col(position = position_dodge(preserve = "single")) +

            # Define some aspects of Y axis
            scale_y_continuous(limits = ylim, expand = c(0, 0), position = "right") +

            # Avoid to drop empty levels for situation variable
            scale_x_discrete(drop = FALSE) +

            # Set Y axis title
            ylab("Frequency (%)") +

            # Define some aspects of legend of colors
            scale_fill_manual("legend", values = color) +

            # Define general aspects for plot
            theme(legend.position = "none", legend.title = element_blank(),
                  axis.title.x = element_blank(),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill = NA),
                  text = element_text(size = textSize))

        # Set plot arrange
        grid.arrange(arrangeGrob(plot_msy, plot_pa, nrow = 2), plot_sbl, nrow = 1)
    }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
