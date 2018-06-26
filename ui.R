library(shiny)

# Define User Interface for application
shinyUI(fluidPage(

	# Application title
	titlePanel("Speed Dating - Male and Female Desirables"),
	h5("Question: What are the least desirable attributes in a male partner? Does this differ for a female partner?"),
	h6("Maja van Put 0843807"),
	
	# Side Bar Layout
	sidebarLayout(	
		sidebarPanel(
			selectInput(
				"categoryInput", 
				label = h4("Select Category"), 
				choices = list(
					"What do you look for in the opposite sex?" = 1,
					"What do you think MOST of your fellow men/women look for in the opposite sex?" = 4,
					"What do you think the opposite sex looks for in a date?" = 2,
					"How do you think you measure up?" = 3, 
					"How do you think others perceive you?" = 5),
				selected = 1),
			br(),
			br(),
			actionButton("raceButton", "Compute by Race"),
			br(),
			br(),
			actionButton("averageButton", "Compute Average Score Desirables"),
			br(),
			br(),
			actionButton("percentageButton", "Compute Percentage Desirables"),
			br(),
			br(),
			br(),
			br(),
			htmlOutput("description"),
			br()
		),
		
		mainPanel(
			htmlOutput("category"),
			plotOutput(outputId = "mainPlot"),
			htmlOutput("source")
		)
	)
))
