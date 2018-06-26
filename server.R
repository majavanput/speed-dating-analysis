library(shiny)

# Define server logic
shinyServer(function(input, output) {
	# Render Desirables by Race Plot when clicked on Race Button.
	observeEvent(input$raceButton, {
		output$mainPlot <- renderPlot ({
			visualizeRaceBarplot(input$categoryInput, output)
		})
	})
	
	# Render Desirables Average Plot when clicked on Average Button.
	observeEvent(input$averageButton, {
		output$mainPlot <- renderPlot ({
			visualizeAverageBarplot(input$categoryInput, output)
		})
	})

	# Render Desirables Percentage Plot when clicked on Proportion Button.
	observeEvent(input$percentageButton, {
		output$mainPlot <- renderPlot ({
			visualizePercentagePieChart(input$categoryInput, output)
		})
	})
})

# Import Speed Dating Data File.
data <- read.csv("data/Speed Dating Data.csv", header = TRUE)

# Male and Female Data Seperated.
dataFemale <- data[data$gender == 0,]
dataMale <- data[data$gender == 1,]
dataGenders <- list(dataMale, dataFemale)

# All Possibile Attributes. Not all categories have the same amount of attributes.
attributesLong <- c("attractive", "sincere", "intelligent", "fun", "ambitious", "shared interests")
attributesShort <- c("attractive", "sincere", "intelligent", "fun", "ambitious")
	
# All Possible Races.
races <- c("Black/African American", "European/Caucasian-American", "Latino/Hispanic American", 
		"Asian/Pacific Islander/Asian-American", "Native American", "Other")
	
categories <- c("What do you look for in the opposite sex?",
				"What do you think the opposite sex looks for in a date?",
				"How do you think you measure up?",
				"What do you think MOST of your fellow men/women look for in the opposite sex?",
				"How do you think others perceive you?")

source <- "Speed Dating Data Kroon"
				
# Get current attribute columns depending on chosen category.
getCurrentAttributeColumns <-
	function(category) {
		myAttr <- c()

		myAttr <- c(	
			paste("attr", sep = "", category, "_1"), 
			paste("sinc", sep = "", category, "_1"), 
			paste("intel", sep = "", category, "_1"), 
			paste("fun", sep = "", category, "_1"), 
			paste("amb", sep = "", category, "_1")	
		)
		
		# Some categories have more attributes.
		if (category != "3" && category != "5") {				
			temp <- paste("shar", sep = "", category, "_1")
			myAttr <- c(myAttr, temp)
		}

		return(myAttr)
	}

# Get a vector of attribute sums based on given data.
getVectorAttributeSum <-
	function(category, dataFilteredByRace) {
		myAttr <- c()

		myAttr <- c(	
			sum(dataFilteredByRace[, paste("attr", sep = "", category, "_1")], na.rm = TRUE),
			sum(dataFilteredByRace[, paste("sinc", sep = "", category, "_1")], na.rm = TRUE),
			sum(dataFilteredByRace[, paste("intel", sep = "", category, "_1")], na.rm = TRUE),
			sum(dataFilteredByRace[, paste("fun", sep = "", category, "_1")], na.rm = TRUE),
			sum(dataFilteredByRace[, paste("amb", sep = "", category, "_1")], na.rm = TRUE)
		)
			
		# Attributes that are used depending which category is selected.
		if (category != "3" && category != "5") {
			temp <- sum(dataFilteredByRace[, paste("shar", sep = "", category, "_1")], na.rm = TRUE)
			myAttr <- c(myAttr, temp)
		}
					
		return(myAttr)		
}

# The sum of all attributes together.
getAllAttributeSum <-
	function(category, dataFilteredByRace) {
		sum(getVectorAttributeSum(category, dataFilteredByRace))
	}

# The sum of a single attribute.
getSingleAttributeSum <-
	function(attribute, dataFilteredByRace) {
		sum(dataFilteredByRace[, attribute], na.rm = TRUE)
	}

# The proportion of points given to an attribute.
getProportionSingleAttribute <-
	function(attribute, dataFilteredByRace, sumAttributes) {
		(getSingleAttributeSum(attribute, dataFilteredByRace) / sumAttributes) * 100
	}

# Least desirables description.
leastDesirablesText <-
	function(output, text) {
		output$description <- renderUI ({
			HTML(
				paste("<small>", text, "</small>")
			)
		})
	}

# General least desirables description.
leastGeneral <-
	function(males, females, x) {
		text = "<b>Least Desirable Attributes:</b><br>"
	
		# Attributes that are used depending which category is selected.
		if (x != "3" && x != "5") {
			currentAttr = attributesLong
		}
		else {
			currentAttr = attributesShort
		}
		
		text =
			paste(
			text,
			"<b>Male : </b>",
			currentAttr[which(males == min(males))],
			"<br>",
			"<b>Female : </b>",
			currentAttr[which(females == min(females))]
			)
		text
	}
	
# Least desirables description by race.
leastByRace <-
	function(males, females, x) {
		text = "<b>Least Desirable Attributes:</b><br><br>
				<table border = '1'>
				<tr>
					<td></td>
					<td><b>Male</b></td>
					<td><b>Female</b></td>
				</tr>"
		
		# Attributes that are used depending which category is selected.
		if (x != "3" && x != "5") {
			currentAttr = attributesLong
		}
		else {
			currentAttr = attributesShort
		}
		
		for (index in 1:length(races))
		{
			# If no data available, go to next.
			if (sum(unlist(males[index])) == 0) {
				next
			}
		
			text = 
				paste(
					text,
					"<tr>
					<td><b>",
					races[index],
					"</b></td>
					<td>",
					currentAttr[which(unlist(males[index]) == min(unlist(males[index])))],
					"</td>
					<td>",
					currentAttr[which(unlist(females[index]) == min(unlist(females[index])))],
					"</tr>"
				)
		}
		
		text = paste(text, "</table>")
		text
	}
	
# Display source under plot.
displaySource <-
	function(output, text) {
		output$source <- renderUI ({
			HTML(
				paste("<h6>Source: ", text, "</h6>")
			)
		})
	}
	
# Display current category.
displayCategory <-
	function(output, text) {
		output$category <- renderUI ({
			HTML(
				paste("<h6>Category: ", text, "</h6>")
			)
		})
	}	

# Visualize the barplot with least desirables by race.
visualizeRaceBarplot <-
	function(x, output) {
		gendersList <- list()
		currentAttributes <- getCurrentAttributeColumns(x)
		i <- 1
		
		# Seperate attributes by gender and race.
		for (gender in dataGenders) {	
			currentGenderAttributes = list()
		
			for (r in 1:length(races)) {	
				currentRace = gender[gender$race == r,]
				currentGenderAttributes[[r]] <- getVectorAttributeSum(x, currentRace)
			}
			
			gendersList[[i]] <- currentGenderAttributes
			i <- i + 1
		}
		
		males <- gendersList[[1]]
		females <- gendersList[[2]]

		leastDesirablesText(output, leastByRace(males, females, x))
		
		# Compute percentages for each race of gender.
		malePlot <- matrix(
			c(
				(unlist(males[1]) / sum(sum(unlist(males[1])), na.rm = TRUE) * 100), 
				(unlist(males[2]) / sum(sum(unlist(males[2])), na.rm = TRUE) * 100), 
				(unlist(males[3]) / sum(sum(unlist(males[3])), na.rm = TRUE) * 100), 
				(unlist(males[4]) / sum(sum(unlist(males[4])), na.rm = TRUE) * 100), 
				(unlist(males[5]) / sum(sum(unlist(males[5])), na.rm = TRUE) * 100), 
				(unlist(males[6]) / sum(sum(unlist(males[6])), na.rm = TRUE) * 100)
			), 
			nrow = length(currentAttributes)
		)
		
		femalePlot <- matrix(
			c(
				(unlist(females[1]) / sum(sum(unlist(females[1])), na.rm = TRUE) * 100), 
				(unlist(females[2]) / sum(sum(unlist(females[2])), na.rm = TRUE) * 100), 
				(unlist(females[3]) / sum(sum(unlist(females[3])), na.rm = TRUE) * 100), 
				(unlist(females[4]) / sum(sum(unlist(females[4])), na.rm = TRUE) * 100), 
				(unlist(females[5]) / sum(sum(unlist(females[5])), na.rm = TRUE) * 100), 
				(unlist(females[6]) / sum(sum(unlist(females[6])), na.rm = TRUE) * 100)
			), 
			nrow = length(currentAttributes)
		)
		
		# Remove races for legend if all values in column are NaN.
		maleRaces <- races[(colSums(is.na(malePlot)) != nrow(malePlot)) != FALSE]
		femaleRaces <- races[(colSums(is.na(femalePlot)) != nrow(femalePlot)) != FALSE]
		
		# Remove columns for plot if all values in column are NaN.
		malePlot <- malePlot[, colSums(is.na(malePlot)) != nrow(malePlot)]
		femalePlot <- femalePlot[, colSums(is.na(femalePlot)) != nrow(femalePlot)]
		
		# Attributes that will appear in legend depending which category is selected.
		if (x != "3" && x != "5") {
			currentAttr <- attributesLong
		}
		else {
			currentAttr <- attributesShort
		}		
		
		par(xpd = TRUE)
		mar.default <- c(5, 4 , 6, 2) + 0.1
		par(mar = mar.default + c(2, 10, -4, 4))		
		par(mfrow = c(2, 1))

		barplot(
			malePlot, 
			main = "Male Proportions Desirables by Race", 
			names.arg = maleRaces, 
			las = 1, 
			horiz = TRUE, 
			cex.names = 0.9,
			xlab = "Percentage Attributes",
			col = terrain.colors(length(maleRaces) + 1)
		)
		
		barplot(
			femalePlot, 
			main = "Female Proportions Desirables by Race", 
			names.arg = femaleRaces, 
			las = 1, 
			horiz = TRUE, 
			cex.names = 0.9,
			xlab = "Percentage Attributes",
			col = terrain.colors(length(femaleRaces) + 1),
			legend = currentAttr, 
			args.legend = list(
			x = "bottom",
			cex = 0.8,
			horiz = TRUE,
			inset = c(0, -1.3))
		)
		
		displayCategory(output, categories[as.integer(x)])
		displaySource(output, source)
	}
	
# Visualize the pie chart with least desirables with percentages.
visualizePercentagePieChart <-
	function(x, output) {				
		gendersList <- list()
		currentAttributes <- getCurrentAttributeColumns(x)
		i <- 1
		
		for (gender in dataGenders) {					
			currentGenderAttributes = list()
			sumAttributes <- getAllAttributeSum(x, gender)
		
			for (j in 1:length(currentAttributes)) {				
				currentGenderAttributes[[j]] <- getProportionSingleAttribute(currentAttributes[j], gender, sumAttributes)
			}
			
			gendersList[[i]] <- currentGenderAttributes
			i = i + 1
		}
		
		attributesM <- unlist(gendersList[[1]], use.names = FALSE)
		attributesF <- unlist(gendersList[[2]], use.names = FALSE)		
		lblsM <- paste(round(attributesM, 2), "%", sep = "")
		lblsF <- paste(round(attributesF, 2), "%", sep = "")
		
		leastDesirablesText(output, leastGeneral(attributesM, attributesF, x))
		
		# Attributes that will appear depending which category is selected.
		if (x != "3" && x != "5") {
			lbls <- attributesLong
		}
		else {
			lbls <- attributesShort
		}
		
		par(xpd = TRUE)
		mar.default <- c(5, 2, 5, 0) + 0.1
		par(mar = mar.default + c(0, 7, -3, 0))		
		par(mfrow = c(1, 2))
		
		pie(attributesM, labels = lblsM, main="Proportion Given Points Desirables Male", col = rainbow(length(attributesM)))
		legend("left", lbls, fill = rainbow(length(attributesM)), inset = c(-0.2, 0), cex = 0.9)
		pie(attributesF, labels = lblsF, main="Proportion Given Points Desirables Female", col = rainbow(length(attributesF)))
		
		displayCategory(output, categories[as.integer(x)])
		displaySource(output, source)
	}

# Visualize the barplot with least desirables average scores.
visualizeAverageBarplot <-
	function(x, output) {
		gendersList <- list()
		currentAttributes <- getCurrentAttributeColumns(x)
		i <- 1
		
		for (gender in dataGenders)
		{					
			currentGenderAttributes = list()
		
			for (j in 1:length(currentAttributes))
			{
				currentMean <- mean(gender[, currentAttributes[j]], na.rm = TRUE)
				currentGenderAttributes[[j]] <- currentMean
			}
			
			gendersList[[i]] <- currentGenderAttributes
			i = i + 1
		}
		
		attributesM = unlist(gendersList[[1]], use.names = FALSE)
		attributesF = unlist(gendersList[[2]], use.names = FALSE)
		leastDesirablesText(output, leastGeneral(attributesM, attributesF, x))
		
		attributes = matrix(c(attributesM, attributesF), nrow = 2, byrow = TRUE)
		
		# Attributes that will appear depending which category is selected.
		if (x != "3" && x != "5") {
			colnames(attributes) <- attributesLong
		}
		else {
			colnames(attributes) <- attributesShort
		}
		
		rownames(attributes) = c("male", "female")
		
		mar.default <- c(5,4,4,2) + 0.1
		par(mar = mar.default + c(0, 4, 0, 12))
		
		barplot(
			attributes, 
			main = "Average Score Attribute by Gender",
			xlab = "Average Score",
			col = c("blue", "pink"), 
			legend = rownames(attributes), 
			args.legend = list(
				x = "topright",
				y = max(colSums(attributes)),
				inset = c(-0.20, 0)
			),
			las = 1,
			horiz = TRUE,
			beside = TRUE,
			cex.names = 0.9
		)
		
		displayCategory(output, categories[as.integer(x)])
		displaySource(output, source)
	}
