# Install necessary packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("diffr")) install.packages("diffr")

# Load necessary libraries
library(shiny)
library(diffr)

# Function to process the file

process_file <- function(file_path) {
    # Read the file
    lines <- readLines(file_path)

    # Process each line
    processed_lines <- sapply(lines, function(line) {
        # Split the line into words
        words <- strsplit(trimws(line), " ")[[1]]

        # If the first word is a number, remove it
        if (length(words) > 0 && grepl("^[0-9]+$", words[1])) {
            words <- words[-1]
        }

        # Combine the words back into a line
        paste(words, collapse = " ")
    })


    # Combine all lines into a single string
    text <- paste(processed_lines, collapse = "\n")

    # Find the position of "E x e c u t i o n"
    pos <- regexpr("E x e c u t i o n", text)

    # If "E x e c u t i o n" is found, remove everything after it
    if (pos != -1) {
        text <- substr(text, 1, pos + attr(pos, "match.length") - 1)
    }

    # Create a temporary file
    temp_file <- tempfile()

    # Write the processed text to the temporary file
    writeLines(text, temp_file)

    # Return the path of the temporary file
    return(temp_file)
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Simple diffr App"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose first text file:"),
      fileInput("file2", "Choose second text file:"),
      actionButton("diff", "Show Difference")
    ),

    mainPanel(
      htmlOutput("diffOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
    observeEvent(input$diff, {
        # Get the texts
        text1 <- input$file1$datapath
        text2 <- input$file2$datapath

        text1 <- process_file(text1)
        text2 <- process_file(text2)

        # Calculate the diff  
        diff_result <- diffr(text1, text2, contextSize = 0, wordWrap = FALSE, width = 2000)

        # Render the diff in HTML format
        output$diffOutput <- renderUI({
            diff_result
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)