library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(shinyjs)

z <- function(egfr, pulm, age, cardiac, hepatic){
    x <- 0
    if(egfr=="> 90"){
    } else if(egfr=="60-89.9"){x <- x+2
    } else if(egfr=="< 60"){x <- x+3}
    if(pulm=="None"){
    } else if(pulm=="Mild/moderate"){x <- x+1
    } else if(pulm=="Severe"){x <- x+2}
    if(age==T){x <- x+1}
    if(cardiac==T){x <- x+2}
    if(hepatic==T){x <- x+2}
    q <- x
    if(x > 4){q <-4}
    q <- (q/4)*100
    return(q)
}

z1 <- function(q){
    q2 <- q/100*4
    if(q2 < 4){q <- paste("SCI Score:", q2)
    } else if(q2 >= 4){q <- paste("SCI Score: >= 4")}
    return(q)
}


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin="red",
    # Application title
    dashboardHeader(title="SCI Calculator"),
    dashboardSidebar(disable=T),
    # Sidebar with a slider input for number of bins 
    dashboardBody(
        tags$head(includeHTML("gt.html")),
            
            
           box(background="black", solidheader=F, 
               title="Select comorbidities:",width=12,
            awesomeRadio(
                inputId = "egfr",
                label = "eGFR", 
                choices = c("> 90", "60-89.9", "< 60"),
                selected = "> 90",
                inline = TRUE, 
                checkbox = TRUE
            ),
            awesomeRadio(
                inputId = "pulm",
                label = "Pulmonary disease", 
                choices = c("None", "Mild/moderate", "Severe"),
                selected = "None",
                inline = TRUE, 
                checkbox = TRUE
            ),
            
            
            prettySwitch(
                inputId = "age",
                label = "Age > 60 years", 
                status = "success",
                fill = TRUE
            ),
            prettySwitch(
                inputId = "cardiac",
                label = "Cardiac disease", 
                status = "success",
                fill = TRUE
            ),
            prettySwitch(
                inputId = "hepatic",
                label = "Moderate/severe hepatic disease", 
                status = "success",
                fill = TRUE
            ),
           ),
           box(title=textOutput("text"), background="black",width=12,
                progressBar(id = "pb3", value = 0, striped = TRUE)),
        fluidRow(box(div(img(src = "scale-01.jpg", width="100%")), width=12)),
        fluidRow(box(h3(tags$b("Comorbidity definitions:")), h4(tags$b("Renal dysfunction"), "- defined according to the estimated glomerular filtration rate (eGFR) mL/min/1.73 m2, using the", tags$a(href="https://www.kidney.org/content/ckd-epi-creatinine-equation-2009", "Chronic Kidney Disease Epidemiology Collaboration"), "formula for estimating creatinine clearance. An eGFR ≥ 90 was considered normal; 60-89.9 mildly decreased; < 60 moderately to severely decreased.", h4(tags$b("Pulmonary disease"), "- moderate defined as a DLCo or FEV1 66-80% or dyspnea on slight activity; severe defined as a DLCo or FEV1 < 66% or dyspnea at rest/oxygen dependent.", h4(tags$b("Cardiac disease:"), "- defined as any of the following: coronary artery disease, congestive heart failure, history of myocardial infarction, left ventricular ejection fraction ≤ 50%, atrial fibrillation or flutter, sick sinus syndrome, ventricular arrhythmias, or any valvular disease except mitral valve prolapse."), h4(tags$b("Modreate/severe hepatic disease"), "- serum bilirubin > 1.5 times upper limit of normal (ULN); ALT or AST > 2.5 times ULN, or chronic hepatitis."))), width=12), width=12),
        fluidRow(box(h4(tags$a(href="https://bit.ly/3n8PDuj", "Shouval, Fein, et al. The Simplified Comorbidity Index (SCI) - a new tool for prediction of non-relapse mortality in allogeneic HCT. Blood Advances, 2021.")), width=12), width=12),

        ))
    


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    showModal(modalDialog(title="Simplified Comporbidity Index", size="l",
                          HTML(paste("<h3 style='text-align:center'>Shouval, Fein, et al., 'The Simplified Comorbidity Index: a new tool for prediction of nonrelapse mortality in all-HCT', Blood Advances, 2021",
                                     "<i>This information is intended for healthcare professionals and should not substitute for clinical judgement.</i>",
                                     "<h4 style='text-align: center'>These results are based on the derivation set from the above-described manuscript, including 573 adult (≥ 18 years) patients who underwent CD34-selected allo-HCT between 2008 and 2018 at Memorial Sloan Kettering Cancer Center. Please see manuscript for additional inclusion details.",
                                     sep="<br><br>"
                          ))))
    x <- 0
    output$text <- renderText({z1(x)})
    observeEvent(input$egfr, {
        x <- z(input$egfr, input$pulm, input$age, input$cardiac, input$hepatic)
        status <- c(NULL,"success", "info","warning", "danger")[(x/100)*4]
        updateProgressBar(session = session, id = "pb3",  status=status, value = x)
        output$text <- renderText({z1(x)})
    })
    observeEvent(input$pulm, {
        x <- z(input$egfr, input$pulm, input$age, input$cardiac, input$hepatic)
        status <- c(NULL,"success", "info","warning", "danger")[(x/100)*4]
        updateProgressBar(session = session, id = "pb3", value = x,  status=status)
        output$text <- renderText({z1(x)})
    })
    observeEvent(input$age, {
        x <- z(input$egfr, input$pulm, input$age, input$cardiac, input$hepatic)
        status <- c(NULL,"success", "info","warning", "danger")[(x/100)*4]
        updateProgressBar(session = session, id = "pb3", value = x, status=status)
        output$text <- renderText({z1(x)})
    })
    observeEvent(input$cardiac, {
        x <- z(input$egfr, input$pulm, input$age, input$cardiac, input$hepatic)
        status <- c(NULL,"success", "info","warning", "danger")[(x/100)*4]
        updateProgressBar(session = session, id = "pb3", value = x, status=status)
        output$text <- renderText({z1(x)})
    })
    observeEvent(input$hepatic, {
        x <- z(input$egfr, input$pulm, input$age, input$cardiac, input$hepatic)
        status <- c(NULL,"success", "info","warning", "danger")[(x/100)*4]
        updateProgressBar(session = session, id = "pb3", value = x, status=status)
        output$text <- renderText({z1(x)})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
