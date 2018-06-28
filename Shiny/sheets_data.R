gs_auth
gs_ls() # not neccessary
gs_mock_runway_title <- gs_title("Mock Runway Table")
gs_MRT <- gs_read(gs_mock_runway_title, ws=1)


operators_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Operators))
RRR_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$RRRs))
sentries_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Sentries))
strategists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Strategists))
specialists_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$Specialists))
Agent_Labor_cost_numeric = as.numeric(gsub("[\\$,]", "", gs_MRT$`Agent Labor Costs`))

df=data.frame(Month = gs_MRT[,1], 
              Operator_Labor_cost = operators_numeric[1:nrow(gs_MRT)],
              #RRR_Labor_Cost = RRR_numeric[1:nrow(gs_MRT)],
              Sentry_Labor_Cost = sentries_numeric[1:nrow(gs_MRT)])#,
#Strategists_Labor_Cost = strategists_numeric[1:nrow(gs_MRT)],
#Specialists_Labor_Cost = specialists_numeric[1:nrow(gs_MRT)],
#Agent_Labor_Cost = Agent_Labor_cost_numeric[1:nrow(gs_MRT)])


tabPanel("Client Growth",
         fluidPage(
           fluidRow(
             column(6, htmlOutput("num_client")), 
             column(6, htmlOutput("rev_client"))),
           fluidRow( htmlOutput("client")))),



