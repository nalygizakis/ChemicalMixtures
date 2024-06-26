rm(list=ls())
library("shiny")
library("shinythemes")
library("readr")
library("readxl")
library("tidyverse")
library("navdata")
library("networkD3")
library("scales")
library("shinyWidgets")
load("data.RData")
Matrices_LIFE_APEX$LA<-unlist(lapply(strsplit(Matrices_LIFE_APEX$Sample,"_"),function(x){ x[1]}))

ui <- fluidPage(

  singleton(tags$head(

    
    HTML(
    '
    <title>LIFE APEX Project</title>
    
    <!-- Web Fonts -->
    <link rel="stylesheet" type="text/css" href="assets/css/font.css">
    
    <!-- CSS Global Compulsory -->
    <link rel="stylesheet" href="assets/plugins/bootstrap/css/bootstrap.min.css">
    <link rel="stylesheet" href="assets/css/style.css">
    
    <!-- CSS Header and Footer -->
    <link rel="stylesheet" href="assets/css/headers/header-default.css">
    <link rel="stylesheet" href="assets/css/footers/footer-v1.css">
    
    <!-- CSS Implementing Plugins -->
    <link rel="stylesheet" href="assets/plugins/animate.css">
    <link rel="stylesheet" href="assets/plugins/line-icons/line-icons.css">
    <link rel="stylesheet" href="assets/plugins/font-awesome/css/font-awesome.min.css">
    <link rel="stylesheet" href="assets/plugins/sky-forms-pro/skyforms/css/sky-forms.css">
    <link rel="stylesheet" href="assets/plugins/sky-forms-pro/skyforms/custom/custom-sky-forms.css">
    <!--[if lt IE 9]><link rel="stylesheet" href="assets/plugins/sky-forms-pro/skyforms/css/sky-forms-ie8.css"><![endif]-->
    
    <!-- CSS Theme -->
    <link rel="stylesheet" href="assets/css/theme-colors/default.css">
    <link rel="stylesheet" href="assets/css/theme-colors/blue.css" id="style_color">
    
    <!-- CSS DataTables -->
    <link rel="stylesheet" href="assets/css/jquery.dataTables.css">
    <link rel="stylesheet" href="assets/css/buttons.dataTables.min.css">
    
    <!-- CSS Customization -->
    <link rel="stylesheet" href="assets/css/custom.css">
    
    <div class="wrapper">
    <!--=== Header ===-->
    <div class="header">
    <div class="container">
    
    <!-- Logo -->
    <a class="logo">
    <img src="assets/img/logo-life-apex-eu1.png" alt="Logo" style="height: 60px">
    </a>

    <h3><center></center></h3>
    <h4><center>Interactive co-occurrence of contaminants of emerging concern (CECs)</h2></center>
    <h4><center><strong>LIFE APEX</strong></h4></center> 

    
    <!-- <h5><center>Chemical screening - 2,316 target subst2ances</h3></center>
    <!-- End Logo 410 suspects emma+1154 suspects nikiforos -->
    <!-- Topbar -->
    
    </div><!--/end container-->
    </div><!--/navbar-collapse-->
    </div>
    <!--=== End Header ===-->
    <!--=== Breadcrumbs ===-->
    <!--<div class="breadcrumbs">
    <div class="container">
    <h1 class="pull-left">Digital Sample Freezing Platform</h1>
    </div>
    </div>-->
    <!--=== End Breadcrumbs ===-->
    
    '
  ))),
  
  theme = shinytheme("simplex"),
  # HTML("<h3><center>Effluent wastewater from WWTPs in Germany</center></h3>"),
  # p("This application uses the data.frame 'eco2mix', included in the 'leaflet.minicharts' packages.",
  #   "It contains the monthly electric production of french regions from 2013 to 2017."),
 
  chooseSliderSkin("Modern"),
  
  sidebarLayout(
    sidebarPanel(style = "background: #F8F8F8",
      sliderInput(inputId = "percentage", label="Frequency of appearance range", min=0.1, max=100, value=c(0,10)),

      actionButton("toggle", "Change selection choice", width='100%'),
      
      conditionalPanel(condition = "input.toggle % 2 == 1", # & input.toggle2 % 2 == 1
                       
      selectInput("specific_samples","Specific sample selection",choices=Matrices_LIFE_APEX$Sample, multiple = TRUE,  selectize=FALSE, size=20),
      ),
      

      
      conditionalPanel(condition = "input.toggle % 2 == 0", # & input.toggle2 % 2 == 1
                       
      fluidRow(
        column(6,
               
               tags$h3("Top predators"),
               checkboxInput("Buzzard", label=HTML('<img src="LIFE_APEX_ICONS/buzzard.png" alt="Buzzard" width="96" height="47">'),value = TRUE),
               checkboxInput("Otter", label=HTML('<img src="LIFE_APEX_ICONS/otter.png" alt="Otter" width="50" height="50">'),value = TRUE),
               checkboxInput("Harbour Seal", label=HTML('<img src="LIFE_APEX_ICONS/harbour seal.png" alt="Harbour Seal" width="65" height="40">'),value = TRUE),
               checkboxInput("Harbour Porpoise", label=HTML('<img src="LIFE_APEX_ICONS/harbour porpoise2.png" alt="Harbour Porpoise" width="73" height="30">'),value = TRUE),
               checkboxInput("Eurasian Lynx", label=HTML('<img src="LIFE_APEX_ICONS/eurasian_lynx.png" alt="Herring" width="50" height="50">'),value = TRUE),
               checkboxInput("Herring Gull Egg", label=HTML('<img src="LIFE_APEX_ICONS/HerringGullEgg.png" alt="Herring" width="40" height="40">'),value = TRUE)
        ),
        
        column(6,
               
               tags$h3("Prey"),
               checkboxInput("Bream", label=HTML('<img src="LIFE_APEX_ICONS/bream.png" alt="Bream" width="57" height="30">'),value = TRUE),
               checkboxInput("Roach", label=HTML('<img src="LIFE_APEX_ICONS/roach.png" alt="Roach" width="57" height="30">'),value = TRUE),
               checkboxInput("Eelpout", label=HTML('<img src="LIFE_APEX_ICONS/eelpout.png" alt="Eelpout" width="57" height="30">'),value = TRUE),
               checkboxInput("Herring", label=HTML('<img src="LIFE_APEX_ICONS/Herring.png" alt="Herring" width="65" height="22">'),value = TRUE)
        )),
      
      
      fluidRow(
        column(4,
               tags$h3("Muscle"),
               checkboxInput("Muscle", label=HTML('<img src="LIFE_APEX_ICONS/muscle.png" alt="Muscle" width="80" height="39">'),value = TRUE)
        ),
        
        column(4,
               tags$h3("Liver"),
               checkboxInput("Liver", label=HTML('<img src="LIFE_APEX_ICONS/liver.png" alt="Liver" width="81" height="41">'),value = TRUE)
        ),
        
        column(4,
               tags$h3("Egg"),
               checkboxInput("Egg", label=HTML('<img src="LIFE_APEX_ICONS/Egg.png" alt="Liver" width="31" height="44">'),value = TRUE)
        )),
      ),
      HTML("<center>"),
        actionButton(inputId = "submit", label="Submit", class = "btn-success"),
      HTML("<center/>"),
        
      width = 3),
      
      
      
      
      
       
    
    
    mainPanel(
      forceNetworkOutput(outputId = "net",width = "100%", height="95vh"),
      downloadButton("download","Download")
    )),
    
  HTML('			<li>	</ul>
       </div><!--/end container-->
       </div>
       </div><!--/navbar-collapse-->'),
  HTML('	<!--=== Footer Version 1 ===-->
       <div class="footer-v1">
       <div class="copyright">
       <div class="container">
       <div class="row">
       <div class="col-md-6">
       <p>2019-',format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y"), ' &copy; All Rights Reserved.</p>
       </div>
       </div>
       </div>
       </div><!--/copyright-->
       </div>
       <!--=== End Footer Version 1 ===-->
       <!-- JS Global Compulsory -->
       <!--<script src="assets/plugins/jquery/jquery.min.js"></script>-->
       <script src="assets/plugins/jquery/jquery-migrate.min.js"></script>
       <script src="assets/plugins/bootstrap/js/bootstrap.min.js"></script>
       <!-- JS Implementing Plugins -->
       <script src="assets/plugins/back-to-top.js"></script>
       <script src="assets/plugins/smoothScroll.js"></script>
       <script src="assets/plugins/sky-forms-pro/skyforms/js/jquery.maskedinput.min.js"></script>
       <script src="assets/plugins/sky-forms-pro/skyforms/js/jquery-ui.min.js"></script>
       <script src="assets/plugins/sky-forms-pro/skyforms/js/jquery.validate.min.js"></script>
       <!-- JS DataTables -->
       <script src="assets/js/jquery.dataTables.min.js"></script>
       <!--<script src="assets/js/dataTables.fixedHeader.min.js"></script>-->
       <!--<script src="assets/js/dataTables.responsive.min.js"></script>-->
       <!--<script src="assets/js/dataTables.bootstrap.min.js"></script>-->
       
       <!-- JS Customization -->
       <script src="assets/js/jquery.blockUI.js"></script>
       <script src="assets/js/jquery.redirect.js"></script>
       <script src="assets/js/custom.js"></script>
       <!-- JS Page Level -->
       <script src="assets/js/app.js"></script>
       <script src="assets/js/plugins/style-switcher.js"></script>
       <script src="assets/js/plugins/masking.js"></script>
       <script src="assets/js/plugins/datepicker.js"></script>
       <script src="assets/js/plugins/validation.js"></script>
       <script type="text/javascript">
       jQuery(document).ready(function() {
       App.init();
       Masking.initMasking();
       Datepicker.initDatepicker();
       Validation.initValidation();
       StyleSwitcher.initStyleSwitcher();
       });
       </script>
       <!--[if lt IE 9]>
       <script src="assets/plugins/respond.js"></script>
       <script src="assets/plugins/html5shiv.js"></script>
       <script src="assets/plugins/placeholder-IE-fixes.js"></script>
       <script src="assets/plugins/sky-forms-pro/skyforms/js/sky-forms-ie8.js"></script>
       <![endif]-->
       <!--[if lt IE 10]>
       <script src="assets/plugins/sky-forms-pro/skyforms/js/jquery.placeholder.min.js"></script>
       <![endif]-->
       </body>
       </html>
       ')
  )
# server function
server<-function(input, output, session) {

  
observeEvent(input$submit,{
  #Selection of columns-----
  if(input$toggle %% 2==0){
    print("Here")
    selection<-rep(FALSE,times=10)
    names(selection)<-c("Buzzard","Otter","Harbour Seal","Harbour Porpoise","Bream","Roach","Eelpout","Herring","Herring Gull Egg","Eurasian Lynx")
    
    
    if(input$Buzzard) selection[names(selection)=="Buzzard"]<-TRUE
    else selection[names(selection)=="Buzzard"]<-FALSE
    
    if(input$Otter) selection[names(selection)=="Otter"]<-TRUE
    else selection[names(selection)=="Otter"]<-FALSE
    
    if(input$"Harbour Seal") selection[names(selection)=="Harbour Seal"]<-TRUE
    else selection[names(selection)=="Harbour Seal"]<-FALSE
    
    if(input$"Harbour Porpoise") selection[names(selection)=="Harbour Porpoise"]<-TRUE
    else selection[names(selection)=="Harbour Porpoise"]<-FALSE
    
    if(input$"Bream") selection[names(selection)=="Bream"]<-TRUE
    else selection[names(selection)=="Bream"]<-FALSE
    
    
    if(input$"Roach") selection[names(selection)=="Roach"]<-TRUE
    else selection[names(selection)=="Roach"]<-FALSE
    
    if(input$"Eelpout") selection[names(selection)=="Eelpout"]<-TRUE
    else selection[names(selection)=="Eelpout"]<-FALSE
    
    
    if(input$"Herring") selection[names(selection)=="Herring"]<-TRUE
    else selection[names(selection)=="Herring"]<-FALSE
    
    
    if(input$"Herring Gull Egg") selection[names(selection)=="Herring Gull Egg"]<-TRUE
    else selection[names(selection)=="Herring Gull Egg"]<-FALSE
    
    if(input$"Eurasian Lynx") selection[names(selection)=="Eurasian Lynx"]<-TRUE
    else selection[names(selection)=="Eurasian Lynx"]<-FALSE
    
    
    selection2<-rep(FALSE,times=3)
    names(selection2)<-c("Muscle","Liver","Egg")
    
    if(input$"Muscle") selection2[names(selection2)=="Muscle"]<-TRUE
    else selection2[names(selection2)=="Muscle"]<-FALSE
    
    if(input$"Liver") selection2[names(selection2)=="Liver"]<-TRUE
    else selection2[names(selection2)=="Liver"]<-FALSE
    
    if(input$"Egg") selection2[names(selection2)=="Egg"]<-TRUE
    else selection2[names(selection2)=="Egg"]<-FALSE
    
    #print(selection2[selection2])
    assign("selection",selection,.GlobalEnv)
    assign("selection2",selection2,.GlobalEnv)
    
    final_selection<-Matrices_LIFE_APEX$Sample[Matrices_LIFE_APEX$Matrix1 %in% names(selection)[selection] &
                                                 Matrices_LIFE_APEX$Matrix2 %in% names(selection2)[selection2]]
    final_selection<-  unlist(lapply(strsplit(final_selection,"_"),function(x){ x[1]}))
  } else {
    final_selection<-  unlist(lapply(strsplit(input$specific_samples,"_"),function(x){ x[1]}))
  }
  assign("final_selection",final_selection,.GlobalEnv)
  output_reduced<-output_dataset[,final_selection]
  
  index<-1
  i<-1
  j<-11
  resultGr1<-list()

  row_selector<-apply(output_reduced,1,function(y){ sum(!is.na(y))})/ncol(output_reduced)*100 >=input$percentage[1] & 
                apply(output_reduced,1,function(y){ sum(!is.na(y))})/ncol(output_reduced)*100 <=input$percentage[2]
  
  assign("row_selector",row_selector,.GlobalEnv)

  
  output_reduced<-output_reduced[row_selector,]

  for(i in 1:nrow(output_reduced)){
    for(j in 1:ncol(output_reduced)){
      if(!is.na(output_reduced[i,j])){
        resultGr1[[index]]<-data.frame(from=row.names(output_reduced)[i],to=names(output_reduced)[j],sig=output_reduced[i,j])
        index<-index+1
      }
    }
  }
  resultGr2<-do.call(rbind.data.frame,resultGr1)
  head(resultGr2)
  resultGr2$to<-as.character(resultGr2$to)
  resultGr2$from<-as.character(resultGr2$from)
  assign("resultGr2_csv",resultGr2,.GlobalEnv)
  require(igraph)
  
  # set seed for graph plot
  set.seed(1)
  
  # Create the graph object as undirected graph
  library("igraph")
  graphNetwork <- graph.data.frame(resultGr2, directed = F)
  
  
  
  names(resultGr2)<-c("to","from","sig")
  
  
  nodes <- data.frame(
    "id"=seq(from=1, to=length(levels(as.factor(c(resultGr2$from,resultGr2$to)))), by=1),
    "label"=levels(as.factor(c(resultGr2$from,resultGr2$to)))
  )
  
  
  i<-1
  assign("nodes",nodes,.GlobalEnv)
  assign("resultGr2",resultGr2,.GlobalEnv)
  
  for(i in 1:nrow(resultGr2)){
    if(length(nodes$id[which(resultGr2$from[i]==nodes$label)])==1) resultGr2$from[i]<-nodes$id[which(resultGr2$from[i]==nodes$label)]
    if(length(nodes$id[which(resultGr2$to[i]==nodes$label)])==1) resultGr2$to[i]<-nodes$id[which(resultGr2$to[i]==nodes$label)]
    #print(i)
  }
  char_to<-as.character(resultGr2$to)
  char_from<-as.character(resultGr2$from)
  
  resultGr2$to<-as.numeric(resultGr2$to)
  resultGr2$from<-as.numeric(resultGr2$from)
  edges<-resultGr2
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  edges$sig<-rescale(edges$sig, to=c(1,50))
  
  
  nodes_d3 <- mutate(nodes, id = id - 1)
  edges_d3 <- mutate(edges, from = from - 1, to = to - 1)
  
  nodes_d3$sizenode<-0.2
  nodes_d3$sizenode[which(grepl(x=nodes_d3$label,pattern="LIFE"))]<-100
  
  nodes_d3$matrixtype<-"Compound"
  
  i<-1
  for(i in 1:nrow(nodes_d3)){
    tell_me_Which<-which(nodes_d3$label[i]==lapply(strsplit(Matrices_LIFE_APEX$Sample,"_"),function(x){ x[1]}))
    if(length(tell_me_Which)>0){
      nodes_d3$matrixtype[i]<-Matrices_LIFE_APEX$Matrix1[tell_me_Which]
      #print(i)
    }
  }
  
  assign("edges_d3",edges_d3,.GlobalEnv)
  assign("nodes_d3",nodes_d3,.GlobalEnv)


  #nodes_d3$label<-levels(as.factor(char_to))
  save.image("test.RData")

  output$net <- renderForceNetwork(
    forceNetwork(
      Links = edges_d3, Nodes = nodes_d3,  
      Source = "from", Target = "to",      # so the network is directed.
      NodeID = "label", Group = "matrixtype", Value = "sig", 
      opacity = 1, fontSize = 16, zoom = TRUE,
      Nodesize = "sizenode", legend = TRUE,
      radiusCalculation = "Math.sqrt(d.nodesize)",
    )
    )
  #linkColour = ValjeanCols #einai oi syndeseis
  t<-2
  loop_times<-levels(as.factor(resultGr2_csv$from))
  for_download<-list()
  for(t in 1:length(loop_times)){
    tmp<-resultGr2_csv[resultGr2_csv$from==loop_times[t],]
    
    
    #####New block of code to find pairs
    pairs_apex_prey<-Matrices_LIFE_APEX[which(Matrices_LIFE_APEX$LA %in% tmp$to),]
    p<-1
    pairs2<-list()
    pairs3<-list()
    for(p in 1:nrow(pairs_apex_prey)){
      pairs_apex_prey$Matrix2[p]
      j<-1
      for(j in 1:length(pairs_apex_prey$Matrix2)){
        if(pairs_apex_prey$Matrix2[p]!=pairs_apex_prey$Matrix2[j]){
          one_element<-pairs_apex_prey$LA[p]
          second_element<-pairs_apex_prey$LA[j]
          
          if(as.numeric(gsub(x=one_element, pattern="LIFE APEX ",replacement=""))<as.numeric(gsub(x=second_element, pattern="LIFE APEX ",replacement=""))){
            pairs2[[j]]<-paste0(one_element,"-",second_element)
          } else {
            pairs2[[j]]<-paste0(second_element,"-",one_element)
          }
        }
      }
      pairs3[[p]]<-pairs2
    }
    pairs4<-unique(unlist(pairs3))
    
    tmp$pairs[1]<-paste0(pairs4,collapse = ",")
    ####
    
    
    tmp$to[1]<-paste0(tmp$to,collapse=",")
    tmp$sig[1]<-paste0(tmp$sig,collapse=",")
    tmp<-tmp[1,]
    for_download[[t]]<-tmp
  }
  for_download2<-do.call(rbind.data.frame,for_download)
  
  for_download3<-for_download2[order(unlist(lapply(strsplit(as.character(for_download2$to),","),length)),decreasing = TRUE),]
  names(for_download3)<-c("Compound","Sample","Concentration ng/g wet weight","Pairs Predator-Prey")
  
  for_download3$`Pairs Predator-Prey`<-gsub(x=for_download3$`Pairs Predator-Prey`,pattern="LIFE APEX ",replacement="LA")
  for_download3$Sample<-gsub(x=for_download3$Sample,pattern="LIFE APEX ",replacement="LA")
  
  for_download3$"Frequency of appearance in selected subset"<-round(unlist(lapply(strsplit(for_download3$Sample,","),length))/length(final_selection)*100,2)
  
  
  for_download3$"Number of Predator-Prey pairs"<-unlist(lapply(strsplit(for_download3$"Pairs Predator-Prey",","),length))
  
  
  #####Calculation of media concentration of predator and prey - Under construction
  t<-1
  for(t in 1:nrow(for_download3)){
    
    unlist(strsplit(for_download3$Sample[t],","))
    unlist(strsplit(for_download3$`Concentration ng/g wet weight`[t],","))
  }
  for_download3$`Concentration ng/g wet weight`
  
  Matrices_LIFE_APEX$Matrix2[which(Matrices_LIFE_APEX$LA %in% for_download3$Sample)]
  #####
    
  
  output$download <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(for_download3, con, row.names = FALSE)
    }
  )
  
})
  
}
shinyApp(ui = ui, server = server)
