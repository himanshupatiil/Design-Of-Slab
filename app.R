#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(DescTools)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Design of Slab, C2-Group1"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("ly", "Longer Span (in m):", 0),
            numericInput("lx", "Shorter Span (in m):", 0),
            numericInput("support", "Size of Support(in m)", 0),
            selectInput("fy", "Grade of Steel", c("MS"="250", "Fe415"="415", "Fe500"="500")),
            numericInput("fck", "Grade of Concrete: M", 0),
            selectInput("dc", "Exposure Condition:", c("Mild Condition"="mild", "Moderate Condition"="moderate")),
            numericInput("dm", "Diameter of Main Steel(in mm):", 0),
            numericInput("dd", "Diameter of Distribution Steel(in mm):", 0),
            actionBttn("submit", "Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("type"),
           textOutput("Data1"),
           textOutput("Data2"),
           textOutput("Data3"),
           textOutput("Data4"),
           textOutput("Data5"),
           textOutput("Data6"),
           tableOutput("result"),
           tableOutput("check")
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    type = eventReactive(input$submit,{
        ratio = input$ly/input$lx
        if(ratio>=2){
            print("Type:One Way Slab")
        }else{
            print("Type: Two Way Slab")
        }

        
    })
    Data1=eventReactive(input$submit,{
        print(paste("Longer Span:", input$ly,"m",",",
                    "Shorter Span;", input$lx,"m"))
    })
    Data2=eventReactive(input$submit,{
        print(paste("Size of Support:", input$support,"m"))
    })
    Data3=eventReactive(input$submit,{
        if(input$fy=="250"){
            x=print("Mild Steel, Fy= 250mpa")
        }else if(input$fy=="415"){
            x=print("Fe415, fy=415 mpa")
        }else{
            x=print("fe500, fy=500mpa")
        }
        print(paste("Grade of Steel:", x, ",","Grade of Concrete, M", input$fck))
        
    })
    Data4=eventReactive(input$submit, {
        if(input$dc=="mild"){
            x=print("Mild Condition, Cover = 20mm")
        }else{
            x=print("Moderate Condition, cover=30mm")
        }
        print(paste("Exposure Condition", x))
    })
    Data5=eventReactive(input$submit, {
        print(paste("Diameter of main Steel:", input$dm,"mm,", "Diameter of Distribution steel:", input$dd,"mm"))
    })
    Data6=eventReactive(input$submit,{
        #determining pt assumed
        if(input$fy=="250"){
            pt = 0.7
        }else if(input$fy=="415"){
            pt=0.35
        }else{
            pt=0.275
        }
        #determining MF
        if(pt==0.7){
            MF = 1.7
        }else if(pt==0.35){
            MF = 1.2
        }else{
            MF=1.5
        }
        print(paste("Percentage of Steel(Assumed):", pt,"%",",","Modification Factor:", MF))
    })
    result=eventReactive(input$submit, {
        ratio=input$ly/input$lx
        if(ratio>=2){
            #determining cover
            if(input$dc=="mild"){
                dcover=20
            }else{
                dcover=30
            }
            #determining pt assumed
            if(input$fy=="250"){
                pt = 0.7
            }else if(input$fy=="415"){
                pt=0.35
            }else{
                pt=0.275
            }
            #determining MF
            if(pt==0.7){
                MF = 1.7
            }else if(pt==0.35){
                MF = 1.2
            }else{
                MF=1.5
            }
            #effective depth
            d=RoundTo((input$lx*1000)/(20*MF), 10)
            #overall depth
            D=RoundTo(d+dcover, 10)
            #determination of effective span
            le1 =(input$lx+input$support)
            le2= input$lx+(d/1000)
            if(le1>le2){
                le=le2
            }else{
                le=le1
            }
            #load calculation
            DL = 25*(D/1000)*1
            LL=3*1
            FF=1*1
            w=DL+FF+LL
            wd=w*1.5
            #Factored BM
            md=(wd*le^2)/8
            #required effective depth
            if(input$fy=="250"){
                dreq=(md*10^6/(0.149*input$fck*1000))^0.5
            }else if(input$fy=="415"){
                dreq=(md*10^6/(0.138*input$fck*1000))^0.5
            }else{
                dreq=(md*10^6/(0.133*input$fck*1000))^0.5
            }
            #area of main steel
            if(input$fy=="250"){
                ast = ((0.5*input$fck)/250)*(1-(1-((4.6*md*10^6)/(input$fck*1000*d^2)))^0.5)*1000*d
            }else if(input$fy=="415"){
                ast = ((0.5*input$fck)/415)*(1-(1-((4.6*md*10^6)/(input$fck*1000*d^2)))^0.5)*1000*d
            }else{
                ast = ((0.5*input$fck)/500)*(1-(1-((4.6*md*10^6)/(input$fck*1000*d^2)))^0.5)*1000*d
            }
            #spacing
            adm =(3.14/4)*input$dm^2
            s= RoundTo((adm/ast)*1000, 10)
            astp=(1000*adm)/s
            #check for deflection
            ptp = (astp/(1000*d))*100
            #distribution steel
            astd= (0.12/100)*1000*D
            ads=(3.14/4)*input$dd^2
            sd=RoundTo((ads/astd)*1000, 10)
            #rtab=matrix(c(d,dreq, D, le, wd,md,ast, s, astp, astd, sd ), byrow = T)
            #colnames(rtab)=c("Effective Depth(mm)", "Eff. Depth Required(mm)", "Overall Depth(mm)", "Eff. Span(m)",
                             #"Factored Load(KN/m", "Factored Bm(KNm)", "Area of Steel(mm2)", "Spacing(main steel)", "Area of Steel Provided(mm2)", "Area of Dist. Steel(mm2)", "Spacing(Dist. Steel,mm)")
            #resultt=as.data.frame(rtab)
            #print(resultt)
            df = data.frame(
                Particulars=c("Effective Depth(mm)", "Eff. Depth Required(mm)", "Overall Depth(mm)", "Eff. Span(m)",
                  "Factored Load(KN/m", "Factored Bm(KNm)", "Area of Steel(mm2)", "Spacing(main steel)", "Area of Steel Provided(mm2)","Percentage of Steel Provided(%)", "Area of Dist. Steel(mm2)", "Spacing(Dist. Steel,mm)"),
                Data=c(d,dreq, D, le, wd,md,ast, s, astp,ptp, astd, sd ),
                stringsAsFactors = F
            )
            print(df)
        }else{
            print("_______________________________Two Way Slab Design is Not Available_________________________________")
        }
    })
    check=eventReactive(input$submit, {
        ratio=input$ly/input$lx
        if(ratio>=2){
            #determining cover
            if(input$dc=="mild"){
                dcover=20
            }else{
                dcover=30
            }
            #determining pt assumed
            if(input$fy=="250"){
                pt = 0.7
            }else if(input$fy=="415"){
                pt=0.35
            }else{
                pt=0.275
            }
            #determining MF
            if(pt==0.7){
                MF = 1.7
            }else if(pt==0.35){
                MF = 1.2
            }else{
                MF=1.5
            }
            #effective depth
            d=RoundTo((input$lx*1000)/(20*MF), 10)
            #overall depth
            D=RoundTo(d+dcover, 10)
            #determination of effective span
            le1 =(input$lx+input$support)
            le2= input$lx+(d/1000)
            if(le1>le2){
                le=le2
            }else{
                le=le1
            }
            #load calculation
            DL = 25*(D/1000)*1
            LL=3*1
            FF=1*1
            w=DL+FF+LL
            wd=w*1.5
            #Factored BM
            md=(wd*le^2)/8
            #required effective depth
            if(input$fy=="250"){
                dreq=(md*10^6/(0.149*input$fck*1000))^0.5
            }else if(input$fy=="415"){
                dreq=(md*10^6/(0.138*input$fck*1000))^0.5
            }else{
                dreq=(md*10^6/(0.133*input$fck*1000))^0.5
            }
            #area of main steel
            if(input$fy=="250"){
                ast = ((0.5*input$fck)/250)*(1-(1-((4.6*md*10^6)/(input$fck*1000*d^2)))^0.5)*1000*d
            }else if(input$fy=="415"){
                ast = ((0.5*input$fck)/415)*(1-(1-((4.6*md*10^6)/(input$fck*1000*d^2)))^0.5)*1000*d
            }else{
                ast = ((0.5*input$fck)/500)*(1-(1-((4.6*md*10^6)/(input$fck*1000*d^2)))^0.5)*1000*d
            }
            #spacing
            adm =(3.14/4)*input$dm^2
            s= RoundTo((adm/ast)*1000, 10)
            astp=(1000*adm)/s
            #check for deflection
            ptp = (astp/(1000*d))*100
            #distribution steel
            astd= (0.12/100)*1000*D
            ads=(3.14/4)*input$dd^2
            sd=RoundTo((ads/astd)*1000, 10)
            #main spacing check
            if((3*d)>s && 300>s){
                smcheck=print(paste("Spacing Provided is less than", round(3*d),"mm", "or 300mm"))
            }else{
                smcheck=print(paste("Provide spacing (Whichever is less)", round(3*d),"mm", "or 300mm"))
            }
            #dist spacing check
            if((5*d)>sd && 500>sd){
                sdcheck=print(paste("Spacing Provided is less than", round(5*d),"mm", "or 500mm"))
            }else{
                sdcheck=print(paste("Provide spacing (Whichever is less)", round(5*d),"mm", "or 500mm"))
            }
            #deflection check
            if(ptp>pt&&dreq>d ){
                dcheck=print(paste("Not Satisfied: Please Revise the Design",",", round(ptp,2),">",round(pt,2),"and", round(dreq,2),">",d))
            }else{
                dcheck=print(paste("Satisfied",",","Steel Percentage" ,round(ptp,2),"<",round(pt,2),"and","Depth", round(dreq,2),"<",d))
            }
            vu=(wd*le)/2
            tv=(vu*10^3)/(1000*d)
            if(input$fck==15){
                tcmax=2.5
            }else if(input$fck==20){
                tcmax=2.8
            }else if(input$fck==25){
                tcmax=3.1
            }else if(input$fck==30){
                tcmax=3.5
            }else if(input$fck==35){
                tcmax=3.7
            }else if(input$fck>=40){
                tcmax=4
            }
            if(input$fck==20){
                tbd=1.2
            }else if(input$fck==25){
                tbd=1.4
            }else if(input$fck==30){
                tbd=1.5
            }else if(input$fck==35){
                tbd=1.7
            }else if(input$fck>=40){
                tbd=1.9
            }
            if(input$fy=="415"){
                ldreq=round((0.87*415*input$dm)/(4*1.6*tbd))
            }else{
                ldreq=round((0.87*500*input$dm)/(4*1.6*tbd))
                
            }
            ld=round((1.3*(md*10^6/2))/(vu*10^3))
            if(ld>ldreq){
                ldcheck=print(paste("Satisfied, Development Lenght required:", ldreq,"mm","<","Development Length:", ld,"mm"))
                
            }else{
                ldcheck=print(paste("Not Satisfied, Development Lenght required:", ldreq,"mm",">","Development Length:", ld,"mm"))
            }
            if(tv<(0.5*tcmax)){
                scheck=print("No Shear Reinforcement Required")
            }else{
                scheck=print("Shear Reinforcement Required")
            }
           
            df1 = data.frame(
                Checks=c("Spacing of Main Steel", "Spacing of Distribution Steel", "Check for Deflection", "Check for Shear", "Check for Development Length"),
                Data=c(smcheck, sdcheck, dcheck, scheck, ldcheck ),
                stringsAsFactors = F
            )
            print(df1)
        }else{
            print("_______________________________Two Way Slab Design is Not Available_________________________________")
        }
    })

    output$type <- renderText({
        type()
    })
    output$Data1 <- renderText({
        Data1()
    })
    output$Data2 <- renderText({
        Data2()
    })
    output$Data3 <- renderText({
        Data3()
    })
    output$Data4 <- renderText({
        Data4()
    })
    output$Data5 <- renderText({
        Data5()
    })
    output$Data6 <- renderText({
        Data6()
    })
    output$result <- renderTable({
        result()
    })
    output$check <- renderTable({
        check()
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
