overviewUI <- function(id) {
  tagList(
    HTML(
      paste(
        h1("STAGEM",align="center",style='background-color:lightgrey'),  
        h1("Saptiotemporal Analysis of Gastech Employees’ Movement Data",align="center")
      )
    ),
    HTML('<center><img src = "vastchallenge.jpg" style="width: 50%; height: 50%"></center>'),
    p(""),
    HTML('<center><img src = "gastechlogo.png" style="width: 15%; height: 15%"></center>'),
    p("Many of the Abila, Kronos-based employees of GAStech have company cars which are approved for both personal and business use. 
      Those who do not have company cars have the ability to check out company trucks for business use, but these trucks cannot be used for personal business."),
    p(""),
    p("Employees with company cars are happy to have these vehicles, because the company cars are generally much higher quality than the cars they would be able to afford otherwise. 
      However, GAStech does not trust their employees. 
      Without the employees knowledge, GAStech has installed geospatial tracking software in the company vehicles. 
      The vehicles are tracked periodically as long as they are moving."),
    p(""),
    p("This vehicle tracking data has been made available to law enforcement to support their investigation. 
      Unfortunately, data is not available for the day the GAStech employees went missing. 
      Data is only available for the two weeks prior to the disappearance."),
    p(""),
    p("To promote local businesses, Kronos based companies provide a Kronos Kares benefit card to GASTech employees giving them discounts and rewards in exchange for collecting information about their credit card purchases and preferences as recorded on loyalty cards. 
      This data has been made available to investigators in the hopes that it can help resolve the situation.
      However, Kronos Kares does not collect personal information beyond purchases."),
    p(""),
    p("As a visual analytics expert assisting law enforcement, 
      we developed visualisation to identify which GASTech employees made which purchases and identify suspicious patterns of behavior. 
      The STAGEM application will answer the following questions:"),
    p(""),
    p("1. Identifing the most popular locations and when they are popular using credit card and loyalty card transactions"),
    p("2. Analysis of vehicle GPS data with credit and loyalty card data"),
    p("3. Inference of each credit card and loyalty card owners"),
    p("4. Identify potential informal or unoffical relationships between GASTech personnel"),
    p("5. Identify evidence of suspicious activity and locations"),
    p(""),
    p("The application guide is available ",
      a("here.",
        href="https://isss608g1group5.netlify.app/userguide.html")),
    p("Our papers and documents are available on our ",
      a("website ", 
        href = "https://isss608g1group5.netlify.app"),
      "and ",
      a("Github.",
        href="https://github.com/jovinkahartanto/Visual-Analytics---Group-Project")),
    p("ISSS608 G1 Group 5"),
    p("Authors: Chen Yuxi, Jovinka Hartanto, Lim Yong Kai"),
    )
}


