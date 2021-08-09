overviewUI <- function(id) {
  tagList(
    h1("Vast Challenge 2021, Mini Challenge 2",
       style='background-color:lightgrey',
       align="center"),
    p("This is a sample text"),
    p("Our papers and documents are available on our ",
      a("website ", 
        href = "https://isss608g1group5.netlify.app"),
      "and ",
      a("Github.",
        href="https://github.com/jovinkahartanto/Visual-Analytics---Group-Project")),
    p("ISSS608 G5"),
    p("Authors: Chen Yuxi, Jovinka Hartanto, Lim Yong Kai"),
    HTML('<iframe width="1120" height="630" src="https://vast-challenge.github.io/2021/" 
                          frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; 
                          picture-in-picture" allowfullscreen></iframe>')
  )
}


