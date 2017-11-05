library(reshape2)
library(ggplot2)
library(plyr)
library(SPARQL)

#create the function
population_pyramid <- function(geography="S92000003", year=2016) {
  endpoint <- "http://statistics.gov.scot/sparql"
  
  # create query statement
  query <- paste(
    "PREFIX qb: <http://purl.org/linked-data/cube#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX purl: <http://purl.org/linked-data/sdmx/2009/dimension#>
    PREFIX scot: <http://statistics.gov.scot/def/dimension/>
    
    SELECT ?Age ?Sex ?Population
    WHERE {
    ?s qb:dataSet <http://statistics.gov.scot/data/population-estimates-current-geographic-boundaries>;
    purl:refArea <http://statistics.gov.scot/id/statistical-geography/",
    geography,
    ">;
    purl:refPeriod <http://reference.data.gov.uk/id/year/",
    year,
    ">;
    scot:sex ?SexURI;
    scot:age ?AgeURI;
    <http://statistics.gov.scot/def/measure-properties/count>  ?Population.
    ?AgeURI rdfs:label ?Age;
    <http://www.w3.org/ns/ui#sortPriority> ?Sort.
    ?SexURI rdfs:label ?Sex.
    FILTER(?SexURI !=  <http://statistics.gov.scot/def/concept/sex/all>)
    FILTER(?AgeURI NOT IN  (<http://statistics.gov.scot/def/concept/age/all>, <http://statistics.gov.scot/def/concept/age/children-under-16>, <http://statistics.gov.scot/def/concept/age/working-age-16-64>, <http://statistics.gov.scot/def/concept/age/pensionable-age-65-and-over> ))
    }
    
    ORDER BY ?Sex ?Sort", sep = "")
  
  qd <- SPARQL(endpoint, query)
  df <- qd$results
  
  df$Population <- ifelse(df$Sex == "Male",df$Population * -1,df$Population)
  #Age has been ordered already by sparql query, so do this to stop 5-9 category appearing in wrong place
  df$Age <- factor(df$Age, levels = unique(df$Age))
  
  
  plot1 <- ggplot(df, aes(x = Age, y = Population, fill = Sex)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = pretty(df$Population),
                       labels =  abs(pretty(df$Population))) +
    coord_flip() +
    scale_fill_brewer(palette = "Set1")  +
    theme_bw()
  plot1
  
}

#run the function for Scotland in 2016
population_pyramid("S92000003", 2016)
