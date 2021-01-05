# RANN Applications
Authors: Justin Chang, Yasmina Abukhadra, Daniel Ortega, Abhinav Godavarthi, Michael Barresi, Alex Lawson

# Applying Randomized Approximate Nearest Neighbors

Given n points in Rm, the k-nearest-neighbors algorithm finds the k nearest neighbors of each point on a Euclidean metric. The Randomized Nearest Neighbor Algorithm is similar to the k-nearest neighbors algorithm, but it runs quickly and preserves performance by finding the approximate nearest neighbors instead of the exact neighbors. Additionally, RANN implements several useful results from real analysis, such as the Johnson-Lindenstrauss theorem (Jones, 2011). We first revised the RANN algorithm to accept custom inputs and used R to develop a workflow for visualizing results as dynamic interactive networks (Bender-DeMoll, 2016). 

This project applies RANN to a diverse collection of real-world datasets: bird images from Caltech-UCSD Birds 200 (Welinder, 2010), COVID-19 data from different regions in the US over many time points (“Novel Corona Virus 2019 Dataset," 2020), genomic profiles across human tissue samples (ENCODE, 2012) (Davis, 2018), single cell RNA sequencing data (Shekhar, 2016), and football team performance based on positional and functional data (PFF Premium Stats, 2020) (Pro Football Reference, 2020). Collectively, these analyses demonstrate that RANN is robust, high-performance, and insightful. Through these varied analyses, this project hopes to provide insight into the performance and possible applications of RANN. The selected applications, though not exhaustive, also yield interesting results with respect to grouping altered images amidst noisy perturbations, relating COVID outcomes across the United States, deducing the identity of tissue samples, analyzing novel RNA sequencing data, and predicting season outcomes for football teams (Jones, 2011).

The visualizations for our work can be found at https://justinchang1124.shinyapps.io/rann_app

# Works Cited

Jones, Peter Wilcox, Andrei Osipov, and Vladimir Rokhlin. "Randomized approximate nearest neighbors algorithm." Proceedings of the National Academy of Sciences 108.38 (2011): 15679-15686.

Davis, Carrie A et al. “The Encyclopedia of DNA elements (ENCODE): data portal update.” Nucleic acids research vol. 46,D1 (2018): D794-D801. doi:10.1093/nar/gkx1081

ENCODE Project Consortium. “An integrated encyclopedia of DNA elements in the human genome.” Nature vol. 489,7414 (2012): 57-74. doi:10.1038/nature11247

Novel Corona Virus 2019 Dataset. https://kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset. Accessed 16 Dec. 2020.

PFF Premium Stats, Pro Football Focus, 2020, premium.pff.com/. Accessed 15 Dec. 2020. 

Pro Football Reference, Sports Reference LLC, 2020, www.pro-football-reference.com/. Accessed 16 Dec. 2020. 

Shekhar, Karthik et al. “Comprehensive Classification of Retinal Bipolar Neurons by Single-Cell Transcriptomics.” Cell vol. 166,5 (2016): 1308-1323.e30. doi:10.1016/j.cell.2016.07.054

Welinder P., Branson S., Mita T., Wah C., Schroff F., Belongie S., Perona, P. “Caltech-UCSD Birds 200”. California Institute of Technology. CNS-TR-2010-001. 2010. download pdf

2018_10_rann_c_for_gal.tar, from Andrei Osipov

Andras Sali and Dean Attali (2020). shinycssloaders: Add Loading Animations to a
'shiny' Output While It's Recalculating. R package version 1.0.0.
https://CRAN.R-project.org/package=shinycssloaders

Dean Attali (2020). shinyjs: Easily Improve the User Experience of Your Shiny Apps
in Seconds. R package version 2.0.0. https://CRAN.R-project.org/package=shinyjs

Gagolewski M. and others (2020). R package stringi: Character string processing
facilities. http://www.gagolewski.com/software/stringi/

Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2020). dplyr: A
Grammar of Data Manipulation. R package version 1.0.2.
https://CRAN.R-project.org/package=dplyr

Ramnath Vaidyanathan, Yihui Xie, JJ Allaire, Joe Cheng, Carson Sievert and Kenton
Russell (2020). htmlwidgets: HTML Widgets for R. R package version 1.5.3.
https://CRAN.R-project.org/package=htmlwidgets

Skye Bender-deMoll (2016). ndtv: Network Dynamic Temporal Visualizations. R package version 0.10. http://statnet.org

Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard: Create Dashboards
with 'Shiny'. R package version 0.7.1.
https://CRAN.R-project.org/package=shinydashboard

Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020).
shiny: Web Application Framework for R. R package version 1.5.0.
https://CRAN.R-project.org/package=shiny

Yihui Xie, Joe Cheng and Xianying Tan (2020). DT: A Wrapper of the JavaScript
Library 'DataTables'. R package version 0.16.
https://CRAN.R-project.org/package=DT
