require(rmarkdown)

render('Report.Rmd',
       pdf_document(number_sections = TRUE,
                    fig_width = 4,
                    fig_height = 3))
