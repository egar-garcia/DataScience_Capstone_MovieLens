require(rmarkdown)

render('Report.Rmd', pdf_document(number_sections = TRUE))
