library(officer)

aux <- readLines("inst/ms/hawaii-arth-mete-supp.aux") 

labs <- grep("newlabel\\{.*", aux, value = TRUE) |> 
    gsub("\\\\newlabel\\{|\\{\\{", "\\{", x = _) |> 
    strsplit("\\}|\\{") |> 
    sapply(function(x) x[c(2, 4)]) |> 
    t() |> 
    (\(x) x[grepl("sec-|fig-", x[, 1]), ])()





doc <- read_docx("inst/ms/hawaii-arth-mete.docx") |> 
    body_replace_all_text(
        old_value = "ssec", 
        new_value = "Supplemental Section "
    ) |> 
    body_replace_all_text(
        old_value = "sfig", 
        new_value = "Supplemental Figure "
    )

for(i in 1:nrow(labs)) {
    doc <- body_replace_all_text(
        doc,
        old_value = sprintf("{%s}", labs[i, 1]), 
        new_value = labs[i, 2], 
        fixed = TRUE, 
        warn = FALSE
    )
}

print(doc, target = "inst/ms/hawaii-arth-mete-ready.docx")
