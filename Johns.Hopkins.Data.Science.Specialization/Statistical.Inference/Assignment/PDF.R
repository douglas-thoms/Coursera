# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("Stats.Inf.Assg.2.Report.Rmd")
markdownToHTML('Stats.Inf.Assg.2.Report.md', 'Stats.Inf.Assg.2.Report.html', options=c("use_xhml"))
system("pandoc -s Stats.Inf.Assg.2.Report.html -o Stats.Inf.Assg.2.Report.pdf")