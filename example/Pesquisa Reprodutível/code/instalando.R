# install rmarkdown
if(!("rmarkdown") %in% installed.packages()) install.packages("rmarkdown")

# loading library rmarkdown
library(rmarkdown)

if(!('tinytex') %in% installed.packages()) install.packages('tinytex')
library(tinytex)

tinytex::install_tinytex()

# install tinytex
install_tinytex(force = FALSE, 
                dir = "auto", 
                repository = "ctan", 
                extra_packages = NULL,
                add_path = TRUE)
tinytex_root()
# [1] "C:\\projetos\\project-rmarkdown\\auto"
use_tinytex(from = "C:\\projetos\\project-rmarkdown\\auto")
#Use of uninitialized value in bitwise or (|) at C:\projetos\project-rmarkdown\auto\texmf-dist\scripts\texlive\tlmgr.pl line 1482.
#Restart R and your editor and check if tinytex::tinytex_root() points to C:\projetos\project-rmarkdown\auto
