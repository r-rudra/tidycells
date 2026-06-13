
# Dependencies in R-Package
# Package     | Popularity | Actively Maintained | Ease of Use | Output Quality         | Best For
# -------------|------------|--------------------|-------------|------------------------|-------------------------------------------------------------
# attachment   | High       | Yes                | Very Easy   | Good                   | Listing packages used in code (pkg::, library(), require()) and updating DESCRIPTION
# pkgnet       | Moderate   | Yes                | Moderate    | Excellent (Graphical)  | Visualization and reporting of package dependencies
# pkgdepends   | Moderate   | Yes                | Easy        | Good                   | Modern dependency management and analysis
# codetools    | Low        | Yes                | Manual      | Basic                  | Custom/scripted static code analysis
# devtools     | Very High  | Yes                | Very Easy   | Good (for CRAN-style deps) | General package development, but not focused on scanning pkg:: usage in code

attachment::att_from_rscripts()
attachment::att_from_examples()
attachment::att_from_description()
attachment::att_from_namespace()
attachment::att_from_rscripts("tests")

attachment::att_from_description() %>%
  setdiff(attachment::att_from_rscripts()) %>%
  setdiff(attachment::att_from_rscripts("tests")) %>%
  setdiff(attachment::att_from_namespace())

# Works after install
pkgnet::CreatePackageReport(pkg_name = "tidycells", pkg_path = ".")


