#'@title Detect and Read File Type and Content
#'
#'@description Automatically detects the file type and attempts to read tabular
#'  content from various file formats, including CSV, HTML, XLS, DOC, XLSX,
#'  DOCX, and PDF. Detection is based on file extensions, magic numbers (file
#'  signatures), and trial-and-error attempts using available R packages. The
#'  function is resilient to incorrect file extensions and adapts dynamically
#'  based on available packages.
#'
#'  This function acts as an automated table reader, focusing on best-effort
#'  content extraction. If reading fails for any format, it will **fail
#'  silently** — without displaying errors or warnings. Users should use this
#'  function cautiously, as it intentionally suppresses detailed error reporting
#'  to ensure smooth fallback behavior.
#'
#'@section External Dependencies: Reading `.doc` files may require external
#'  system dependencies such as
#' **LibreOffice**, which enables content extraction via back-end system calls.
#'  While this functionality is available, reading DOC files is generally not
#'  recommended unless necessary, due to their less structured nature.
#'
#'@param fn Character string. Path to the file to be read.
#'
#'@param omit (Optional) Character vector. A vector of file types to skip during
#'  detection. Supported types include: `"html"`, `"csv"`, `"tsv"`, `"xls"`,
#'  `"doc"`, `"xlsx"`, `"docx"`, and `"pdf"`. Defaults to `NULL` (no types are
#'  skipped).
#'
#'@return A data structure suitable for downstream use: either a `data.frame`,
#'  `matrix`, or a list of such objects. If a list is returned, it may be
#'  homogeneous or heterogeneous depending on the file content.
#'
#'@keywords internal
file_op_detect_and_read <- function(fn, omit = NULL) {

  omit <- tryCatch(
    intersect(
      omit,
      c("html", "csv", "tsv", "xls", "doc", "xlsx", "docx", "pdf")
    ),
    error = function(e) NULL
  )

  file_op_validate_file_input(fn) # Common file checks

  sig <- file_op_identify_format_by_signature(fn)
  content <- NULL

  if (sig == "txt") {
    content <- file_op_read_txt(fn, omit = omit)
  } else if (sig == "pdf") {
    content <- file_op_read_pdf(fn, omit = omit)
  } else if (sig == "xls_doc") {
    content <- file_op_read_xls_doc(fn, omit = omit)
  } else if (sig == "xlsx_docx") {
    content <- file_op_read_xlsx_docx(fn, omit = omit)
  }

  return(content)
}
