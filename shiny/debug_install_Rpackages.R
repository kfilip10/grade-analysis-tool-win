# This code installs R packages on the local machine for debugging purposes.
# This does not package with the installer. That is done through the 'add-cran-binary-pkgs.R' script.


#set workspace to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Read package names from req.txt
packages <- readLines("req.txt")

# Remove empty lines and trim whitespace
packages <- trimws(packages[packages != ""])

# Get currently installed packages
installed <- rownames(installed.packages())

# Determine which packages need to be installed
to_install <- packages[!packages %in% installed]

# Install missing packages
if (length(to_install) > 0) {
  cat("Installing packages:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install, dependencies = TRUE)
} else {
  cat("All packages are already installed.\n")
}

# Verify installation
cat("\nVerifying installation...\n")
for (pkg in packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Good install - ", pkg, "\n")
  } else {
    cat("X", pkg, "FAILED\n")
  }
}