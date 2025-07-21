# Instructor Notes for Pangborn 2025 tutorial: “Publication-quality data visualizations using the R tidyverse”

These notes are just to cover some basic situations that are likely to show up in `R` tutorials at various sensory conferences.  

## Non-American keyboards

Many participants will be using keyboards that are not configured for English-language use, and there are even some slight differences between Commonwealth and US English keyboards.  Be prepared to ask participants to show you where specific key combinations are.  

If the users are working entirely in a non-English runtime environment there may be other problems that may not be resolvable directly by a non-speaker.

## Helping students find their files

- Trouble finding downloads
  - Users have been instructed to pre-download the materials.  If they can't, they are available from the [github repo](https://github.com/jlahne/pangborn-tutorial-2025?tab=readme-ov-file#quick-reference).  There are also links to instructions on how to extract these files.
  - If the user can't find those files, prompt them to just redownload--it should be faster.
  - If the user doesn't know where there workspace is, save to the Desktop as the easiest spot to work from.
- Trouble finding files once RStudio is open
  - Have the user close out everything (save work if necessary as a `.txt` or `.R` file) and then have the re-open RStudio from the included `pangborn-tutorial-2025.Rproj` so as to properly set up the working directory.
  - If for some reason the user can't open an `.Rproj` file, help them use the `Set Working Directory` menu option or the `setwd()` command as a last resort.

## Can't unzip files once downloaded

Often users on Windows will download a `.zip` archive and, because Windows Explorer can open these as folders, try to open `R` files from within them.  This will not work.

The guide for how to unzip files is [here](https://www.hostinger.com/tutorials/how-to-unzip-files). 

The link to the tutorial part covering this is [here](https://lhamilton.shinyapps.io/pre-tutorial-setup-2025/#section-downloading-data).

## Can't run RStudio

Users were prompted to set up RStudio before beginning the tutorial.  If they do not have it installed, it can be downloaded and installed from [Posit](https://posit.co/download/rstudio-desktop/#download).  If they do not have `R` installed yet, this is a necessary first step.

If the user has `R` but cannot install RStudio because of security settings, we have flash drives with a version of RStudio that can be run without admin privileges.
