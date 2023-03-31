# Calculate eGFRCysC, relativt
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#  eGFR (CystatinC) Relativt beräknas med formel ”CAPA”
#  eGFR = (130 x cystatin C- 1.069 x age- 0.117) - 7,0


eGFRCysC <- function(Age, CysC){
  message("Syntax\n eGFRCysC( Age, CysC\n\n")

  message( "Age: ", Age, "\nCysC:  ", CysC, "\n\n")


  gfr <- (130 * ((CysC)^(-1.069)) *  (Age^(-0.117))) -7.0

  gfr
}
