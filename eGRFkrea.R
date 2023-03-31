# Calculate eGFRkrea
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


eGFRkrea <- function(sex, age, krea){
  cat("Syntax\n eGFRkrea(K/M, ålder, krea\n\n")

  cat("Kön:   ", sex, "\nÅlder: ", age, "\nKrea:  ", krea, "\n\n")

  if(sex=="M" & krea <180){
    X=2.56+0.00968*(180-krea)
  }

  if(sex=="M" & krea >=180){
    X=2.56-0.926*log(krea/180)
  }

  if(sex=="K" & krea <150){
    X=2.5+0.0121*(150-krea)
  }

  if(sex=="K" & krea >=150){
    X=2.5-0.926*log(krea/150)
  }

  gfr <- exp(X-0.0158*age+(0.438*log(age)))

  gfr
}
