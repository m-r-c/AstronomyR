# Converts visible wavelengths of light to RGB colour values
# Adapted from http://www.physics.sfasu.edu/astro/color/spectra.html by Dan Bruton and 
# http://www.noah.org/wiki/Wavelength_to_RGB_in_Python

GAMMA = 0.8

toRGB <- function(wavelength) {
  if (wavelength >= 380 & wavelength <= 440) {
    attenuation = 0.3 + 0.7 * (wavelength - 380) / (440 - 380)
    R = ((-(wavelength - 440) / (440 - 380)) * attenuation) ^ GAMMA
    G = 0.0
    B = (1.0 * attenuation) ^ GAMMA
  } else if (wavelength >= 440 & wavelength <= 490) {
    R = 0.0
    G = ((wavelength - 440) / (490 - 440)) ^ GAMMA
    B = 1.0
  } else if (wavelength >= 490 & wavelength <= 510) {
    R = 0.0
    G = 1.0
    B = (-(wavelength - 510) / (510 - 490)) ^ GAMMA
  } else if (wavelength >= 510 & wavelength <= 580) {
    R = ((wavelength - 510) / (580 - 510)) ^ GAMMA
    G = 1.0
    B = 0.0
  } else if (wavelength >= 580 & wavelength <= 645) {
    R = 1.0
    G = (-(wavelength - 645) / (645 - 580)) ^ GAMMA
    B = 0.0
  } else if (wavelength >= 645 & wavelength <= 750) {
    attenuation = 0.3 + 0.7 * (750 - wavelength) / (750 - 645)
    R = (1.0 * attenuation) ^ GAMMA
    G = 0.0
    B = 0.0
  } else {
    R = 0.0
    G = 0.0
    B = 0.0
  }
  
  rgb(R, G, B)
}
