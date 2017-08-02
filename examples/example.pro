; NASA Ames PAH IR Spectroscopic Database
;
; This is the adaption of the example shown in Lst. 1 of Bauschlicher
; et al. (2010) and should confirm that the AmesPAHdbIDLSuite has been
; properly installed.
; 
; Additional information can be found at
; http://www.astrochem.org/pahdb, in Bauschlicher et al. 2010, The
; Astrophysical Journal Supplement Series, 189, 341 and in Boersma et
; al. 2014, The Astrophysical Journal Supplement Series, 211, 8.
;
; USAGE
;   example
;
PRO example

  ; read in the default database defined by the
  ; environement variable !AMESPAHDEFAULTDB or
  ; the system variable AMESPAHDEFAULTDB.
  ; use the keyword FILENAME if these have not
  ; been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite') 

  ; get the integrated cross-sections for coronene
  transitions = pahdb->getTransitionsByUID(18)

  ; plot the 'stick' spectrum
  transitions->Plot

  key = ''
  read,key,prompt="Press <enter> to continue..."

  ; calculate the emission spectrum at the temperature 
  ; reached after absorbing a 4 eV (CGS units) photon
  transitions->CalculatedTemperature,4D * 1.603D-12

  ; plot the emission 'stick' spectrum at that temperature
  transitions->Plot

  read,key,prompt="Press <enter> to continue..."

  ; convolve the bands with a Lorentzian with
  ; FWHM of 30 /cm
  convolved = transitions->Convolve(FWHM=30D) 

  ; plot the convolved spectrum
  convolved->Plot

  ; clean up
  OBJ_DESTROY,[convolved, transitions, pahdb]
END
