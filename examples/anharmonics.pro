; NASA Ames PAH IR Spectroscopic Database
;
; This is an example of fitting a constructed spectrum built around
; the functionality provided by the Suite and should help confirm that
; the AmesPAHdbIDLSuite has been properly installed.
; 
; Additional information can be found at
; http://www.astrochem.org/pahdb, in Bauschlicher et al. 2010, The
; Astrophysical Journal Supplement Series, 189, 341 and in Boersma et
; al. 2014, The Astrophysical Journal Supplement Series, 211, 8.
;
; USAGE
;   anharmonics
;
PRO ANHARMONICS

  ; read in the default database defined by the
  ; environement variable !AMESPAHDEFAULTDB or
  ; the system variable AMESPAHDEFAULTDB.
  ; use the keyword FILENAME if these have not
  ; been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite') 

  ; get the integrated cross-sections for coronene
  transitions = pahdb->GetTransitionsByUID(18)

  ; calculate the emission spectrum after
  ; absorbing a 4 eV (CGS units) photon
  transitions->Cascade,4D * 1.603D-12

  ; convolve the bands with a Lorentzian of 
  ; FWHM 20 wavenumbers
  spectrum1 = transitions->Convolve(/Anharmonic) 

  ; convolve the bands with a Lorentzian and
  ; treat anharmonics
  spectrum2 = transitions->Convolve(FWHM=30) 

  ; plot the convolved spectra
  spectrum1->Plot,/Fill

  spectrum2->Plot,/Oplot,/Fill,Legend=0,Color=4

  ; clean up
  OBJ_DESTROY,[spectrum2, spectrum1, transitions, pahdb]
  
END
