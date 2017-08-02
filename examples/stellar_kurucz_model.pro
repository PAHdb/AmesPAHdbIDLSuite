; NASA Ames PAH IR Spectroscopic Database
;
; This is an example of creating an astronomical PAH spectrum using a
; Kurucz stellar model for excitination, built around the
; functionality provided by the Suite and should help confirm that the
; AmesPAHdbIDLSuite has been properly installed.
; 
; Additional information can be found at
; http://www.astrochem.org/pahdb, in Bauschlicher et al. 2010, The
; Astrophysical Journal Supplement Series, 189, 341 and in Boersma et
; al. 2014, The Astrophysical Journal Supplement Series, 211, 8.
;
; USAGE
;   stellar_kurucz_model
;
PRO STELLAR_KURUCZ_MODEL

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')
 
  ; use the unique identifier for coronene
  uid = 18

  ; retrieve the transitions from the database
  transitions = pahdb->getTransitionsByUID(uid)

  ; load Kurucz stellar model
  FTAB_EXT,'ckp00_17000.fits',[1,10],angstroms,flem,EXT=1
  
  ; block under/overflow
  e = !EXCEPT

  !EXCEPT = 0

  ; include the temperature cascade
  transitions->Cascade, $
     AMESPAHDBIDLSUITE_CREATE_KURUCZ_STELLARMODEL_S(angstroms, flem), $
     /Star, $
     /StellarModel, $
     /Convolved
  
  ; shift data 15 wavenumber to the red
  transitions->Shift,-15D

  ; convolve stick spectrum
  spectrum = transitions->Convolve(FWHM=15D, XRange=1D4/[15,2.5])
  
  ; plot the spectrum
  spectrum->Plot

  ; clean up
  OBJ_DESTROY,[spectrum, transitions, pahdb]

  ; reset under/overflow errors
  !EXCEPT = e

END
