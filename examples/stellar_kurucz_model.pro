; docformat = 'rst'

;+
;
; This is an example of creating an astronomical PAH emission spectrum
; using a Kurucz stellar model for excitination, built around the
; functionality provided by the AmesPAHdbIDLSuite and should help
; confirm that the it has been properly installed. The source code is
; annotated to guide users and developers in the inner workings of the
; suite.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> stellar_kurucz_model
;
; :Author:
;   Dr. Christiaan Boersma
;
; :Copyright:
;   BSD licensed
;
; :History:
;   Changes::
;
;     08-19-2019
;     Documentation added. Christiaan Boersma.
;-

;+
; Procedure creating an astronomical PAH emission spectrum using a
; Kurucz stellar model for excitination.
;
; :Categories:
;   Example
;-
PRO STELLAR_KURUCZ_MODEL

  COMPILE_OPT IDL2

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use the
  ; keyword FILENAME if these have not been set
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
