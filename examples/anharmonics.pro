; docformat = 'rst'

;+
;
; This is an example of calculating the PAH emission spectrum of
; coronene where the bands have been convolved using anharmonic
; profiles buildt around the functionality provided by the
; AmesPAHdbIDLSuite and should help confirm that the it has been
; properly installed. The source code is annotated to guide users and
; developers in the inner workings of the suite.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> anharmonics
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
; Procedure calculating the anharmonic spectrum of coronene.
;
; :Categories:
;   Example
;-
PRO ANHARMONICS

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB.  use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; get the integrated cross-sections for coronene
  transitions = pahdb->GetTransitionsByUID(18)

  ; calculate the emission spectrum after
  ; absorbing a 4 eV (CGS units) photon
  transitions->Cascade,4D * 1.603D-12

  ; convolve the bands with a Lorentzian of FWHM 20 wavenumbers and
  ; treat anharmonics
  spectrum1 = transitions->Convolve(/Anharmonic)

  ; convolve the bands with a Lorentzian with a Lorentian of FWHM 30
  ; wavenumbers
  spectrum2 = transitions->Convolve(FWHM=30)

  ; plot the convolved spectra
  spectrum1->Plot,/Fill

  spectrum2->Plot,/Oplot,/Fill,Legend=0,Color=4

  ; clean up
  OBJ_DESTROY,[spectrum2, spectrum1, transitions, pahdb]

END
