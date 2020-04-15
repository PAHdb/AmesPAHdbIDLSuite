; docformat = 'rst'

;+
;
; This is an example of fitting an astronomical spectrum and should
; confirm that the AmesPAHdbIDLSuite has been correctly installed. The
; source code is annotated to guide users and developers in the inner
; workings of the suite.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> fit_a_spectrum
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
; Procedure demonstrating fitting an astronomical spectrum using the
; AmesPAHdbIDLSuite.
;
; :Categories:
;   Example
;-
PRO FIT_A_SPECTRUM

  ; the Spitzer IRS/SL 10 - 15 micron spectrum of NGC7023
  file = 'ngc7023.dat'

  ; read observations into AmesPAHdbIDLSuite_Observation
  observation = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                        file, $
                        Units=AmesPAHdbIDLSuite_CREATE_OBSERVATION_UNITS_S(AUNIT=3, OUNIT=1))

  ; turn wavelength into frequency
  observation->AbscissaUnitsTo,1

  ; rebin to uniform frequency grid
  ;observation->Rebin,5D,/Uniform

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; retrieve the transitions from the database for a subset of PAHs
  transitions = pahdb->getTransitionsByUID( $
                pahdb->Search("magnesium=0 oxygen=0 iron=0 silicium=0 chx=0 ch2=0 c>20 h>0"))

  ; shift data 15 wavenumber to the red
  transitions->Shift,-15D

  ; convolve the transitions into a spectrum
  spectrum = transitions->Convolve(Grid=observation->getGrid(), $
                                   FWHM=15D, $
                                   /Gaussian)

  ; fit the spectrum
  fit = spectrum->Fit(observation)

  ; clean up spectrum
  obj_destroy,[spectrum]

  ; display fit and breakdown
  fit->Plot,/Wavelength

  key = ''
  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Wavelength,/Residual

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Wavelength,/Size

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Wavelength,/Charge

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  fit->Plot,/Wavelength,/Composition

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; predict 3 - 20 um spectrum
  transitions->Intersect, $
          fit->getUIDs()

  xrange = 1D4 / [20D, 3D]

  spectrum = transitions->Convolve(FWHM=15D, $
                                   /Gaussian, $
                                   XRange=xrange)

  coadded = spectrum->Coadd(Weights=fit->getWeights())

  coadded->Plot

  ; clean up objects
  OBJ_DESTROY,[coadded, spectrum, fit, transitions, pahdb, observation]

END
