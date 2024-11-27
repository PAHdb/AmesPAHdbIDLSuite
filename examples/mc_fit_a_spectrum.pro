; docformat = 'rst'

;+
;
; This is an example of fitting an astronomical spectrum using a Monte Carlo
; approach and should confirm that the AmesPAHdbIDLSuite has been correctly
; installed. The source code is annotated to guide users and developers in
; the inner workings of the suite.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> mc_fit_a_spectrum
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
;     04-15-2023
;     Remove redundant code and don't destroy observation. Christiaan Boersma.
;     05-03-2022
;     Add plotting size distribution. Christiaan Boersma.
;     04-28-2022
;     Adapt for piecewise errors. Christiaan Boersma.
;     04-27-2022
;     Use MCFit instead of Fit. Christiaan Boersma.
;     07-06-2021
;     Cleaned up progress bar. Christiaan Boersma.
;     04-30-2021
;     First version. Christiaan Boersma.
;-

;+
; Procedure demonstrating fitting an astronomical spectrum using the
; AmesPAHdbIDLSuite and establishing uncertainties using a Monte-Carlo
; approach.
;
; :Categories:
;   Example
;-
PRO MC_FIT_A_SPECTRUM

  COMPILE_OPT IDL2

  ; the Spitzer IRS/SL 10 - 15 micron spectrum of NGC7023
  file = 'ngc7023.dat'

  ; read observations into AmesPAHdbIDLSuite_Observation
  observation = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                        file, $
                        Units=AmesPAHdbIDLSuite_CREATE_OBSERVATION_UNITS_S(AUNIT=3, OUNIT=1))

  ; turn wavelength into frequency
  observation->AbscissaUnitsTo,1

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; search for the UIDs for a subset of PAHs
  uids = pahdb->Search("magnesium=0 oxygen=0 iron=0 silicium=0 chx=0 ch2=0 c>20 h>0")

  ; put back fullerenes, which have h=0
  fullerenes = [717, 720, 723, 735, 736, 737]

  uids = [uids, fullerenes]

  ; retrieve the transitions for the subset of PAHs
  transitions = pahdb->getTransitionsByUID(uids)

  ; apply full temperature cascade emission model with an 8 eV photon
  transitions->Cascade,8D*1.602D-12

  ; shift data 15 wavenumber to the red
  transitions->Shift,-15D

  ; convolve the transitions into a spectrum
  spectrum = transitions->Convolve(Grid=observation->getGrid(), $
                                   FWHM=15D, $
                                   /Gaussian)

  ; clean up transitions
  OBJ_DESTROY,[transitions]

  ; fit the spectrum using Monte Carlo approach
  mcfit = spectrum->MCFit(observation, 1024)

  ; clean up
  OBJ_DESTROY,[spectrum]

  ; print results
  mcbd = mcfit->GetBreakdown()

  tags = TAG_NAMES(mcbd)

  ntags = N_TAGS(mcbd)

  PRINT,FORMAT='(A8,X,A6,X,A6)','','MEAN','STDEV'
  FOR i = 0, ntags - 1 DO $
    PRINT,FORMAT='(A8,X,F6.2,X,F6.3)',tags[i],mcbd.(i)[0],SQRT(mcbd.(i)[1])
  mcerr = mcfit->GetError()

  tags = TAG_NAMES(mcerr)

  ntags = N_TAGS(mcerr)

  FOR i = 0, ntags - 1 DO $
    PRINT,FORMAT='(A8,X,F6.2,X,F6.3)',tags[i],mcerr.(i)[0],mcerr.(i)[1]

  mcfit->Plot,/Wavelength

  key = ''
  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  mcfit->Plot,/Wavelength,/Size

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  mcfit->Plot,/Wavelength,/Charge

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  mcfit->Plot,/Wavelength,/Composition

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  mcfit->Plot,/DistributionSize,NBins=10L,Min=20,Max=200

  OBJ_DESTROY,[mcfit, pahdb, observation]

END
