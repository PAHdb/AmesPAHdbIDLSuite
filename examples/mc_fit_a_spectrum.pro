; docformat = 'rst'

;+
;
; This is an example of fitting an astronomical spectrum using a Monte Carlo
; approachand should confirm that the AmesPAHdbIDLSuite has been correctly
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

  ; fit the spectrum using NNLC
  fit = spectrum->Fit(observation, Notice=0)

  ; store results
  fits = fit->getFit()

  results = fit->getBreakdown()

  errors = fit->getError()

  classes = fit->getClasses()

  ; clean up fit
  OBJ_DESTROY,[fit]

  ; get observations struct
  observation_s = observation.Get()

  ; clean up observation
  OBJ_DESTROY,[observation]

  ; Monte Carlo runs
  nruns = 1024

  ny = N_ELEMENTS(observation_s.data.y)

  FOR run = 0L, nruns - 1L DO BEGIN

    ; show progress
    PRINT,FORMAT='(80("' + STRING(8B) + '"),"mc run:",X,I4,"/",I4,$)',run+1L,nruns

    ; permutate spectrum
    y = observation_s.data.y - $
        observation_s.data.continuum + $
        observation_s.data.ystdev * (2D * RANDOMU(seed, ny, /DOUBLE) - 1D)

    ; fit the spectrum using NNLC
    fit = spectrum->Fit(y, observation_s.data.ystdev, Notice=0)

    ; store results
    fits = [fits, fit->getFit()]

    results = [results, fit->getBreakdown()]

    errors = [errors, fit->getError()]

    classes = [classes, fit->getClasses()]

    ; clean up fit
    OBJ_DESTROY,[fit]
  ENDFOR

  PRINT
  PRINT,"done!"
  PRINT

  ; clean up
  OBJ_DESTROY,[spectrum, pahdb]

  ; print results
  tags = TAG_NAMES(results)

  ntags = N_TAGS(results)

  PRINT,FORMAT='(A8,X,A6,X,A6,X,A6,X,A6,X,A6)','','MIN','MAX','MEDIAN','MEAN','STDEV'
  FOR i = 0, ntags - 1 DO $
    PRINT,FORMAT='(A8,X,F6.2,X,F6.2,X,F6.2,X,F6.2,X,F6.3)', $
          tags[i],MIN(results.(i)),MAX(results.(i)),MEDIAN(results.(i)), MEAN(results.(i)),STDDEV(results.(i))
  PRINT,FORMAT='(A8,X,F6.2,X,F6.2,X,F6.2,X,F6.2,X,F6.3)','ERROR',MIN(errors),MAX(errors),MEDIAN(errors),MEAN(errors),STDDEV(errors)

  ; display results for charge
  PLOT,1D4/observation_s.data.x,observation_s.data.y-observation_s.data.continuum,PSYM=0,XTITLE='wavelength [micron]',YTITLE='surface brightness [MJy/sr]'
  ERRPLOT,1D4/observation_s.data.x,observation_s.data.y-observation_s.data.continuum-observation_s.data.ystdev,observation_s.data.y-observation_s.data.continuum+observation_s.data.ystdev

  fits = REFORM(fits, ny, nruns + 1)

  m_fits = MOMENT(fits, DIMENSION=2, MAXMOMENT=2)
  ERRPLOT,1D4/observation_s.data.x,m_fits[*,0]-SQRT(m_fits[*,1]),m_fits[*,0]+SQRT(m_fits[*,1]),THICK=4
  OPLOT,1D4/observation_s.data.x,m_fits[*,0],THICK=4
  XYOUTS,0.85,0.80,'FIT',/NORMAL,/ALIGN

  classes = REFORM(classes, ny, nruns + 1)

  m_anion = MOMENT(classes.anion, DIMENSION=2, MAXMOMENT=2)
  ERRPLOT,1D4/observation_s.data.x,m_anion[*,0]-SQRT(m_anion[*,1]),m_anion[*,0]+SQRT(m_anion[*,1]),COLOR=2
  OPLOT,1D4/observation_s.data.x,m_anion[*,0],COLOR=2
  XYOUTS,0.85,0.75,'ANION',COLOR=2,/NORMAL,/ALIGN

  m_neutral = MOMENT(classes.neutral, DIMENSION=2, MAXMOMENT=2)
  ERRPLOT,1D4/observation_s.data.x,m_neutral[*,0]-SQRT(m_neutral[*,1]),m_neutral[*,0]+SQRT(m_neutral[*,1]),COLOR=3
  OPLOT,1D4/observation_s.data.x,m_neutral[*,0],COLOR=3
  XYOUTS,0.85,0.70,'NEUTRAL',COLOR=3,/NORMAL,/ALIGN

  m_cation = MOMENT(classes.cation, DIMENSION=2, MAXMOMENT=2)
  ERRPLOT,1D4/observation_s.data.x,m_cation[*,0]-SQRT(m_cation[*,1]),m_cation[*,0]+SQRT(m_cation[*,1]),COLOR=4
  OPLOT,1D4/observation_s.data.x,m_cation[*,0],COLOR=4
  XYOUTS,0.85,0.65,'CATION',COLOR=4,/NORMAL,/ALIGN

END
