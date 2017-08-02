; NASA Ames PAH IR Spectroscopic Database
;
; This is an example of fitting an astronomical spectrum built around
; the functionality provided by the Suite and should help confirm that
; the AmesPAHdbIDLSuite has been properly installed.
; 
; Additional information can be found at
; http://www.astrochem.org/pahdb, in Bauschlicher et al. 2010, The
; Astrophysical Journal Supplement Series, 189, 341 and in Boersma et
; al. 2014, The Astrophysical Journal Supplement Series, 211, 8.
;
; USAGE
;   stability_of_fit_a_spectrum
;
PRO STABILITY_OF_FIT_A_SPECTRUM

  ; the Spitzer IRS/SL 10 - 15 micron spectrum of NGC7023 
  file = 'ngc7023.dat'

  ; read observations into AmesPAHdbIDLSuite_Observation
  observation = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                        file, $
                        Units=AmesPAHdbIDLSuite_CREATE_OBSERVATION_UNITS_S(AUNIT=3, OUNIT=1))

  ; turn wavelength into frequency
  observation->AbscissaUnitsTo,1

  ; rebin to uniform frequency grid
  observation->Rebin,5D,/Uniform
  
  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; retrieve all the transitions from the database
  transitions = pahdb->getTransitionsByUID( -1 )

  ; have every PAH absorb 6 eV (CGS units) and include the temperature
  ; cascade
  transitions->Cascade,6D * 1.6021765D-12
  
  ; shift data 15 wavenumber to the red
  transitions->Shift,-15D

  ; convolve the transitions into a spectrum
  spectrum = transitions->Convolve( $
             Grid=observation->getGrid(), $
             FWHM=15D, $
             /Gaussian)

  OBJ_DESTROY,transitions
  
  ; run stability test
  niterations = 10

  data = REPLICATE({norm: 0D, $
                    anion: 0D, $
                    neutral: 0D, $
                    cation:0D, $
                    small:0D, $
                    large:0D, $
                    pure:0D, $
                    nitrogen: 0D,$
                    solo: 0L, $
                    duo: 0L, $
                    trio: 0L, $
                    quartet: 0L, $
                    quintet: 0L}, niterations)

  FOR i = 0, niterations - 1 DO BEGIN

    ; fit the spectrum
    fit = spectrum->Fit(observation)

    ; store breakdown

    d = data[i]

    STRUCT_ASSIGN,fit->getBreakdown(), d

    data[i] = d
    
    data[i].norm = fit->getNorm()
    
    ; sort the spectra based on their total contribution to the flux
    fit->Sort,/Flux

    ; remove most contributing species from spectrum
    spectrum->Intersect,(fit->getUids())[1:*]

    ; clean up fit
    OBJ_DESTROY,fit
    
  ENDFOR

  PRINT

  ; clean up
  OBJ_DESTROY,[spectrum, pahdb]

  ; plot ionized fraction
  PLOT,INDGEN(niterations), data.cation / (data.cation + data.neutral),XTITLE='iteration [#] / species removed [#]',YTITLE='ionized fraction [ratio]',YRANGE=[0,1],/YSTYLE

  key = ''
  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot large fraction
  PLOT,INDGEN(niterations), data.large / (data.large + data.small),XTITLE='iteration [#] / species removed [#]',YTITLE='large fraction [ratio]',YRANGE=[0,1],/YSTYLE

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot nitrogen fraction
  PLOT,INDGEN(niterations), data.nitrogen / (data.nitrogen + data.pure),XTITLE='iteration [#] / species removed [#]',YTITLE='nitrogen fraction [ratio]',YRANGE=[0,1],/YSTYLE

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot solo/(duo+trio)
  PLOT,INDGEN(niterations), data.solo / (data.duo + data.trio),XTITLE='iteration [#] / species removed [#]',YTITLE='solo/duo+trio [ratio]',YRANGE=[0,1],/YSTYLE

END
