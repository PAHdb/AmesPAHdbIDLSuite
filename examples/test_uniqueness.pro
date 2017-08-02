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
;   test_uniqueness
;
PRO TEST_UNIQUENESS

  ; avoid underflow messages
  !EXCEPT = 0

  ; set number of spectra to co-add, the FWHM to be applied when
  ; convolving and spectral range
  nspectra = 40

  fwhm = 15D

  xrange = [15D, 2.5D]
  
  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; retrieve all the transitions from the database
  transitions = pahdb->getTransitionsByUID( -1 )

  ; retrieve all unique identifiers
  uids = transitions->getUIDs(nuids)

  ; have every PAH absorb 6 eV
  transitions->Cascade,6D * 1.6021765D-12
  
  ; convolve the transitions into a spectrum
  spectrum = transitions->Convolve( $
             Xrange=1D4/xrange, $
             FWHM=fwhm, $
             /Gaussian)

  OBJ_DESTROY,transitions

  spectrum_s = spectrum->Get()

  ; run uniqueness test
  niterations = 1000L

  store = REPLICATE({norm: 0D, $
                     anion: 0D, $
                     neutral: 0D, $
                     cation:0D, $
                     small:0D, $
                     large:0D, $
                     pure:0D, $
                     nitrogen: 0D,$
                     solo: 0D, $
                     duo: 0D, $
                     trio: 0D, $
                     quartet: 0D, $
                     quintet: 0D}, niterations)

  ntags = N_TAGS(store)
  
  selected_weights = REPLICATE({uid:0L, weight:0D}, nspectra)

  indices = LONARR(nspectra, /NOZERO)
  
  FOR i = 0, niterations - 1 DO BEGIN

     ; print progress
     PRINT,"========================================================="
     PRINT,"        ITERATION: " + STRTRIM(STRING(FORMAT='(I0)', i + 1), 2) + "/" + STRTRIM(STRING(FORMAT='(I0)',  niterations), 2)
     PRINT,"========================================================="

     ; create random selection of unique identifiers and avoid doubles
     n = nspectra
     
     WHILE n GT 0 DO BEGIN

        indices[nspectra - n] = LONG(RANDOMU(seed, n) * nuids)

        indices = indices[SORT(indices)]

        u = UNIQ(indices)

        n = nspectra - N_ELEMENTS(u)

        indices[0] = indices[u]
     ENDWHILE

     selected_uids = uids[indices]
     
     ; intersect
     spectrum->Intersect,selected_uids

     ; store data
     data = (spectrum->Get()).data
    
     ; create random weights
     selected_weights.uid = selected_uids
  
     selected_weights.weight = 1D14 * RANDOMU(LONG(SYSTIME(1)), nspectra, /DOUBLE)

     ; co-add the spectra using the random weights
     coadd = spectrum->Coadd(Weights=selected_weights)

     ; reset spectrum
     spectrum->Set,spectrum_s

     ; difference
     spectrum->Difference,selected_uids

     ; fit the spectrum
     fit = spectrum->Fit((coadd.Get()).data.intensity)

     OBJ_DESTROY,coadd

     IF OBJ_VALID(fit) THEN BEGIN
    
       ; breakdown of the fit
       bd_fit = fit->getBreakdown()    

       store[i].norm = fit->getNorm()

       ; breakdown of the original spectrum
       FOR j = 0, nspectra - 1 DO BEGIN

          sel1 = WHERE(selected_weights.uid EQ selected_uids[j])

          sel2 = WHERE(data.uid EQ selected_uids[j])

          data[sel2].intensity *= selected_weights[sel1].weight
       ENDFOR
    
       fit->Set,Data=data,Weights=selected_weights,UIDs=selected_uids
       
       bd_org = fit->getBreakdown()
    
       ; store breakdown fractional difference
       FOR j = 0, ntags - 2 DO store[i].(j + 1) = DOUBLE(bd_fit.(j)) / DOUBLE(bd_org.(j))
   
       ; clean up fit
       OBJ_DESTROY,fit

    ENDIF
    
    ; reset spectrum
    spectrum->Set,spectrum_s

  ENDFOR

  ; clean up
  OBJ_DESTROY,[spectrum, pahdb]

  ; plot distribution anion
  h = HISTOGRAM(store.anion, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction anion correct',YTITLE='frequency [#]',PSYM=10

  key = ''
  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot distribution neutral
  h = HISTOGRAM(store.neutral, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction neutral correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot distribution cation
  h = HISTOGRAM(store.cation, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction cation correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot distribution small
  h = HISTOGRAM(store.small, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction small correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot distribution large
  h = HISTOGRAM(store.large, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction large correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot distribution nitrogen
  h = HISTOGRAM(store.nitrogen, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction nitrogen correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."
  
  ; plot distribution solo
  h = HISTOGRAM(store.solo, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction solo correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot distribution duo
  h = HISTOGRAM(store.duo, LOCATIONS=l, BINSIZE=0.05)

  PLOT,l,h,XTITLE='fraction duo correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  ; plot distribution trio
  h = HISTOGRAM(store.trio, LOCATIONS=l, BINSIZE=0.05, /NAN)

  PLOT,l,h,XTITLE='fraction trio correct',YTITLE='frequency [#]',PSYM=10

  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

END
