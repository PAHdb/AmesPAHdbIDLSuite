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
;   test_nnls
;
FUNCTION DO_NNLS

  ; avoid underflow messages
  !EXCEPT = 0
 
  ; set number of spectra to co-add, the FWHM to be applied when
  ; convolving, the signal-to-noise ratio and spectral range
  nspectra = 20

  fwhm = 20D

  sn = 100D

  xrange = [20D, 2.5D]

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; get the properties of all the PAHs in the database
  species = pahdb->getSpeciesByUID( -1 )

  ; retrieve all unique identifiers
  uids = (species->Get()).data.uid

  ; count number of unique identifiers
  nuids = N_ELEMENTS(uids)

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
     
  ; get the transitions for the selected species
  transitions = pahdb->getTransitionsByUID(selected_uids)

  ; convolve the transitions in order to obtain a spectrum
  spectrum = transitions->Convolve(FWHM=fwhm, /Gaussian, XRange=1D4/xrange)

  ; create random weights
  selected_weights = REPLICATE({uid:0L, weight:0D}, nspectra)

  selected_weights.uid = selected_uids
  
  selected_weights.weight = RANDOMU(LONG(SYSTIME(1)), nspectra, /DOUBLE)

  ; co-add the spectra using the random weights
  coadd = spectrum->Coadd(Weights=selected_weights)

  ; obtain the spectrum for use outside the object
  coadd_s = coadd->Get()

  ; add some Gaussian noise
  noise = MAX(coadd_s.data.intensity) * RANDOMN(seed, N_ELEMENTS(coadd_s.data)) / sn

  coadd_s.data.intensity += noise

  ; clean up
  OBJ_DESTROY,[spectrum, transitions, species]

  ; retrieve all transitions from the database
  transitions = pahdb->getTransitionsByUID( -1 )

  ; convolve the transitions in order to obtain a spectrum
  spectrum = transitions->Convolve(FWHM=fwhm, /Gaussian, XRange=1D4/xrange)

  ; fit the spectrum
  fit = spectrum->Fit(coadd_s.data.intensity)

  ; obtain the unique identifiers of the fitted species
  found_uids = fit->getUIDS()

  ; obtain the fitted weights
  found_weights = fit->getWeights()

  ; clean up
  OBJ_DESTROY,[fit, coadd, spectrum, transitions, pahdb]

  ; concat selected and found unique identifiers
  all_uids = [selected_uids, found_uids]

  ; sort all unique identifiers
  srt = SORT(all_uids)

  all_uids = all_uids[srt]

  ; keep only one unique identifier
  all_uids = all_uids[UNIQ(all_uids)]

  ; count all unique identifiers
  nall_uids = N_ELEMENTS(all_uids)

  ; output header
  PRINT,FORMAT='(A10,4X,A10)',"UID","weight"

  PRINT,FORMAT='(A0,X,A0,X,A0,X,A0)',"selected","found","selected","found"

  ; compare selected with found
  FOR i = 0, nall_uids - 1 DO BEGIN

     select = WHERE(selected_uids EQ all_uids[i], count)

     IF count EQ 0 THEN BEGIN

        selected_uid = ''

        selected_weight = ''

     ENDIF ELSE BEGIN

        selected_uid = STRING(FORMAT='(I3)', selected_uids[select])

        select = WHERE(selected_weights.uid EQ all_uids[i])
        
        selected_weight = STRING(FORMAT='(G7.2)', selected_weights[select].weight)

     ENDELSE

     select = WHERE(found_uids EQ all_uids[i], count)

     IF count EQ 0 THEN BEGIN

        found_uid = ''
    
        found_weight = ''

     ENDIF ELSE BEGIN

        found_uid = STRING(FORMAT='(I3)', found_uids[select])

        select = WHERE(found_weights.uid EQ all_uids[i])
  
        found_weight = STRING(FORMAT='(G7.2)', found_weights[select].weight)

     ENDELSE

     PRINT,FORMAT='(A3,4X,A3,4X,A7,4X,A7)',selected_uid,found_uid,selected_weight,found_weight

  ENDFOR

  points = {x:0D, y:0D} 

  FOR i = 0, nspectra - 1 DO BEGIN

     select = WHERE(selected_uids[i] EQ found_uids , nselect)

     IF nselect EQ 0 THEN found_weight = 0D $
     ELSE BEGIN

        select = WHERE(selected_uids[i] EQ found_weights.uid)
        
        found_weight = found_weights[select].weight

     ENDELSE
     
     points = [points, {x:selected_weights[i].weight, y:found_weight}]

  ENDFOR

  RETURN,points[1:*]

END

PRO TEST_NNLS

  points = {x:0D, y:0D}

  FOR i = 0, 5 DO points = [points, DO_NNLS()]

  points = points[1:*]

  PLOT,[0,MAX(points.x)],[0,MAX(points.y)],XTITLE='input weights',YTITLE='output weights',/NODATA

  OPLOT,[0,1],[0,1],LINESTYLE=5,COLOR=2

  OPLOT,points.x,points.y,PSYM=1

END
