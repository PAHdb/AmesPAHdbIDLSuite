; NASA Ames PAH IR Spectroscopic Database
;
; This is an example of applying principal component analysis to the
; spectra in the database built around the functionality provided by
; the Suite and should help confirm that the AmesPAHdbIDLSuite has
; been properly installed.
; 
; Additional information can be found at
; http://www.astrochem.org/pahdb, in Bauschlicher et al. 2010, The
; Astrophysical Journal Supplement Series, 189, 341 and in Boersma et
; al. 2014, The Astrophysical Journal Supplement Series, 211, 8.
;
; USAGE
;   test_svdc
;
PRO TEST_SVDC

  ; spectral parameters
  Ein = 6D * 1.6021765D-12

  FWHM = 20D

  gaussian = 0

  wrange = [2.5D, 20D]

  npoints = 400L

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use
  ; the keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  uids = pahdb->Search("c>20 ch2=0 chx=0 mg=0 fe=0 si=0 o=0", nuids)

  transitions = pahdb->getTransitionsByUID(uids)

  transitions->Cascade,Ein

  spectrum = transitions->Convolve(FWHM=FWHM, Gaussian=gaussian, XRange=1D4/REVERSE(wrange), NPoints=npoints)

  frequency = spectrum->getGrid()
  
  spectra = spectrum->get()

  OBJ_DESTROY,[spectrum, transitions, pahdb]

  select = WHERE(spectra.data.uid EQ uids[0])

  wavelength = 1D4/frequency

  ; do singular value decomposition
  matrix = DBLARR(nuids, npoints)

  FOR i = 0L, nuids - 1 DO BEGIN

     select = WHERE(spectra.data.uid EQ uids[i])

     matrix[i, *] = spectra.data[select].intensity

  ENDFOR

  SVDC,matrix,w,u,v,/DOUBLE

  PLOT,[0,1],[0,1],XRANGE=MINMAX(wavelength),/XSTYLE,YRANGE=MINMAX(ABS(u[0,*])),XTITLE='wavelength [micron]',YTITLE='-intensity [x10!U??!N erg cm!U-1!N]',/NODATA

  OPLOT,wavelength,ABS(u[0,*]),COLOR=2

  XYOUTS,0.2,0.85,STRING(FORMAT='(F4.1,"%")', 100E*w[0]/TOTAL(w)),COLOR=2,/NORMAL

  key = ''
  read,key,prompt="Press <enter> to continue..."
  
  srt = REVERSE(SORT(w))
  
  PLOT,w[srt],XTITLE="Rank of ordered eigenvalues",YTITLE='Eigenvalues',/NODATA,/YLOG

  OPLOT,w[srt],COLOR=4

  OPLOT,w[srt],COLOR=2,PSYM=1
  
END
