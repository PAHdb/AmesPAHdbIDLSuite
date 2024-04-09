; docformat = 'rst'

;+
;
; This is an example of applying principal component analysis to the
; spectra in the database, built around the functionality provided by
; the AmesPAHdbIDLSuite and should help confirm that the it has been
; properly installed.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> test_pcomp
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
; Procedure for testing a principal component analysis on a number of
; generated PAH emission spectra.
;
; :Categories:
;   Example
;-
PRO TEST_PCOMP

  COMPILE_OPT IDL2

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

  ; do principal component analysis
  matrix = DBLARR(nuids, npoints)

  FOR i = 0L, nuids - 1 DO BEGIN

     select = WHERE(spectra.data.uid EQ uids[i])

     matrix[i, *] = 1D14 * spectra.data[select].intensity

  ENDFOR

  pc = PCOMP(matrix, /COVARIANCE, EIGENVALUES=eigenval, /DOUBLE)

  cum = 100E * TOTAL(eigenval, /CUMULATIVE) / TOTAL(eigenval)

  PLOT,[0,1],[0,1],XRANGE=MINMAX(wavelength),/XSTYLE,YRANGE=MINMAX(pc),XTITLE='wavelength [micron]',YTITLE='intensity [x10!U??!N erg cm!U-1!N]',/NODATA

  FOR i = 0, nuids - 1 DO BEGIN

     OPLOT,wavelength,pc[i,*],COLOR=i+2

     XYOUTS,0.2,0.85 - FLOAT(i) * 0.05,STRING(FORMAT='(F4.1,"%")', 100E*eigenval[i]/TOTAL(eigenval)),COLOR=i+2,/NORMAL

     IF cum[i] GE 60E THEN BREAK

  ENDFOR

  key = ''

  read,key,prompt="Press <enter> to continue..."

  PLOT,eigenval,XTITLE="Rank of ordered eigenvalues",YTITLE='Eigenvalues',/NODATA,/YLOG

  OPLOT,eigenval,COLOR=4

  OPLOT,eigenval,COLOR=2,PSYM=1

  read,key,prompt="Press <enter> to continue..."

  PLOT,cum,XTITLE="Rank of ordered eigenvalues",YTITLE='Explains [%]',YSTYLE=16,/NODATA

  OPLOT,cum,COLOR=4

  OPLOT,cum,COLOR=2,PSYM=1

  save,filename='~/Desktop/pcom.sav',wavelength,pc

END
