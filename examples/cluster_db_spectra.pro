; docformat = 'rst'

;+
;
; This is an example of clustering (kmeans) PAH absorption spectra,
; built around the functionality provided by the AmesPAHdbIDLSuite and
; should help confirm that the it has been properly installed. The
; source code is annotated to guide users and developers in the inner
; workings of the suite.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> cluster_db_spectra
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
; Procedure performing the clustering analyzes.
;
; :Categories:
;   Example
;-
PRO CLUSTER_DB_SPECTRA

  ; define FWHM, frequency range, number of points and number of clusters to use
  fwhm = 20D

  xrange = 1D4 / [15, 2.5D]

  npoints = 301L

  nclusters = 2

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use the
  ; keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; retrieve all transitions for all species from the database
  transitions = pahdb->getTransitionsByUID(-1)

  uids = pahdb->Search("c>20", nuids)

  ; convolve spectra
  spectrum = transitions->Convolve(FWHM=fwhm, Xrange=xrange, Npoints=npoints)

  ; obtain the spectrum for use outside object
  spectra = spectrum->Get()

  ; set all integrated intensities to unity
  FOR i = 0, nuids - 1 DO BEGIN

     sel = WHERE(spectra.data.uid EQ uids[i])

     spectra.data[sel].intensity /=  INT_TABULATED(spectra.grid, spectra.data[sel].intensity)

  ENDFOR

  ; clean up objects
  OBJ_DESTROY,[transitions, spectrum, pahdb]

  ; define matrix
  matrix = DBLARR(npoints, nuids, /NOZERO)

  ; fill matrix
  FOR i = 0, nuids - 1 DO BEGIN

     sel = WHERE(spectra.data.uid EQ uids[i])

     matrix[*,i] = spectra.data[sel].intensity

  ENDFOR

  ; perform k-means cluster analysis
  means = CLUST_WTS(matrix, $
                    N_CLUSTERS=nclusters, $
                    /DOUBLE, $
                    N_ITERATIONS=128)

  ; obtain cluster members
  clusters = CLUSTER(matrix, means, N_CLUSTERS=nclusters, /DOUBLE)

  ; plot means
  PLOT,spectra.grid,[0,1],YRANGE=MINMAX(means),XTITLE='frequency [cm!U-1!N]',YTITLE='normalized intensity [km/mol]',/NODATA

  FOR i = 0, nclusters - 1 DO OPLOT,spectra.grid,means[*,i],COLOR=i+2

END
