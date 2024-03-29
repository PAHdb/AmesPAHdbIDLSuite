; docformat = 'rst'

;+
;
; This is an example of clustering (hierarchical) PAH emission spectra
; and should confirm that the AmesPAHdbIDLSuite has been correctly
; installed. The source code is annotated to guide users and
; developers in the inner workings of the suite.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> overlap_matrix
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
; Procedure performing a hierarchical split.
;
; :Categories:
;   Example
;
; :Params:
;   matrix: in, required, type=double array
;     n by n overlap matrix
;   depth: in, required, type=int
;     Current split
;   max_depth: in, required, type=int
;     Maximum number of splits
;   min_bins: in, required, type=int
;     Minimum number of members required to form a bin
;   bins: in, required, type=int array
;     Assigned cluster bin
;   parent: in, required, type=double array
;     Parent of current cluster
;
; :Keywords:
;   METRIC: in, optional, type=int
;     Whether to use a metric distance measure
;
; :Private:
;-
PRO NODES,matrix,depth,max_depth,min_bins,bins,parent,METRIC=metric

  COMPILE_OPT IDL2

  MESSAGE,STRING(FORMAT='("DEPTH:",X,I0)', depth),/INFORMATIONAL

  IF depth GT 0 THEN d = depth - 1 $
  ELSE d = depth

  k = WHERE(bins[*,d] EQ parent, nbin)

  IF nbin LT min_bins THEN BEGIN

     MESSAGE,STRING(FORMAT='(A0,X,"(NBIN=",I0,")")', "SINGULAR DECOMPOSITION OR SPARSE BIN: DONE", nbin),/INFORMATIONAL

     bins[k,depth:*] = parent

     RETURN

  ENDIF

  void = MIN(matrix, imin, /NAN, SUBSCRIPT_MAX=imax)

  bin1 = parent * 2 + 1 & bin2 = bin1 + 1

  IF NOT KEYWORD_SET(metric) THEN ij = ARRAY_INDICES(matrix, imin) $
  ELSE ij = ARRAY_INDICES(matrix, imax)

  srt = SORT(ij)

  ij = ij[srt]

  MESSAGE,STRING(FORMAT='("BINS:",X,I0,X,"&",X,I0)',bin1,bin2),/INFORMATIONAL

  bins[ij[0],depth] = bin1 & bins[ij[1],depth] = bin2

  m1 = matrix & m2 = matrix

  m1[ij[1],*] = !VALUES.D_NAN & m1[*,ij[1]] = !VALUES.D_NAN

  m2[ij[0],*] = !VALUES.D_NAN & m2[*,ij[0]] = !VALUES.D_NAN

  FOR j = 0, nbin - 1 DO BEGIN

     IF k[j] EQ ij[0] OR k[j] EQ ij[1] THEN CONTINUE

     IF NOT FINITE(matrix[ij[0], k[j]]) OR NOT FINITE(matrix[ij[1], k[j]]) THEN CONTINUE

     IF NOT KEYWORD_SET(metric) THEN BEGIN

        a = matrix[ij[0], k[j]] ; bin1

        b = matrix[ij[1], k[j]] ; bin2

        ; if a>b then add k[j] to bin1 - i.e., b<a

     ENDIF ELSE BEGIN

        b = matrix[ij[0], k[j]] ; bin1

        a = matrix[ij[1], k[j]] ; bin2

        ; if b<a then add k[j] to bin1 - i.e., a>b

     ENDELSE

     IF a GT b THEN BEGIN

        bins[k[j],depth] = bin1

        m2[k[j],*] = !VALUES.D_NAN

        m2[*,k[j]] = !VALUES.D_NAN

     ENDIF ELSE BEGIN

        bins[k[j],depth] = bin2

        m1[k[j],*] = !VALUES.D_NAN

        m1[*,k[j]] = !VALUES.D_NAN

     ENDELSE

  ENDFOR

  depth++

  IF depth EQ max_depth THEN BEGIN

     MESSAGE,'HIT MAX DEPTH: DONE',/INFORMATIONAL

     RETURN

  ENDIF

  d = depth

  NODES,m1,d,max_depth,min_bins,bins,bin1,METRIC=metric

  NODES,m2,depth,max_depth,min_bins,bins,bin2,METRIC=metric

END

;+
; Procedure performing a hierarchical clustering of PAH emission
; spectra based on their overlap (area).
; 
; :Keywords:
;   REBUILD: in, optional, type=int
;     Wheter to rebuild the distance matrix
;   METRIC: in, optional, type=int
;     Whether to use a metric distance measure
;
; :Categories:
;   Example
;-
PRO OVERLAP_MATRIX,REBUILD=rebuild,METRIC=metric

  COMPILE_OPT IDL2

  ; define path to save overlap matrix
  path = 'overlap_matrix.sav'

  ; define FWHM, frequency range, number of points
  Ein = 6D * 1.6021765D-12

  FWHM = 15D

  gaussian = 0

  xrange = [20D, 2.5D]

  npoints = 400L

  ; read in the default database defined by the environement variable
  ; !AMESPAHDEFAULTDB or the system variable AMESPAHDEFAULTDB. use the
  ; keyword FILENAME if these have not been set
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  ; limit ourselves to a certain set of PAHs
  uids = pahdb->Search("c>20 mg=0 fe=0 si=0 o=0 chx=0 ch2=0", nuids)

  ; retrieve all transitions for all species from the database
  transitions = pahdb->GetTransitionsByUID(uids)

  ; apply a PAH excitation/emission model
  transitions->Cascade,Ein

  ; convolve into spectrum
  spectrum = transitions->Convolve(FWHM=fwhm, $
                                   Xrange=1D4/xrange, $
                                   Gaussian=gaussian, $
                                   Npoints=npoints)

  ; obtain the spectrum for use outside object
  spectrum_s = spectrum->Get()

  ; clean up
  OBJ_DESTROY,[spectrum, transitions, pahdb]

  IF FILE_TEST(path, /READ) AND NOT KEYWORD_SET(REBUILD) THEN BEGIN

     MESSAGE,'READING MATRIX FROM ' + path,/INFORMATIONAL

     RESTORE,path

  ENDIF ELSE BEGIN

    IF KEYWORD_SET(REBUILD) AND FILE_TEST(path, /READ) THEN MESSAGE,'REBUILDING MATRIX',/INFORMATIONAL $
    ELSE MESSAGE,'BUILDING MATRIX',/INFORMATIONAL

    ; define matrix
    matrix = DBLARR(nuids, nuids)

    ; fill matrix
    FOR i = 0, nuids - 1 DO BEGIN

       FOR j = 0, i DO BEGIN

          IF i EQ j THEN BEGIN

             matrix[i,j] = 1D

             CONTINUE

          ENDIF

          sel1 = WHERE(spectrum_s.data.uid EQ uids[i])

          sel2 = WHERE(spectrum_s.data.uid EQ uids[j])

          IF KEYWORD_SET(METRIC) THEN matrix[i,j] = NORM(spectrum_s.data[sel2].intensity - spectrum_s.data[sel1].intensity, LNORM=2) $
          ELSE BEGIN

             upper = WHERE(spectrum_s.data[sel2].intensity GE spectrum_s.data[sel1].intensity, COMPLEMENT=lower)

             overlap = [spectrum_s.data[sel1[upper]].intensity, spectrum_s.data[sel2[lower]].intensity]

             srt = SORT([upper,lower])

             matrix[i,j] = INT_TABULATED(spectrum_s.grid, overlap[srt])

          ENDELSE

          matrix[j,i] = matrix[i,j]

       ENDFOR

    ENDFOR

    ; save overlap matrix to file
    SAVE,matrix,FILENAME=path

  ENDELSE

  m = matrix

  FOR i = 0, nuids - 1 DO m[i,i] = !VALUES.D_NAN

  depth = 0

  parent = 0

  max_depth = 3

  bins = INTARR(nuids, max_depth)

  min_bins = 10

  NODES,m,depth,max_depth,min_bins,bins,parent,METRIC=0

  min = 0

  key = ''

  FOR i = 0, max_depth - 1 DO BEGIN

     min += 2^i

     depth = bins[*, i]

     unq = UNIQ(depth, SORT(depth))

     u = depth[unq]

     nu = N_ELEMENTS(u)

     FOR j = 0, nu - 1 DO BEGIN

        IF u[j] LT min THEN CONTINUE

        sel1 = WHERE(depth EQ u[j], nsel1)

        uids_i = uids[sel1]

        sel2 = WHERE(spectrum_s.data.uid EQ uids_i[0])

        avg = spectrum_s.data[sel2].intensity

        FOR k = 1, nsel1 - 1 DO BEGIN

           sel2 = WHERE(spectrum_s.data.uid EQ uids_i[k])

           avg += spectrum_s.data[sel2].intensity

        ENDFOR

        avg /= DOUBLE(nsel1)

        PLOT,1D4/spectrum_s.grid,1D14*avg,XTITLE='wavelength [micron]',YTITLE='intensity [x10!U-14!N erg cm]',POSITION=[0.15,0.125,0.675,0.9]

        XYOUTS,0.18,0.85,STRING(FORMAT='("BIN #:",X,I0,X,"NBIN:",X,I0)', u[j], nsel1),/NORMAL

        IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

     ENDFOR

  ENDFOR

END
