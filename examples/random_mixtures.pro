; docformat = 'rst'

;+
;
; This is an example of producing a number of random-mixture PAH
; emission spectrum, built around the functionality provided by the
; AmesPAHdbIDLSuite and should help confirm that the it has been
; properly installed.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: 
; `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Call the procedure directly::
;
;     IDL> random_mixtures
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
;     06-02-2023
;     Accommodate UIDs >9999. Christiaan Boersma.
;     05-09-2023
;     Fix progress bar, use HISTOGRAM instead of WHERE, prompt for next plot,
;     divide carbon and hydrogen sizes by nuids, and directly write to sample.
;     Christiaan Boersma.
;     07-06-2021
;     Cleaned up progress bar. Christiaan Boersma.
;     08-19-2019
;     Documentation added. Christiaan Boersma.
;-


;+
; Procedure creating a number of random-mixture spectra.
;
; :Categories:
;   Example
;-
PRO RANDOM_MIXTURES

  COMPILE_OPT IDL2

  ; test parameters
  n = 1000L

  binsize = 0.0001

  ; spectral parameters
  Ein = 6D * 1.6021765D-12

  FWHM = 15D

  gaussian = 0

  wrange = [2.5D, 20D]

  npoints = 300L

  ; retrieve spectra from the database
  pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  uids = pahdb->Search("mg=0 fe=0 si=0 o=0 c>20 chx=0 ch2=0", nuids)

  transitions = pahdb->getTransitionsByUID(uids)

  transitions->Cascade,Ein

  spectrum = transitions->Convolve(FWHM=FWHM, Gaussian=gaussian, XRange=1D4/REVERSE(wrange), NPoints=npoints)

  wavelength = 1D4 / spectrum->getGrid()

  spectra = spectrum->get()

  species = pahdb->getSpeciesByUID(uids)

  properties = species->get()

  OBJ_DESTROY,[species, spectrum, transitions, pahdb]

  ; run test
  samples = DBLARR(n, npoints)

  sizeC = DBLARR(n)

  sizeH = DBLARR(n)

  charge = DBLARR(5, n)

  h = HISTOGRAM(spectra.data.uid, MIN=0, REVERSE_INDICES=ri)

  FOR i = 0L, n - 1L DO BEGIN

     PRINT,FORMAT='("' + STRING(13B) + '",I5,$)',i+1L

     abundance =  RANDOMU(seed, nuids, /UNIFORM)

     FOR j = 0, nuids - 1 DO BEGIN

        select = ri[ri[uids[j]]:ri[uids[j]+1]-1]

        samples[i,*] += abundance[j] * spectra.data[select].intensity

        select = WHERE(properties.data.uid EQ uids[j])

        sizeC[i] += abundance[j] * properties.data[select].nC

        sizeH[i] += abundance[j] * properties.data[select].nH

        charge[properties.data[select].charge + 1, i] += abundance[j]

     ENDFOR

  ENDFOR

  matrix = DBLARR(n, n)

  FOR i = 0, n - 1 DO BEGIN

     FOR j = 0, i DO BEGIN

        IF i EQ j THEN BEGIN

           matrix[i,j] = 1D

           CONTINUE

        ENDIF

        matrix[i,j] = CORRELATE(REFORM(samples[i, *]), REFORM(samples[j, *]))

        matrix[j,i] = matrix[i,j]

     ENDFOR

  ENDFOR

  PRINT,FORMAT='("' + STRING(13B) + 'done!")'

  FOR i = 0, n - 1 DO samples[i, *] /= TOTAL(samples[i, *])

  moments = MOMENT(samples, DIMENSION=1, MAXMOMENT=2)

  h = HISTOGRAM(matrix, LOCATIONS=locations, BINSIZE=binsize)

  dummy = MAX(h, imax)

  select = WHERE(matrix NE 1, nselect)

  avg = MOMENT(matrix[select])

  med = MEDIAN(matrix[select])


  hC = HISTOGRAM(sizeC / nuids, LOCATIONS=locationsC, BINSIZE=0.15)

  dummy = MAX(hc, imaxC)

  avgC = MOMENT(sizeC / nuids)


  hH = HISTOGRAM(sizeH / nuids, LOCATIONS=locationsH, BINSIZE=0.1)

  dummy = MAX(hH, imaxH)

  avgH = MOMENT(sizeH / nuids)


  hCharge = HISTOGRAM(TOTAL(charge[2:*,*], 1) / TOTAL(charge, 1), LOCATIONS=locationsCharge, BINSIZE=0.005)

  dummy = MAX(hCharge, imaxCharge)

  avgCharge = MOMENT(TOTAL(charge[2:*,*], 1) / TOTAL(charge, 1))


  hDbC = HISTOGRAM(properties.data.nC, LOCATIONS=locationsDbC, BINSIZE=10)

  dummy = MAX(hDbC, imaxDbC)

  avgDbC = MOMENT(properties.data.nC)


  hDbH = HISTOGRAM(properties.data.nH, LOCATIONS=locationsDbH, BINSIZE=1)

  dummy = MAX(hDbH, imaxDbH)

  avgDbH = MOMENT(properties.data.nH)


  ; plot results
  PLOT,[0,1],[0,1],XRANGE=MINMAX(wavelength),/XSTYLE,YRANGE=MINMAX(samples),XTITLE='wavelength [micron]',YTITLE='normalized intensity [erg cm]',/NODATA

  FOR i = 0, n - 1 DO OPLOT,wavelength,samples[i,*],COLOR=14

  FOR i = 0, npoints - 1 DO OPLOT,wavelength[i]*[1,1],SQRT(moments[i,1])*[-1,1]+moments[i,0],COLOR=2,THICK=6

  OPLOT,wavelength,moments[*, 0],THICK=6,COLOR=2

  key = ''
  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  PLOT,locations,100*h/TOTAL(h),XTITLE='r [Pearson correlation coefficient]',YTITLE='probability [%]',PSYM=10

  OPLOT,locations[imax]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=4

  OPLOT,avg[0]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=2

  OPLOT,med*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=3

  OPLOT,avg[0]-0.5*SQRT(avg[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  OPLOT,avg[0]+0.5*SQRT(avg[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  ;OPLOT,avg[0]+0.5*SQRT(avg[1])*[-1,1],5.5*[1,1],LINESTYLE=5,COLOR=5


  XYOUTS,0.2,0.70,STRING(FORMAT='("peak:",X,F-6.4)', locations[imax]),/NORMAL,CHARSIZE=1.75,COLOR=4

  XYOUTS,0.2,0.65,STRING(FORMAT='("<r>:",X,F-6.4)', avg[0]),/NORMAL,CHARSIZE=1.75,COLOR=2

  XYOUTS,0.2,0.60,STRING(FORMAT='("median!Lr!N:",X,F-6.4)', med),/NORMAL,CHARSIZE=1.75,COLOR=3

  XYOUTS,0.2,0.55,STRING(FORMAT='("!Ms!X!Lr!N:",X,F-6.4)', SQRT(avg[1])),/NORMAL,CHARSIZE=1.75,COLOR=5

  XYOUTS,0.2,0.50,STRING(FORMAT='("binsize:",X,F-6.4)', binsize),/NORMAL,CHARSIZE=1.75

  XYOUTS,0.2,0.45,STRING(FORMAT='("n!Lsamples!N:",X,I-4)', n),/NORMAL,CHARSIZE=1.75

  XYOUTS,0.2,0.40,STRING(FORMAT='("fwhm:",X,F-5.2,X,"cm!U-1!N")', fwhm),/NORMAL,CHARSIZE=1.75

  XYOUTS,0.2,0.35,STRING(FORMAT='("E!Lin!N:",X,F-5.2,X,"eV")', Ein / 1.6021765D-12),/NORMAL,CHARSIZE=1.75

  XYOUTS,avg[0],2.5,STRING(FORMAT='(F6.4)',avg[0]),ALIGNMENT=0.5,COLOR=2,CHARSIZE=1

  XYOUTS,locations[imax],3.5,STRING(FORMAT='(F6.4)',locations[imax]),ALIGNMENT=0.5,COLOR=4,CHARSIZE=1

  XYOUTS,med,4.5,STRING(FORMAT='(F6.4)',med),ALIGNMENT=0.5,COLOR=3,CHARSIZE=1

  XYOUTS,avg[0],5.6,STRING(FORMAT='(F6.4)',SQRT(avg[1])),ALIGNMENT=0.5,COLOR=5,CHARSIZE=1


  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  PLOT,locationsC,100*hC/TOTAL(hC),XTITLE='<n!Lcarbon!N> [#]',YTITLE='probability [%]',PSYM=10

  OPLOT,locationsDbC,100*hDbC/TOTAL(hDbC),PSYM=10,LINESTYLE=5

  OPLOT,locationsC[imaxC]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=4

  OPLOT,avgC[0]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=2

  OPLOT,avgC[0]-SQRT(avgC[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  OPLOT,avgC[0]+SQRT(avgC[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  OPLOT,avgC[0]+SQRT(avgC[1])*[-1,1],5.5*[1,1],LINESTYLE=5,COLOR=5


  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  PLOT,locationsH,100*hH/TOTAL(hH),XTITLE='<n!Lhydrogen!N> [#]',YTITLE='probability [%]',PSYM=10

  OPLOT,locationsDbH,100*hDbC/TOTAL(hDbC),PSYM=10,LINESTYLE=5

  OPLOT,locationsH[imaxH]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=4

  OPLOT,avgH[0]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=2

  OPLOT,avgH[0]-SQRT(avgH[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  OPLOT,avgH[0]+SQRT(avgH[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  OPLOT,avgH[0]+SQRT(avgH[1])*[-1,1],10*[1,1],LINESTYLE=5,COLOR=5


  IF !D.NAME EQ 'X' THEN READ,key,PROMPT="Press <enter> to continue..."

  PLOT,locationsCharge,100*hCharge/TOTAL(hCharge),XTITLE='f!Lionized!N [ratio]',YTITLE='probability [%]',PSYM=10

  OPLOT,locationsCharge[imaxCharge]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=4

  OPLOT,avgCharge[0]*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=2

  OPLOT,avgCharge[0]-SQRT(avgCharge[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  OPLOT,avgCharge[0]+SQRT(avgCharge[1])*[1,1],!Y.CRANGE,LINESTYLE=5,COLOR=5

  OPLOT,avgCharge[0]+SQRT(avgCharge[1])*[-1,1],4*[1,1],LINESTYLE=5,COLOR=5

END
