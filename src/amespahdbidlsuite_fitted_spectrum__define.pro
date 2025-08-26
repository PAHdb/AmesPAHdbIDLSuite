; docformat = 'rst'

;+
;
; Class to manage a fitted spectrum.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create and destroy an
;   AmesPAHdbIDLSuite_Fitted_Spectrum-instance::
;
;     IDL> fitted = OBJ_NEW('AmesPAHdbIDLSuite_Fitted_Spectrum')
;     IDL> fitted->Set,data
;     IDL> fitted->Plot
;     IDL> OBJ_DESTORY,fitted
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
;     11-27-2024
;     Medium/large PAH size cutoff set to 70 to align with Python counterpart.
;     Christiaan Boersma.
;     09-19-2024
;     Use trapezium integration in favor of INT_TABULATED. Christiaan Boersma.
;     09-10-2024
;     Added GETAVERAGENUMBEROFCARBONATOMS. Christiaan Boersma.
;     12-13-2023
;     Align TOLARANCE AND ITERATIONS in DESCRIPTION. Christiaan Boersma.
;     11-22-2023
;     Add tolerance and iterations properties and their handling in SET, GET,
;     and DESCRIPTION and add GETTOLERANCE and GETITERATIONs. Christiaan
;     Boersma.
;     11-17-2023
;     Add MEDIUM size bin to GETBREAKDOWN, GETCLASSES, and PLOT. Christiaan
;     Boersma.
;     10-04-2023
;     Allow setting the SMALL threshold in PLOT through keyword-inheritance.
;     Christiaan Boersma.
;     06-02-2023
;     Accommodate UIDs >9999 in PLOT and use Courier font when presenting
;     contributing UIDs. Christiaan Boersma.
;     11-23-2022
;     Add missing OPLOT keyword to PLOT signature. Christiaan
;     Boersma.
;     06-14-2022
;     Use lazy instantiation for getters and keep track of function
;     signatures. Christiaan Boersma.
;     05-03-2022
;     Align integration ranges with Python code in GETERROR.
;     Christiaan Boersma
;     04-28-2022
;     Return piecewise errors in GETERROR and handle piecewise.
;     errors in DESCRIPTION. Christiaan Boersma.
;     04-27-2022
;     Refactored plotting of residual in PLOT and corrected logic
;     flow in SET. Christiaan Boersma.
;     06-23-2021
;     Get sorting right for GETERROR, added error from GETERROR
;     to DESCRIPTION, and call Plot::Init. Christiaan Boersma.
;     04-30-2021
;     Added GETERROR. Christiaan Boersma.
;     03-31-2021
;     Intersect now calls Spectrum::Intersect instead of
;     Data::Intersect. Christiaan Boersma.
;     02-17-2021
;     Overload Intersect to ensure weights are also
;     intersected. Christiaan Boersma.
;     09-04-2020
;     Remove extra comma labels when plotting structures PLOT.
;     Christiaan Boersma.
;     02-10-2020
;     Make sure to remove continuum when calculating norm and
;     chi-squared. Christiaan Boersma.
;     01-25-2019
;     Now writing files as IPAC-tables. Christiaan Boersma.
;     01-23-2017
;     Handle UIDs larger than 999 in PLOT. Christiaan Boersma.
;     04-26-2016
;     Updated GETSIZEDISTRIBUTION to use Rice Rule for determining the
;     number of bins. Christiaan Boersma.
;     04-11-2016
;     Added method GETSIZEDISTRIBUTION. Added SIZEDISTRIBUTION-keyword
;     to PLOT. Christiaan Boersma.
;     03-29-2016
;     Fixed dot-notation for class access in DESCRIPTION. Christiaan Boersma.
;     07-12-2015
;     Added GETMETHOD and its infrastructure. Christiaan Boersma.
;     05-20-2015
;     Fixed missing Struct in SET. Christiaan Boersma.
;     05-03-2015
;     Updated formatting for DESCRIPTION. Christiaan Boersma.
;     04-24-2015
;     Refactored GETRESIDUAL and GETBREAKDOWN to use GETFIT and other
;     small fixes. Christiaan Boersma.
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;-

;+
;  Output spectrum description.
;
;  :Params:
;    Str: out, optional, type="string array"
;      Ouput to Str
;
; :Categories:
;   INFORMATIVE
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Spectrum::Description,Str

  err = self->getError()

  Str = [Str, $
         STRING(FORMAT='(A-12,":",X,A0)', "fit", self->getMethod()), $
         STRING(FORMAT='(A-12,":",X,g-8.3)', "|_tolerance", self->getTolerance()), $
         STRING(FORMAT='(A-12,":",X,I0)', "|_iterations", self->getIterations()), $
         STRING(FORMAT='(A-12,":",X,g-8.3)', "|_norm", self->getNorm()), $
         STRING(FORMAT='(A-12,":",X,g-8.3)', "|_chisquared", self->getChiSquared()), $
         STRING(FORMAT='(A-12,":",X,g-8.3)', "|_error", err.(0))]

  tags = STRLOWCASE(TAG_NAMES(err))

  ntags = N_TAGS(err)
  FOR i = 1L, ntags - 1L DO $
    IF err.(i) GE 0.0D THEN $
      Str = [Str, $
             STRING(FORMAT='(A-12,":",X,g-8.3)', "|_" + tags[i], err.(i))]

  Str = STRJOIN(Str, "!C")

  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

;+
;  Plot the fitted spectrum.
;
;  :Keywords:
;    DistributionSize: in, optional, type=int
;      Toggle to plot the PAH size distribution
;    Residual: in, optional, type=int
;      Toggle to plot the residuals of the fit
;    Size: in, optional, type=int
;      Toggle to plot the size breakdown of the fit
;    Charge: in, optional, type=int
;      Toggle to plot the charge breakdown of the fit
;    Composition: in, optional, type=int
;      Toggle to plot the compositional breakdown of the fit
;    Structures: in, optional, type=int
;      Toggle to plot the chemical structures of the PAHs contributing
;      to the fit
;    Wavelength: in, optional, type=int
;      Whether to set the abscissa units to wavelength
;    Stick: in, optional, type=int
;      Whether to plot the spectrum as sticks
;    Fill: in, optional, type=int
;       Whether to solid-fill the spectrum
;    Legend: in, optional, type=int
;      Whether to show a legend
;    Color: in, optional, type=int
;      Color to plot the spectrum with
;    _EXTRA: in, optional, type=struct
;      Required for IDL's keyword-inheritance mechanism
;
; :Categories:
;   PLOTTING
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Plot,DistributionSize=DistributionSize,Residual=Residual,Size=Size,Charge=Charge,Composition=Composition,Structures=Structures,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Legend=Legend,Color=Color,Oplot=Oplot,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF KEYWORD_SET(Structures) THEN BEGIN

     TVLCT,ch1,ch2,ch3,/GET

     LOADCT,0,/SILENT

     idx = ULINDGEN((*self.database).sizes.ngeometries, self.nuids)

     sel =  WHERE((*self.database).data.geometries[idx MOD (*self.database).sizes.ngeometries].uid EQ (*self.uids)[idx / (*self.database).sizes.ngeometries]) MOD (*self.database).sizes.ngeometries

     g = OBJ_NEW('AmesPAHdbIDLSuite_Geometry', Data=(*self.database).data.geometries[sel], $
                                               UIDs=*self.uids)

     g->Diagonalize

     idx = ULINDGEN((*self.database).sizes.nspecies, self.nuids)

     sel = WHERE((*self.database).data.species[idx MOD (*self.database).sizes.nspecies].uid EQ (*self.uids)[idx / (*self.database).sizes.nspecies]) MOD (*self.database).sizes.nspecies

     s = OBJ_NEW('AmesPAHdbIDLSuite_Species', Data=(*self.database).data.species[sel], $
                                              UIDs=*self.uids)

     s->formatFormulae

     f = (s->get()).data.formula

     OBJ_DESTROY,[s]

     a_perc = 100D * (*self.weights).weight / TOTAL((*self.weights).weight)

     f_perc = DBLARR(self.nuids, /NOZERO)

     dx = (*self.grid)[1:*] - *self.grid

     FOR i = 0L, self.nuids - 1 DO BEGIN

        sel = WHERE((*self.data).uid EQ (*self.uids)[i])

        intensity = (*self.data)[sel].intensity

        yavg = (intensity[0:-2] + intensity[1:*]) / 2D

        f_perc[i] = TOTAL(dx * yavg)
     ENDFOR

     f_perc *= 100D / TOTAL(f_perc)

     PLOT,[0,1],[0,1],XRANGE=[0,1],YRANGE=[0,1],XSTYLE=5,YSTYLE=5,/NODATA,POSITION=[0,0,1,1]

     IF !D.NAME EQ 'X' THEN BEGIN

        resolution = 128 * [1, 1]

        nx = !D.X_SIZE / resolution[0]
     ENDIF ELSE BEGIN

        resolution = 300 * [1, 1]

        nx = FLOOR(!D.X_SIZE / (!D.X_PX_CM * 2.54))
     ENDELSE

     FOR i = 0, self.nuids - 1 DO BEGIN

        img = g->Structure((*self.uids)[i], Background=[255, 255, 255], Resolution=resolution)

        IF !D.NAME EQ 'X' THEN BEGIN

           x = resolution[0] * (i MOD nx) & y = !D.Y_SIZE - resolution[1] * (1 + (i / nx))

           TV,img,x,y,/DEVICE,/TRUE

           XYOUTS,x+resolution/2,y+0.15*resolution[1],STRING(FORMAT='(A0,X,"(a:",F4.1,"%;f:",F4.1,"%)")',f[i],a_perc[i], f_perc[i]),ALIGNMENT=0.5,/DEVICE
        ENDIF ELSE BEGIN

           x = !D.X_PX_CM * 2.54 * (i MOD nx) & y = !D.Y_SIZE - (!D.Y_PX_CM * 2.54 * (1 + (i / nx)))

           TV,img,x,y,XSIZE=!D.X_PX_CM*2.54,YSIZE=!D.Y_PX_CM*2.54,/TRUE,/DEVICE

           XYOUTS,x+1.27*!D.X_PX_CM,y+!D.Y_PX_CM*0.38,STRING(FORMAT='(A0,"(a:",F4.1,"%;f:",F4.1,"%)")',f[i],a_perc[i],f_perc[i]),ALIGNMENT=0.5,/DEVICE,CHARSIZE=0.5
        ENDELSE
     ENDFOR

     OBJ_DESTROY,[g]

     TVLCT,ch1,ch2,ch3

     RETURN
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400

  IF KEYWORD_SET(DistributionSize) THEN GOTO,SIZEDISTRIBUTION

  x = *self.grid

  nx = N_ELEMENTS(x)

  xunits = self.units.abscissa.str

  xrange = [MAX(x, MIN=xmin), xmin]

  obs_xstdev = (*self.observation).data.xstdev

  IF KEYWORD_SET(Wavelength) THEN BEGIN

     obs_xstdev = 1D4 * obs_xstdev / x^2

     x = 1D4 / x

     xrange = [MIN(x, MAX=xmax), xmax]

     xunits = 'wavelength [!Mm!Xm]'
  ENDIF

  yunits = (*self.observation).units.ordinate.str

  IF yunits NE '' THEN yunits += '!C'

  yunits += self.units.ordinate.str

  fit = self->getFit()

  IF NOT KEYWORD_SET(Oplot) THEN BEGIN

     IF KEYWORD_SET(Residual) THEN self->AmesPAHdbIDLSuite_Plot::Plot,[0,1],[0,1],COLOR=Color,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN([(*self.observation).data.y - (*self.observation).data.ystdev, (*self.observation).data.continuum + fit]), MAX([(*self.observation).data.y + (*self.observation).data.ystdev, (*self.observation).data.continuum + fit])],XTICKFORMAT='(A1)',YTITLE=yunits,POSITION=[0.2,0.45,0.95,0.9],/NoData,_EXTRA=EXTRA $
     ELSE IF KEYWORD_SET(Size) OR KEYWORD_SET(Charge) OR KEYWORD_SET(COMPOSITION) THEN self->AmesPAHdbIDLSuite_Plot::Plot,[0,1],[0,1],COLOR=Color,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN([(*self.observation).data.y - (*self.observation).data.ystdev, (*self.observation).data.continuum + fit]), MAX([(*self.observation).data.y + (*self.observation).data.ystdev, (*self.observation).data.continuum + fit])],XTITLE=xunits,YTITLE=yunits,POSITION=[0.2,0.2,0.95,0.9],/NoData,_EXTRA=EXTRA $
     ELSE self->AmesPAHdbIDLSuite_Plot::Plot,[0,1],[0,1],COLOR=Color,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN([(*self.observation).data.y - (*self.observation).data.ystdev, (*self.observation).data.continuum + fit]), MAX([(*self.observation).data.y + (*self.observation).data.ystdev, (*self.observation).data.continuum + fit])],XTITLE=xunits,YTITLE=yunits,POSITION=[0.4,0.2,0.95,0.9],/NoData,_EXTRA=EXTRA
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::OplotError,x,(*self.observation).data.y,(*self.observation).data.ystdev,obs_xstdev,COLOR=14

  self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.y,LINESTYLE=5,COLOR=14

  self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum,LINESTYLE=2

  self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+fit

  IF NOT KEYWORD_SET(Color) THEN Color = 2

  IF KEYWORD_SET(Size) OR KEYWORD_SET(Charge) OR KEYWORD_SET(COMPOSITION) THEN BEGIN

     classes = self->getClasses(_EXTRA=EXTRA)

     IF KEYWORD_SET(Size) THEN BEGIN

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.small,Stick=Stick,Fill=Fill,COLOR=Color

        XYOUTS,0.25,0.75,'small',COLOR=Color,/Normal

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.medium,Stick=Stick,Fill=Fill,COLOR=Color+1

        XYOUTS,0.25,0.70,'medium',COLOR=Color+1,/Normal

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.large,Stick=Stick,Fill=Fill,COLOR=Color+2

        XYOUTS,0.25,0.65,'large',COLOR=Color+2,/NORMAL
     ENDIF ELSE IF KEYWORD_SET(Charge) THEN BEGIN

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.anion,Stick=Stick,Fill=Fill,COLOR=Color

        XYOUTS,0.25,0.75,'anion',COLOR=Color,/Normal

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.neutral,Stick=Stick,Fill=Fill,COLOR=Color+1

        XYOUTS,0.25,0.70,'neutral',COLOR=Color+1,/Normal

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.cation,Stick=Stick,Fill=Fill,COLOR=Color+2

        XYOUTS,0.25,0.65,'cation',COLOR=Color+2,/Normal
     ENDIF ELSE BEGIN

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.pure,Stick=Stick,Fill=Fill,COLOR=Color

        XYOUTS,0.25,0.75,'pure',COLOR=Color,/Normal

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.nitrogen,Stick=Stick,Fill=Fill,COLOR=Color+1

        XYOUTS,0.25,0.70,'nitrogen',COLOR=Color+1,/Normal
     ENDELSE
  ENDIF ELSE BEGIN

     FOR i = 0L, self.nuids - 1 DO BEGIN

        select = WHERE((*self.data).uid EQ (*self.uids)[i])

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+(*self.data)[select].intensity,Stick=Stick,Fill=Fill,COLOR=Color+i
     ENDFOR
  ENDELSE

  IF SIZE(Legend, /TYPE) EQ 0 THEN Legend = 1

  IF Legend THEN BEGIN

     self->Description,outs

     self->AmesPAHdbIDLSuite_Plot::Legend,outs
  ENDIF

  IF KEYWORD_SET(Residual) THEN BEGIN

     y = -100D * self->getResidual() / (*self.observation).data.y

     ystdev = 100 * (*self.observation).data.ystdev / (*self.observation).data.y

     self->AmesPAHdbIDLSuite_Plot::Plot,x,y,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN(y - ystdev), MAX(y + ystdev)],XTITLE=xunits,YTITLE='residual!C[%]',POSITION=[0.2,0.2,0.95,0.45],/NoData,/NOERASE,_EXTRA=EXTRA

     self->AmesPAHdbIDLSuite_Plot::OplotError,x,0*x,ystdev,obs_xstdev,Color=14

     self->AmesPAHdbIDLSuite_Plot::Oplot,!X.CRANGE,[0,0],Color=14,LINESTYLE=5

     self->AmesPAHdbIDLSuite_Plot::Oplot,x,y,Stick=Stick,Fill=Fill,COLOR=Color
  ENDIF ELSE IF NOT (KEYWORD_SET(Size) OR KEYWORD_SET(Charge) OR KEYWORD_SET(COMPOSITION)) THEN BEGIN

     PLOT,[0,1],[0,1],/NODATA,POSITION=[0.05,0.2,0.25,0.9],XTICKS=1,YTICKS=1,XMINOR=1,YMINOR=1,XTICKFORMAT='(A1)',YTICKFORMAT='(A1)',/NOERASE

     !P.CHARSIZE = 1.25

     XYOUTS,0.055,0.87,STRING(FORMAT='("!11",A-5,4X,A-8,"!3")', 'UID', 'WEIGHT'),/NORMAL

     i = 0

     ypos = 0.87 - 0.03

     WHILE i LT self.nuids AND ypos GT 0.225 DO BEGIN

        XYOUTS,0.055,ypos,STRING(FORMAT='("!11",I5,4X,g-8.3,"!3")', (*self.weights)[i].uid, (*self.weights)[i].weight),COLOR=Color+i++,/NORMAL

        ypos -= 0.025
     ENDWHILE

     IF i NE self.nuids THEN XYOUTS,0.055,ypos,STRING(FORMAT='("!11+",I0,X,"more...!3")', self.nuids - i),/NORMAL
  ENDIF

  GOTO,FINISH

SIZEDISTRIBUTION:

  sd = self->getSizeDistribution()

  sd.distribution = 100 * sd.distribution / TOTAL(sd.distribution)

  nbins = N_ELEMENTS(sd.size) - 1

  PLOT,[0,1],[0,1],XRANGE=[sd.size[0], sd.size[nbins]],YRANGE=[0, MAX(sd.distribution)],XTITLE='n!LCarbon!N',YTITLE='frequency [%]',POSITION=[0.2,0.2,0.95,0.9],/NODATA

  PLOTS,[sd.size[0],sd.size[0],sd.size[1]],[0,sd.distribution[0],sd.distribution[0]],COLOR=2,THICK=2

  FOR i = 1, nbins - 1 DO PLOTS,[sd.size[i], sd.size[i], sd.size[i+1]],[sd.distribution[i-1],sd.distribution[i],sd.distribution[i]],COLOR=2,THICK=2

  PLOTS,[1,1]*sd.size[nbins],[0,sd.distribution[nbins-1]],COLOR=2,THICK=2

FINISH:

  self->AmesPAHdbIDLSuite_Plot::Restore
END

;+
; Write the fitted spectrum to file as an IPAC-table.
;
; :Params:
;   Filename: in, optional, type=string
;     Output filename
;
; :Categories:
;   OUTPUT
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Write,Filename

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() LT 1 THEN Filename = OBJ_CLASS(self) + '.tbl'

  timestamp = SYSTIME()
  hdr = []
  FXADDPAR,hdr,"DATE",timestamp," Date this file was generated"
  FXADDPAR,hdr,"ORIGIN","NASA Ames Research Center"," Organization generating this file"
  FXADDPAR,hdr,"CREATOR",STRING(FORMAT='("IDL",X,A0,X,"on",X,A0)', !VERSION.RELEASE, !VERSION.OS_NAME)," Software used to create this file"
  FXADDPAR,hdr,"SOFTWARE","AmesPAHdbIDLSuite"," Program used to create this file"
  FXADDPAR,hdr,"AUTHOR","Dr. C. Boersma"," Author of the program"
  FXADDPAR,hdr,"TYPE",OBJ_CLASS(self)," AmesPAHdbIDLSuite data type"

  self->Description,description

  comments = STRSPLIT(description, "!C", /EXTRACT, /REGEX, COUNT=ncomments)

  FOR i = 0L, ncomments - 1 DO FXADDPAR,hdr,"COMMENT",comments[i]

  abscissa = STREGEX(self.units.abscissa.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT)

  ordinate = STREGEX(self.units.ordinate.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT)

  half_abscissa_len = STRLEN(abscissa[1]) / 2
  half_ordinate_len = STRLEN(ordinate[1]) / 2

  fmt1 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  fmt2 = '("|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  half_abscissa_len = STRLEN(abscissa[2]) / 2
  half_ordinate_len = STRLEN(ordinate[2]) / 2

  fmt3 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  cols = [STRING(FORMAT=fmt1,STRUPCASE(abscissa[1]),STRUPCASE(ordinate[1]),'WEIGHT','UID'), $
          STRING(FORMAT=fmt2,"double","double","double","int"), $
          STRING(FORMAT=fmt3,abscissa[2],ordinate[2],"","")]

  n = N_ELEMENTS(*self.grid)
  intensities = REFORM((*self.data).intensity, n, self.nuids)
  srt = SORT(*self.uids)
  intensities = intensities[*, srt]

  OPENW,funit,Filename,/GET_LUN
  PRINTF,funit,FORMAT='("\",A0)',hdr[0:WHERE(STRPOS(hdr, 'END') EQ 0)]
  PRINTF,funit,STRJOIN(cols, STRING( 10B ))
  n = N_ELEMENTS(*self.grid)

  FOR i = 0L, self.nuids - 1L DO BEGIN
     FOR j = 0L, n - 1L DO PRINTF,funit,FORMAT='(X,F25.6,X,F25.6,X,F25.6,X,I)',(*self.grid)[j],intensities[j,i],(*self.weights)[i].weight,(*self.uids)[i]
  ENDFOR
  CLOSE,funit
  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"    WRITTEN IPAC TABLE: ", Filename
  PRINT,"========================================================="
  PRINT
END

;+
;  Sorts the UIDs by their contribution to the fit.
;
; :Params:
;   Weights: out, optional, type="struct array"
;     Return the sorted weights
;
; :Keywords:
;   Flux: in, optional, type=int
;     Whether to sort by flux instead of abundance.
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Sort,Weights,Flux=Flux

  COMPILE_OPT IDL2

  ON_ERROR,2

  Weights = REPLICATE({AmesPAHdbIDLSuite_Weights_S, uid:0L, weight:0D}, self.nuids)

  IF NOT KEYWORD_SET(Flux) THEN Weights.weight = (*self.weights).weight $
  ELSE BEGIN

     dx = (*self.grid)[1:*] - *self.grid

     FOR i = 0L, self.nuids - 1 DO BEGIN

        select = WHERE((*self.data).uid EQ (*self.uids)[i])

        intensity = (*self.data)[select].intensity

        yavg = (intensity[0:-2] + intensity[1:*]) / 2.0

        Weights[i].weight = TOTAL(dx * yavg)
     ENDFOR
  ENDELSE

  Weights.uid = *self.uids

  srt = REVERSE(SORT(Weights.weight))

  Weights = weights[srt]

  *self.weights = (*self.weights)[srt]

  *self.uids = (*self.uids)[srt]
END

;+
; Retrieves the AmesPAHdbIDLSuite_Fitted_Spectrum representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  struct = self->AmesPAHdbIDLSuite_Spectrum::Get()

  struct.type = OBJ_CLASS(self)+'_S'

  RETURN,CREATE_STRUCT(struct, 'observation', *self.observation, 'weights', *self.weights, 'norm', self->getNorm(), 'chisquared', self->getChiSquared(), 'method', self.method, 'tolerance', self.tolerance, 'iterations', self.iterations)
END

;+
; Populates the AmesPAHdbIDLSuite_Fitted_Spectrum-instance.
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   Version: in, optional, type=string
;    Versioning information
;   Data: in, optional, type=struct
;     Data structure
;   PAHdb: in, optional, type=pointer
;     Pointer to parsed database file
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;   Model: in, optional, type=string
;     References
;   Units: in, optional, type="AmesPAHdb_Units_S struct"
;     Units
;   Shift: in, optional, type=float
;     Shift
;   Grid: in, optional, type="float array"
;     Grid
;   Profile: in, optional, type=string
;     Profile
;   FWHM: in, optional, type=float
;     FWHM
;   Observation: in, optional, type="AmesPAHdb_Observation_S array"
;     Observation
;   Weights: in, optional, type="struct array"
;     Weights
;   Method: in, optional, type=string
;     Method
;   Tolerance: in, optional, type=double
;     Tolerance
;   Iterations: in, optional, type=long
;     Number of iterations
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method,Tolerance=Tolerance,Iterations=Iterations

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Data) THEN self.InvalidateLazy

           IF NOT KEYWORD_SET(Observation) THEN BEGIN

              IF PTR_VALID(self.observation) THEN PTR_FREE,self.observation

              self.observation = PTR_NEW(Struct.observation)
           ENDIF

           IF NOT KEYWORD_SET(Weights) THEN BEGIN

              IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights

              self.weights = PTR_NEW(Struct.weights)
           ENDIF

           IF NOT KEYWORD_SET(Method) THEN self.method = Struct.method

           IF NOT KEYWORD_SET(Tolerance) THEN self.tolerance = Struct.tolerance

           IF NOT KEYWORD_SET(Iterations) THEN self.iterations = Struct.iterations
        ENDIF

        IF KEYWORD_SET(Data) THEN self.InvalidateLazy

        self->AmesPAHdbIDLSuite_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM
     ENDIF
  ENDIF ELSE BEGIN

     IF KEYWORD_SET(Data) THEN self.InvalidateLazy

     self->AmesPAHdbIDLSuite_Spectrum::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM
  ENDELSE

  IF KEYWORD_SET(Observation) THEN BEGIN

      IF PTR_VALID(self.observation) THEN PTR_FREE,self.observation

      self.observation = PTR_NEW(Observation)
   ENDIF

   IF KEYWORD_SET(Weights) THEN BEGIN

      IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights

      self.weights = PTR_NEW(Weights)
   ENDIF

    IF KEYWORD_SET(Method) THEN self.method = Method

    IF KEYWORD_SET(Tolerance) THEN self.tolerance = Tolerance

    IF KEYWORD_SET(Iterations) THEN self.iterations = Iterations
END

;+
; Retrieves the residual of the fit.
;
; :Returns:
;   float array
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetResidual

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self._lazy.residual) THEN $
    self._lazy.residual = PTR_NEW(((*self.observation).data.y - (*self.observation).data.continuum) - self->getFit())

  RETURN,*self._lazy.residual
END

;+
; Retrieves the total fit.
;
; :Returns:
;   float array
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetFit

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self._lazy.fit) THEN $
    self._lazy.fit = PTR_NEW(TOTAL(REFORM((*self.data).intensity, N_ELEMENTS(*self.grid), self.nuids), 2))

  RETURN,*self._lazy.fit
END

;+
; Retrieves the ordinate values of the observation.
;
; :Returns:
;   float array
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetObservation

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.observation) THEN RETURN,*self.observation

  RETURN,0
END

;+
;  Retrieves the indices intersection of two sets.
;
; :Returns:
;   long array
;
; :Params:
;   a: in, required, type="long or long array"
;    First set
;   b: in, required, type="long or long array"
;     Second set
;   Count: out, optional, type=long
;     Number in intersection
;
; :Categories:
;   SET OPERATIONS
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum__Intersection,a,b,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  min_a = MIN(a, MAX=max_a)

  min_b = Min(b, MAX=max_b)

  min_ab = min_a > min_b

  max_ab = max_a < max_b

  IF ((max_a LT min_ab) AND (min_b GT max_ab)) OR ((max_b LT min_ab) AND (min_a GT max_ab)) THEN BEGIN

     Count = 0

     RETURN, -1
  ENDIF

  r = WHERE((HISTOGRAM([a], MIN=min_ab, MAX=max_ab, REVERSE_INDICES=r_a) NE 0) AND  $
            (HISTOGRAM([b], MIN=min_ab, MAX=max_ab, REVERSE_INDICES=r_b) NE 0), Count)

  IF Count EQ 0 THEN RETURN, -1

  RETURN, TEMPORARY(r) + min_ab
END

;+
;  Retrieves the indices of the intersection of a certain
;  property from a subset of PAHs.
;
; :Params:
;   selector: in, required, type="long array"
;     Indices of the subset to consider
;   property: in, required, type=struct
;     Property structure with a 'UID'-tag
;   Count: in, optional, type=long
;    Number of matches
;
; :Returns:
;   Structure
;
; :Categories:
;   SET Operations
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::Select,selector,property,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  intersect = AmesPAHdbIDLSuite_Fitted_Spectrum__Intersection((*self.database).data.species[selector].uid, *self.uids, Count)

  IF Count GT 0 THEN BEGIN

     nitag = N_ELEMENTS(property)

     idx = ULINDGEN(nitag, Count)

     select = WHERE(property[idx MOD nitag].uid EQ intersect[idx / nitag]) MOD nitag

     RETURN,select
  ENDIF

  Count = 0

  RETURN,0
END

;+
; Updates Data and Weigths to the Intersection with UIDs
;
; :Params:
;   UIDs: in, required, type="long or long array"
;     UIDs to consider for Difference
;   Count: out, optional, type=long
;     Number of UIDs
;
; :Categories:
;   SET OPERATIONS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Intersect,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Spectrum::Intersect,UIDs,Count

  IF Count EQ 0 THEN RETURN

  nuids = N_ELEMENTS(UIDs)

  nweights = N_ELEMENTS(*self.weights)

  idx = ULINDGEN(nweights, nuids)

  select = WHERE((*self.weights)[idx MOD nweights].uid EQ UIDs[idx / nweights], nselect) MOD nweights

  *self.weights = (*self.weights)[select]
END

;+
; Retrieves the PAH size distribution of the fitted PAHs.
;
; :Keywords:
;   NBins: in, optional, type=int
;     Set the number of bins to use, otherwise Rice's rule is used.
;   Min: in, optional, type=float
;    Set the minimum size to consider.
;   Max: in, optional, type=float
;    Set the maximum size to consider.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetSizeDistribution,NBins=nbins,Min=min,Max=max

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self._lazy.sizedistribution) THEN BEGIN

    IF NOT PTR_VALID(self.database) THEN BEGIN
      PRINT
      PRINT,"========================================================="
      PRINT," VALID DATABASE POINTER NEEDED TO GET SIZE DISTRIBUTION  "
      PRINT,"========================================================="
      PRINT
      self.state = 0
      RETURN,0
   ENDIF

    IF NOT PTR_VALID(self.weights) THEN BEGIN
      PRINT
      PRINT,"========================================================="
      PRINT,"                  WEIGHTS ARE NOT SET                    "
      PRINT,"========================================================="
      PRINT
      self.state = 0
      RETURN,0
    ENDIF

    ndata = N_ELEMENTS((*self.database).data.species)

    idx = ULINDGEN(ndata, self.nuids)

    select = WHERE((*self.database).data.species[idx MOD ndata].uid EQ (*self.uids)[idx / ndata]) MOD ndata

    nc = (*self.database).data.species[select].nc

    IF N_ELEMENTS(Min) EQ 0 THEN min = MIN(nc)

    IF NOT KEYWORD_SET(Max) THEN max = MAX(nc)

    ; Rice Rule
    IF NOT KEYWORD_SET(NBins) THEN nbins = CEIL(2 * self.nuids^(1./3))

    size = min + (max - min) * DINDGEN(nbins + 1) / DOUBLE(nbins)

    distribution = DBLARR(nbins)

    idx = VALUE_LOCATE(size, nc)

    left = WHERE(idx LT 0, c)
    IF c GT 0 THEN idx[left] = 0
    right = WHERE(idx GE nbins, c)
    IF c GT 0 THEN idx[right] = nbins - 1

    FOR i = 0, self.nuids - 1 DO distribution[idx[i]] += (*self.weights)[i].weight

    self._lazy.sizedistribution = PTR_NEW({size:size, distribution:distribution})
  ENDIF

  RETURN,*self._lazy.sizedistribution
END

;+
; Retrieves the average number and standard deviation of carbon atoms of the
; fitted PAHs.
;
; :Returns:
;   double array
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetAverageNumberOfCarbonAtoms

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self._lazy.averagenumberofcarbonatoms) THEN BEGIN

    ndata = N_ELEMENTS((*self.database).data.species)

    idx = ULINDGEN(ndata, self.nuids)

    select = WHERE((*self.database).data.species[idx MOD ndata].uid EQ (*self.weights)[idx / ndata].uid) MOD ndata

    nc = (*self.database).data.species[select].nc

    tot = TOTAL((*self.weights).weight)

    avg = TOTAL(nc * (*self.weights).weight) / tot

    var = TOTAL((nc - avg)^2 * (*self.weights).weight) / (self.nuids * tot / (self.nuids - 1D))

    self._lazy.averagenumberofcarbonatoms = PTR_NEW([avg, SQRT(var)])
  END

  RETURN,*self._lazy.averagenumberofcarbonatoms
END

;+
; Retrieves the breakdown of the fitted PAHs.
;
; :Keywords:
;   Small: in, optional, type=int
;     Cut-off size for small PAHs, defaults to 50
;   Medium: in, optional, type=int
;     Cut-off size for medium PAHs, defaults to 70
;   Flux: in, optional, type=int
;     Whether to use the flux to determine the breakdown
;   Absolute: in, optional, type=int
;     Whether to return the absolute breakdown, otherwise normalize
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetBreakdown,Small=Small,Medium=Medium,Flux=Flux,Absolute=Absolute

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self._lazy.breakdown) THEN BEGIN

    signature = ([KEYWORD_SET(Small) ? Small : 0L, $
                  KEYWORD_SET(Medium) ? Medium : 0L, $
                  KEYWORD_SET(Flux), $
                  KEYWORD_SET(Absolute)]).HASHCODE()

    IF signature NE self._lazy.breakdown_sig THEN BEGIN

      PTR_FREE,self._lazy.breakdown

      self._lazy.breakdown_sig = signature
    ENDIF
  ENDIF

  IF NOT PTR_VALID(self._lazy.breakdown) THEN BEGIN

    IF  NOT PTR_VALID(self.database) THEN BEGIN
      PRINT
      PRINT,"========================================================="
      PRINT,"     VALID DATABASE POINTER NEEDED TO GET BREAKDOWN      "
      PRINT,"========================================================="
      PRINT
      self.state = 0
      RETURN,0
    ENDIF

    breakdown = {AmesPAHdb_Breakdown, $
                 anion:0D, $
                 neutral:0D, $
                 cation:0D, $
                 small:0D, $
                 medium:0D, $
                 large:0D, $
                 pure:0D, $
                 nitrogen:0D, $
                 solo:0L, $
                 duo: 0L, $
                 trio: 0L, $
                 quartet: 0L, $
                 quintet: 0L}

    ndata = N_ELEMENTS((*self.database).data.species)

    idx = ULINDGEN(ndata, self.nuids)

    select = WHERE((*self.database).data.species[idx MOD ndata].uid EQ (*self.uids)[idx / ndata]) MOD ndata

    ;; TODO: Is this useful information? Provide number of solos/etc. for the average PAH?
    breakdown.solo = TOTAL((*self.database).data.species[select].nsolo)

    breakdown.duo = TOTAL((*self.database).data.species[select].nduo)

    breakdown.trio = TOTAL((*self.database).data.species[select].ntrio)

    breakdown.quartet = TOTAL((*self.database).data.species[select].nquartet)

    breakdown.quintet = TOTAL((*self.database).data.species[select].nquintet)

    total = 1.0D

    IF KEYWORD_SET(Flux) THEN BEGIN

       IF NOT PTR_VALID(self.grid) THEN BEGIN
          PRINT
          PRINT,"========================================================="
          PRINT,"                    GRID IS NOT SET                      "
          PRINT,"========================================================="
          PRINT
         self.state = 0
         RETURN,0
       ENDIF

       classes = self->getClasses(Small=Small, Medium=Medium)

       ntags = N_TAGS(classes)

       dx = (*self.grid)[1:*] - *self.grid

       IF NOT KEYWORD_SET(Absolute) THEN BEGIN

          y = self->getFit()

          yavg = (y[0:-2] + y[1:*]) / 2.0D

          total = TOTAL(dx * yavg)
       ENDIF

       FOR i = 0, ntags - 1 DO BEGIN

          class = classes.(i)

          yavg = (class[0:-2] + class[1:*]) / 2.0D

          breakdown.(i) = TOTAL(dx * yavg) / total
      ENDFOR
    ENDIF ELSE BEGIN

      IF NOT PTR_VALID(self.weights) THEN BEGIN
         PRINT
         PRINT,"========================================================="
         PRINT,"                  WEIGHTS ARE NOT SET                    "
         PRINT,"========================================================="
         PRINT
         self.state = 0
         RETURN,0
      ENDIF

      IF NOT KEYWORD_SET(Absolute) THEN total = TOTAL((*self.weights).weight)

      select = self->Select(WHERE((*self.database).data.species.charge LT 0), $
                            *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.anion = TOTAL((*self.weights)[select].weight) / total

      select = self->Select(WHERE((*self.database).data.species.charge EQ 0), $
                            *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.neutral = TOTAL((*self.weights)[select].weight) / total

      select = self->Select(WHERE((*self.database).data.species.charge GT 0), $
                            *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.cation = TOTAL((*self.weights)[select].weight) / total

      IF NOT KEYWORD_SET(Small) THEN Small = 50L

      select = self->Select(WHERE((*self.database).data.species.nc LE Small), $
                            *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.small = TOTAL((*self.weights)[select].weight) / total

      IF NOT KEYWORD_SET(Medium) THEN Medium = 70L

      select = self->Select(WHERE((*self.database).data.species.nc GT Small AND $
                                  (*self.database).data.species.nc LE Medium), $
                            *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.medium = TOTAL((*self.weights)[select].weight) / total

      select = self->Select(WHERE((*self.database).data.species.nc GT Medium), $
                            *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.large = TOTAL((*self.weights)[select].weight) / total

      select = self->Select(WHERE((*self.database).data.species.nn EQ 0 AND $
                                  (*self.database).data.species.no EQ 0 AND $
                                  (*self.database).data.species.nmg EQ 0 AND $
                                  (*self.database).data.species.nsi EQ 0 AND $
                                  (*self.database).data.species.nfe EQ 0), $
                                  *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.pure = TOTAL((*self.weights)[select].weight) / total

      select = self->Select(WHERE((*self.database).data.species.nn GT 0), $
                            *self.weights, nselect)

      IF nselect NE 0 THEN breakdown.nitrogen = TOTAL((*self.weights)[select].weight) / total
    ENDELSE

    self._lazy.breakdown = PTR_NEW(breakdown)
  ENDIF

  RETURN,*self._lazy.breakdown
END

;+
; Retrieves the spectra of the different classes of the fitted PAHs.
;
; :Keywords:
;   Small: in, optional, type=int
;     Cut-off size for small PAHs, defaults to 50
;   Medium: in, optional, type=int
;     Cut-off size for medium PAHs defaults to 70
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetClasses,Small=Small,Medium=Medium

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self._lazy.classes) THEN BEGIN

    signature = ([KEYWORD_SET(Small) ? Small : 0L, $
                  KEYWORD_SET(Medium) ? Medium : 0L]).HASHCODE()

    IF signature NE self._lazy.classes_sig THEN BEGIN

      PTR_FREE,self._lazy.classes

      self._lazy.classes_sig = signature
    ENDIF
  ENDIF

  IF NOT PTR_VALID(self._lazy.classes) THEN BEGIN

    IF NOT PTR_VALID(self.database) THEN BEGIN
       PRINT
       PRINT,"========================================================="
       PRINT,"       VALID DATABASE POINTER NEEDED TO GET CLASSES      "
       PRINT,"========================================================="
       PRINT
       self.state = 0
       RETURN,0
    ENDIF

    ngrid = N_ELEMENTS(*self.grid)

    classes = REPLICATE({AmesPAHdb_Classes, $
                         anion:0D, $
                         neutral:0D, $
                         cation:0D, $
                         small:0D, $
                         medium:0D, $
                         large:0D, $
                         pure:0D, $
                         nitrogen:0D}, ngrid)

    select = self->Select(WHERE((*self.database).data.species.charge LT 0), $
                          *self.data, nselect)

    IF nselect NE 0 THEN classes.anion = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    select = self->Select(WHERE((*self.database).data.species.charge EQ 0), $
                          *self.data, nselect)

    IF nselect NE 0 THEN classes.neutral = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    select = self->Select(WHERE((*self.database).data.species.charge GT 0), $
                          *self.data, nselect)

    IF nselect NE 0 THEN classes.cation = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    IF NOT KEYWORD_SET(Small) THEN Small = 50L

    select = self->Select(WHERE((*self.database).data.species.nc LE Small), $
                          *self.data, nselect)

    IF nselect NE 0 THEN classes.small = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    IF NOT KEYWORD_SET(Medium) THEN Medium = 70L

    select = self->Select(WHERE((*self.database).data.species.nc GT Small AND $
                                (*self.database).data.species.nc LE Medium), $
                          *self.data, nselect)

    IF nselect NE 0 THEN classes.medium = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    select = self->Select(WHERE((*self.database).data.species.nc GT Medium), $
                          *self.data, nselect)

    IF nselect NE 0 THEN classes.large = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    select = self->Select(WHERE((*self.database).data.species.nn EQ 0 AND $
                                (*self.database).data.species.no EQ 0 AND $
                                (*self.database).data.species.nmg EQ 0 AND $
                                (*self.database).data.species.nsi EQ 0 AND $
                                (*self.database).data.species.nfe EQ 0), $
                                *self.data, nselect)

    IF nselect NE 0 THEN classes.pure = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    select = self->Select(WHERE((*self.database).data.species.nn GT 0), $
                          *self.data, nselect)

    IF nselect NE 0 THEN classes.nitrogen = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

    self._lazy.classes = PTR_NEW(classes)
  ENDIF

  RETURN,*self._lazy.classes
END

;+
; Retrieves the weights of the fitted PAHs
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetWeights

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.weights) THEN RETURN,*self.weights

  RETURN,0
END

;+
; Retrieves the Norm of the fit
;
; :Returns:
;   float
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetNorm

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self._lazy.norm) THEN $
    self._lazy.norm = PTR_NEW(SQRT(TOTAL(((*self.observation).data.y-(*self.observation).data.continuum-self->getFit())^2, /NAN)))

  RETURN,*self._lazy.norm
END

;+
; Retrieves the Chi-squared of the fit.
;
; :Returns:
;   float
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetChiSquared

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self._lazy.chisquared) THEN $
    self._lazy.chisquared = PTR_NEW(TOTAL(((*self.observation).data.y-(*self.observation).data.continuum-self->getFit())^2 / (*self.observation).data.ystdev, /NAN))

  RETURN,*self._lazy.chisquared
END

;+
; Retrieves the error for the fit.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetError

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self._lazy.error) THEN BEGIN

    srt = SORT(*self.grid)
    x = (*self.grid)[srt]
    y = ((*self.observation).data.y-(*self.observation).data.continuum)[srt]
    r = (ABS(self->getResidual()))[srt]

    tags = ['err',  'e127',  'e112',  'e77', 'e62', 'e33']
    piecewise = CREATE_STRUCT(NAME='AmesPAHdb_Piecewise_Error', $
                              tags, -1.0D, -1.0D, -1.0D, -1.0D, -1.0D, -1.0D)

    range = [[MIN(x, MAX=xmax), xmax], $
             [ 754.0,  855.0], $
             [ 855.0, 1000.0], $
             [1000.0, 1495.0], $
             [1495.0, 1712.0], $
             [2900.0, 3125.0]]

    ntags = N_ELEMENTS(tags)
    FOR i = 0L, ntags - 1L DO BEGIN

      sel = WHERE(x GE range[0, i] AND x LE range[1, i], nsel)

      IF nsel EQ 0 THEN CONTINUE

      dx = (*self.grid)[sel[1:*]] - (*self.grid)[sel]
      yavg = (y[sel[0:-2]] + y[sel[1:*]]) / 2D
      ravg = (r[sel[0:-2]] + r[sel[1:*]]) / 2D

      piecewise.(i) = TOTAL(dx * ravg) / TOTAL(dx * yavg)
    ENDFOR

    self._lazy.error = PTR_NEW(piecewise)
  ENDIF

  RETURN,*self._lazy.error
END

;+
; Retrieves the method used for the fit.
;
; :Returns:
;   string
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetMethod

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.method
END

;+
; Retrieves the tolerance used for the fit.
;
; :Returns:
;   double
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetTolerance

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.tolerance
END

;+
; Retrieves the number of iterations needed for the fit.
;
; :Returns:
;   long
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetIterations

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.iterations
END

;+
; Invalidate the AmesPAHdbIDLSuite_Fitted_Lazy structure.
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::InvalidateLazy

  COMPILE_OPT IDL2

  ON_ERROR,2

  FOR i = 0L, N_TAGS(self._lazy) - 1L DO BEGIN

    IF PTR_VALID(self._lazy.(i)) THEN PTR_FREE,self._lazy.(i)
   ENDFOR
END

;+
; Clean-up an AmesPAHdbIDLSuite_Fitted_Spectrum-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.InvalidateLazy

  IF PTR_VALID(self.observation) THEN PTR_FREE,self.observation

  IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights

  self->AmesPAHdbIDLSuite_Spectrum::Cleanup
END

;+
; Create an AmesPAHdbIDLSuite_Fitted_Spectrum-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Fitted_Spectrum-instance
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   Version: in, optional, type=string
;    Versioning information
;   Data: in, optional, type=struct
;     Data structure
;   PAHdb: in, optional, type=pointer
;     Pointer to parsed database file
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;   Model: in, optional, type=string
;     References
;   Units: in, optional, type="AmesPAHdb_Units_S struct"
;     Units
;   Shift: in, optional, type=float
;     Shift
;   Grid: in, optional, type="float array"
;     Grid
;   Profile: in, optional, type=string
;     Profile
;   FWHM: in, optional, type=float
;     FWHM
;   Observation: in, optional, type="AmesPAHdb_Observation_S array"
;     Observation
;   Weights: in, optional, type="struct array"
;     Weights
;   Method: in, optional, type=string
;     Method
;   Tolerance: in, optional, type=double
;     Tolerance
;   Iterations: in, optional, type=long
;     Number of iterations
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method,Tolerance=Tolerance,Iterations=Iterations

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method,Tolerance=Tolerance,Iterations=Iterations $
  ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method,Tolerance=Tolerance,Iterations=Iterations

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Fitted_Spectrum Class
;
; :Fields:
;   observation: type=pointer
;    Pointer to an observation structure
;   weights: type=pointer
;     Pointer to a weights structure
;   method: type=string
;     Method used for the fit
;   tolerance: type=double
;     Tolerance used for the fit
;   iterations: type=long
;     Number of iterations needed for the fit
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Fitted_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Fitted_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Spectrum, $
          observation:PTR_NEW(), $
          weights:PTR_NEW(), $
          method:'', $
          tolerance:0D, $
          iterations:0L, $
          _lazy:{AmesPAHdbIDLSuite_Fitted_Lazy, $
                 residual:PTR_NEW(), $
                 fit:PTR_NEW(), $
                 sizedistribution:PTR_NEW(), $
                 averagenumberofcarbonatoms:PTR_NEW(), $
                 breakdown:PTR_NEW(), $
                 breakdown_sig:0L, $
                 classes:PTR_NEW(), $
                 classes_sig:0L, $
                 norm:PTR_NEW(), $
                 chisquared:PTR_NEW(), $
                 error:PTR_NEW() $
                 }}
END

; END OF amespahdbidlsuite_fitted_spectrum__define.pro
