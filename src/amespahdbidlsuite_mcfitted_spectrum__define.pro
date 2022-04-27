; docformat = 'rst'

;+
;
; Class to manage a spectrum fitted using the Monte Carlo approach.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create and destroy an
;   AmesPAHdbIDLSuite_MCFitted_Spectrum-instance::
;
;     IDL> mcfitted = OBJ_NEW('AmesPAHdbIDLSuite_MCFitted_Spectrum')
;     IDL> mcfitted->Set,data
;     IDL> mcfitted->Plot
;     IDL> OBJ_DESTORY,mcfitted
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
;     04-25-2022
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
PRO AmesPAHdbIDLSuite_MCFitted_Spectrum::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  (*self.obj)[0]->Description,Str

  Str = [Str, $
         STRING(FORMAT='(A-12,":",X,A0)', "Monte Carlo"), $
         STRING(FORMAT='(A-12,":",X,A0)', "|_draw", self.distribution), $
         STRING(FORMAT='(A-12,":",X,I0)', "|_samples", N_ELEMENTS(*self.obj))]

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
;    Size: in, optional, type=int
;      Toggle to plot the size breakdown of the fit
;    Charge: in, optional, type=int
;      Toggle to plot the charge breakdown of the fit
;    Composition: in, optional, type=int
;      Toggle to plot the compositional breakdown of the fit
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
PRO AmesPAHdbIDLSuite_MCFitted_Spectrum::Plot,DistributionSize=DistributionSize,Size=Size,Charge=Charge,Composition=Composition,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Legend=Legend,Color=Color,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400

  IF KEYWORD_SET(DistributionSize) THEN GOTO,SIZEDISTRIBUTION

  obj_s = (*self.obj)[0]->Get()

  x = obj_s.grid

  nx = N_ELEMENTS(x)

  xunits = obj_s.units.abscissa.str

  xrange = [MAX(x, MIN=xmin), xmin]

  obs_xstdev = obj_s.observation.data.xstdev

  IF KEYWORD_SET(Wavelength) THEN BEGIN

     obs_xstdev = 1D4 * obs_xstdev / x^2

     x = 1D4 / x

     xrange = [MIN(x, MAX=xmax), xmax]

     xunits = 'wavelength [!Mm!Xm]'
  ENDIF

  yunits = obj_s.observation.units.ordinate.str

  IF yunits NE '' THEN yunits += '!C'

  yunits += obj_s.units.ordinate.str

  fit = self->getFit()

  fit[*, 1] = SQRT(fit[*, 1])

  IF NOT KEYWORD_SET(Oplot) THEN BEGIN

     IF KEYWORD_SET(Size) OR KEYWORD_SET(Charge) OR KEYWORD_SET(COMPOSITION) THEN self->AmesPAHdbIDLSuite_Plot::Plot,[0,1],[0,1],COLOR=Color,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN([obj_s.observation.data.y - obj_s.observation.data.ystdev, obj_s.observation.data.continuum + fit[*, 0] - fit[*, 1]]), MAX([obj_s.observation.data.y + obj_s.observation.data.ystdev, obj_s.observation.data.continuum + fit[*, 0] + fit[*, 1]])],XTITLE=xunits,YTITLE=yunits,POSITION=[0.2,0.2,0.95,0.9],/NoData,_EXTRA=EXTRA $
     ELSE self->AmesPAHdbIDLSuite_Plot::Plot,[0,1],[0,1],COLOR=Color,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN([obj_s.observation.data.y - obj_s.observation.data.ystdev, obj_s.observation.data.continuum + fit[*, 0]] - fit[*, 1]), MAX([obj_s.observation.data.y + obj_s.observation.data.ystdev, obj_s.observation.data.continuum + fit[*, 0] +fit[*, 1] ])],XTICKFORMAT='(A1)',YTITLE=yunits,POSITION=[0.2,0.45,0.95,0.9],/NoData,_EXTRA=EXTRA
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.y,obj_s.observation.data.ystdev,obs_xstdev,COLOR=14

  self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.y,LINESTYLE=5,COLOR=14

  self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum,LINESTYLE=2

  self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+fit[*, 0],fit[*, 1]

  self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum+fit[*, 0]

  IF SIZE(Legend, /TYPE) EQ 0 THEN Legend = 1

  IF Legend THEN BEGIN

     self->Description,outs

     self->AmesPAHdbIDLSuite_Plot::Legend,outs
  ENDIF

  IF NOT KEYWORD_SET(Color) THEN Color = 2

  IF KEYWORD_SET(Size) OR KEYWORD_SET(Charge) OR KEYWORD_SET(COMPOSITION) THEN BEGIN

     classes = self->getClasses()

     ntags = N_TAGS(classes)
     FOR i = 0L, ntags - 1 DO classes[*, 1].(i) = SQRT(classes[*, 1].(i))

     IF KEYWORD_SET(Size) THEN BEGIN

        self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+classes[*, 0].small,classes[*, 1].small,Color=Color

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum+classes[*, 0].small,Stick=Stick,Fill=Fill,COLOR=Color

        XYOUTS,0.25,0.75,'small',COLOR=Color,/Normal

        self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+classes[*, 0].large,classes[*, 1].large,Color=Color+1

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum+classes[*, 0].large,Stick=Stick,Fill=Fill,COLOR=Color+1

        XYOUTS,0.25,0.70,'large',COLOR=Color+1,/NORMAL
     ENDIF ELSE IF KEYWORD_SET(Charge) THEN BEGIN

        self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+classes[*, 0].anion,classes[*, 1].anion,COLOR=Color

        self->AmesPAHdbIDLSuite_Plot::oPlot,x,obj_s.observation.data.continuum+classes[*, 0].anion,Stick=Stick,Fill=Fill,COLOR=Color

        XYOUTS,0.25,0.75,'anion',COLOR=Color,/Normal

        self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+classes[*, 0].neutral,classes[*, 1].neutral,COLOR=Color+1

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum+classes[*, 0].neutral,Stick=Stick,Fill=Fill,COLOR=Color+1

        XYOUTS,0.25,0.70,'neutral',COLOR=Color+1,/Normal

        self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+classes[*, 0].cation,classes[*, 1].cation,Stick=Stick,Fill=Fill,COLOR=Color+2

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum+classes[*, 0].cation,COLOR=Color+2

        XYOUTS,0.25,0.65,'cation',COLOR=Color+2,/Normal
     ENDIF ELSE BEGIN

        self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+classes[*, 0].pure,classes[*, 1].pure,Stick=Stick,Fill=Fill,COLOR=Color

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum+classes[*, 0].pure,COLOR=Color

        XYOUTS,0.25,0.75,'pure',COLOR=Color,/Normal

        self->AmesPAHdbIDLSuite_Plot::OplotError,x,obj_s.observation.data.continuum+classes[*, 0].nitrogen,classes[*, 1].nitrogen,Stick=Stick,Fill=Fill,COLOR=Color+1

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,obj_s.observation.data.continuum+classes[*, 0].nitrogen,COLOR=Color+1

        XYOUTS,0.25,0.70,'nitrogen',COLOR=Color+1,/Normal
     ENDELSE
  ENDIF ELSE BEGIN

     res = self->getResidual()

     y = -100D *  res[*, 0] / obj_s.observation.data.y

     ystdev = 100 * SQRT(res[*, 1])

     self->AmesPAHdbIDLSuite_Plot::Plot,x,y,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN(y - ystdev), MAX(y + ystdev)],XTITLE=xunits,YTITLE='residual!C[%]',POSITION=[0.2,0.2,0.95,0.45],/NoData,/NOERASE,_EXTRA=EXTRA

     self->AmesPAHdbIDLSuite_Plot::OplotError,x,y,ystdev

     self->AmesPAHdbIDLSuite_Plot::Oplot,!X.CRANGE,[0,0],Color=14,LINESTYLE=5

     self->AmesPAHdbIDLSuite_Plot::Oplot,x,y,Stick=Stick,Fill=Fill,COLOR=Color
  ENDELSE

  GOTO,FINISH

SIZEDISTRIBUTION:

  sd = self->getSizeDistribution(_EXTRA=EXTRA)

  tot = TOTAL(sd.distribution[*, 0])

  dist = 100.0 * sd.distribution[*, 0] / tot

  unc = 100.0 * SQRT(sd.distribution[*, 1]) / tot

  nbins = N_ELEMENTS(sd.size) - 1

  PLOT,[0,1],[0,1],XRANGE=[sd.size[0], sd.size[nbins]],YRANGE=[0, MAX(dist + unc)],XTITLE='n!LCarbon!N',YTITLE='frequency [%]',POSITION=[0.2,0.2,0.95,0.9],/NODATA

  PLOTS,[sd.size[0],sd.size[0],sd.size[1]],[0,dist[0],dist[0]],COLOR=2,THICK=2

  FOR i = 1, nbins - 1 DO PLOTS,[sd.size[i], sd.size[i], sd.size[i+1]],[dist[i-1],dist[i],dist[i]],COLOR=2,THICK=2

  PLOTS,[1,1]*sd.size[nbins],[0,dist[nbins-1]],COLOR=2,THICK=2

  FOR i = 0L, nbins - 1L DO PLOTS,sd.size[i]*[1,1]+(sd.size[i+1]-sd.size[i])/2.0,dist[i]+[-1.0,1.0]*unc[i],COLOR=2,THICK=2

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
PRO AmesPAHdbIDLSuite_MCFitted_Spectrum::Write,Filename

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

  obj_s = (*self.obj)[0]->Get()

  abscissa = STREGEX(obj_s.units.abscissa.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT)

  ordinate = STREGEX(obj_s.units.ordinate.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT)

  ordinate_unc = ordinate[1] + '_unc'

  half_abscissa_len = STRLEN(abscissa[1]) / 2
  half_ordinate_len = STRLEN(ordinate[1]) / 2
  half_ordinate_unc_len = STRLEN(ordinate_unc) / 2


  fmt1 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_unc_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_unc_len) + 'X,' + $
          '"|")'

  fmt2 = '("|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|")'

  half_abscissa_len = STRLEN(abscissa[2]) / 2
  half_ordinate_len = STRLEN(ordinate[2]) / 2

  fmt3 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|")'

  cols = [STRING(FORMAT=fmt1,STRUPCASE(abscissa[1]),STRUPCASE(ordinate[1]),STRUPCASE(ordinate_unc)), $
          STRING(FORMAT=fmt2,"double","double","double"), $
          STRING(FORMAT=fmt3,abscissa[2],ordinate[2],ordinate[2])]


  grid = self->GetGrid()

  fit = self->GetFit()

  OPENW,funit,Filename,/GET_LUN
  PRINTF,funit,FORMAT='("\",A0)',hdr[0:WHERE(STRPOS(hdr, 'END') EQ 0)]
  PRINTF,funit,STRJOIN(cols, STRING( 10B ))

  n = N_ELEMENTS(grid)
  FOR i = 0L, n - 1L DO PRINTF,funit,FORMAT='(X,F25.6,X,F25.6,X,F25.6)',grid[i],fit[i,0],SQRT(fit[i,1])

  CLOSE,funit
  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"    WRITTEN IPAC TABLE: ", Filename
  PRINT,"========================================================="
  PRINT
END

;+
; Retrieves the AmesPAHdbIDLSuite_MCFitted_Spectrum representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.obj) THEN RETURN, 0

  struct.type = OBJ_CLASS(self)+'_S'

  RETURN,CREATE_STRUCT(struct, 'obj', *self.obj, 'distribution', self.distribution)
END

;+
; Populates the AmesPAHdbIDLSuite_MCFitted_Spectrum-instance.
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   obj: in, optional, type=obj-array
;     Array holding AmesPAHdbIDLSuiteFittedSpectrum instances
;   distribution: in, optional, type=string
;     Distribution used for permutating errors
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_MCFitted_Spectrum::Set,Struct,Type=Type,Obj=Obj,Distribution=Distribution

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Obj) THEN BEGIN

              IF PTR_VALID(self.obj) THEN PTR_FREE,self.obj

              self.obj = PTR_NEW(Struct.obj)
           ENDIF

          IF NOT KEYWORD_SET(Distribution) THEN self.distribution = Struct.distribution
        ENDIF
     ENDIF
  ENDIF

  IF KEYWORD_SET(Obj) THEN BEGIN

      IF PTR_VALID(self.obj) THEN PTR_FREE,self.obj

      self.obj = PTR_NEW(Obj)
   ENDIF

   IF KEYWORD_SET(Distribution) THEN self.distribution = Distribution
END

;+
; Retrieves the residual of the fit.
;
; :Returns:
;    A n by four-element vector containing the mean, variance, skewness, and kurtosis
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetResidual

  COMPILE_OPT IDL2

  ON_ERROR,2

  obs = (*self.obj)[0].GetObservation()
  y = obs.data.y - obs.data.continuum

  nobj = N_ELEMENTS(*self.obj)

  res = DBLARR(nobj, N_ELEMENTS(y))
  FOR i = 0L, nobj - 1L DO res[i, *] = y - (*self.obj)[i]->GetFit()
  
  RETURN,MOMENT(res, DIMENSION=1)
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
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetFit

  COMPILE_OPT IDL2

  ON_ERROR,2

  nobj = N_ELEMENTS(*self.obj)

  spc = DBLARR(nobj, N_ELEMENTS((*self.obj)[0]->GetGrid()))

  FOR i = 0L, nobj - 1L DO spc[i, *] = (*self.obj)[i]->GetFit()

  RETURN,MOMENT(spc, DIMENSION=1)
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
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetObservation

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.obj) THEN RETURN,(*self.obj)[0]->GetObservation()

  RETURN,0
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
;   A structure with the distribution as a four-element vectors
;   containing the mean, variance, skewness, and kurtosis
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetSizeDistribution,NBins=nbins,Min=min,Max=max

  COMPILE_OPT IDL2

  ON_ERROR,2

  nobj = N_ELEMENTS(*self.obj)

  his = PTRARR(nobj, /ALLOCATE_HEAP)

  left = 1D100 ; a Google!

  right = 0

  n = 0L

  FOR i = 0L, nobj - 1L DO BEGIN

    (*his[i]) = (*self.obj)[i]->GetSizeDistribution(NBins=nbins, Min=min, Max=max)

    hleft = MIN((*his[i]).size, MAX=hright)

    IF hleft LT left THEN min = hleft
    IF hright GT right THEN max = hright

    n += N_ELEMENTS((*his[i]).size)
  ENDFOR

  nbins = FLOOR((n - 1L) / FLOAT(nobj))

  size = min + (max - min) * DINDGEN(nbins + 1) / DOUBLE(nbins)

  dist = DBLARR(nbins, nobj)


  PLOT,(*his[0]).size,(*his[0]).distribution,PSYM=10,YRANGE=[0,1.5]

  FOR i = 0L, nobj - 1L DO BEGIN

    idx = VALUE_LOCATE(size, (*his[i]).size)

    left = WHERE(idx LT 0, c)
    IF c GT 0 THEN idx[left] = 0
    right = WHERE(idx GE nbins, c)
    IF c GT 0 THEN idx[right] = nbins - 1

    n = N_ELEMENTS((*his[i]).distribution)
    FOR j = 0L, n - 1L DO dist[idx[j], i] = (*his[i]).distribution[j]
  ENDFOR

  FOR i = 0L, nobj - 1L DO PTR_FREE,his[i]

  RETURN,{size:size, distribution:MOMENT(dist, DIMENSION=2)}
END

;+
; Retrieves the breakdown of the fitted PAHs.
;
; :Keywords:
;   Small: in, optional, type=int
;     Cut-off size for small PAHs
;   Flux: in, optional, type=int
;     Whether to use the flux to determine the breakdown
;   Absolute: in, optional, type=int
;     Whether to return the absolute breakdown, otherwise normalize
;
; :Returns:
;   A structure with four-element vectors containing the mean,
;   variance, skewness, and kurtosis
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetBreakdown,Small=Small,Flux=Flux,Absolute=Absolute

  COMPILE_OPT IDL2

  ON_ERROR,2

  nobj = N_ELEMENTS(*self.obj)

  bd = REPLICATE({AmesPAHdb_Breakdown, $
                   anion:0D, $
                   neutral:0D, $
                   cation:0D, $
                   small:0D, $
                   large:0D, $
                   pure:0D, $
                   nitrogen:0D, $
                   solo:0L, $
                   duo:0L, $
                   trio:0L, $
                   quartet:0L, $
                   quintet:0L}, nobj)

  FOR i = 0L, nobj - 1L DO $
    bd[i] = (*self.obj)[i]->GetBreakdown(Small=Small,Flux=Flux,Absolute=Absolute)

  mcbd = {AmesPAHdb_MCBreakdown, $
          anion:DBLARR(4), $
          neutral:DBLARR(4), $
          cation:DBLARR(4), $
          small:DBLARR(4), $
          large:DBLARR(4), $
          pure:DBLARR(4), $
          nitrogen:DBLARR(4), $
          solo:LONARR(4), $
          duo:LONARR(4), $
          trio:LONARR(4), $
          quartet:LONARR(4), $
          quintet:LONARR(4)}

  ntags = N_TAGS(bd)

  FOR i = 0L, ntags - 1L DO $
    mcbd.(i) = MOMENT(bd.(i))

  RETURN,mcbd
END

;+
; Retrieves the spectra of the different classes of the fitted PAHs.
;
; :Keywords:
;   Small: in, optional, type=int
;     Cut-off size for small PAHs
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetClasses,Small=Small

  COMPILE_OPT IDL2

  ON_ERROR,2

  nobj = N_ELEMENTS(*self.obj)

  ngrid = N_ELEMENTS((*self.obj)[0]->GetGrid())

  cls = REPLICATE({AmesPAHdb_Classes, $
                   anion:0D, $
                   neutral:0D, $
                   cation:0D, $
                   small:0D, $
                   large:0D, $
                   pure:0D, $
                   nitrogen:0D}, ngrid, nobj)

  FOR i = 0L, nobj - 1L DO $
    cls[*, i] = (*self.obj)[i]->GetClasses(Small=Small)

  mccls = REPLICATE({AmesPAHdb_Classes, $
                     anion:0D, $
                     neutral:0D, $
                     cation:0D, $
                     small:0D, $
                     large:0D, $
                     pure:0D, $
                     nitrogen:0D}, ngrid, 4)

  ntags = N_TAGS(cls)

  FOR i = 0L, ntags - 1L DO $
    mccls.(i) = MOMENT(cls.(i), DIMENSION=2)

  RETURN,mccls
END

;+
; Retrieves the abscissa valuesa.
;
; :Returns:
;   double array (1D)
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetGrid

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.obj) THEN RETURN, (*self.obj)[0]->GetGrid()

  RETURN,0
END

;+
; Retrieves the Norm of the fit
;
; :Returns:
;   A four-element vector containing the mean, variance, skewness, and kurtosis
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetNorm

  COMPILE_OPT IDL2

  ON_ERROR,2

  obs = (*self.obj)[0]->GetObservation()
  y = obs.data.y - obs.data.continuum

  nobj = N_ELEMENTS(*self.obj)
  nrm = DBLARR(nobj)
  FOR i = 0L,  nobj - 1L DO nrm[i] = SQRT(TOTAL((y - (*self.obj)[i]->getFit())^2, /NAN))
  
  RETURN,MOMENT(nrm)
END

;+
; Retrieves the Chi-squared of the fit.
;
; :Returns:
;   A four-element vector containing the mean, variance, skewness, and kurtosis
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetChiSquared

  COMPILE_OPT IDL2

  ON_ERROR,2

  obs = (*self.obj)[0]->GetObservation()
  y = obs.data.y - obs.data.continuum
  ystdev = obs.data.ystdev

  nobj = N_ELEMENTS(*self.obj)
  chi = DBLARR(nobj)
  FOR i = 0L,  nobj - 1L DO chi[i] = TOTAL((y - (*self.obj)[i]->getFit())^2 / ystdev, /NAN)
  
  RETURN,MOMENT(chi)
END

;+
; Retrieves the error for the fit.
;
; :Returns:
;   A four-element vector containing the mean, variance, skewness, and kurtosis
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetError

  COMPILE_OPT IDL2

  ON_ERROR,2

  grid = (*self.obj)[0]->GetGrid()
  srt = SORT(grid)
  x = grid[srt]

  obs = (*self.obj)[0]->GetObservation()
  y = obs.data[srt].y - obs.data[srt].continuum

  int = INT_TABULATED(x, y)

  nobj = N_ELEMENTS(*self.obj)
  err = DBLARR(nobj)
  FOR i = 0L,  nobj - 1L DO BEGIN

    res = ABS((*self.obj)[i]->getResidual())
    err[i] = INT_TABULATED(x, res[srt]) / int
  ENDFOR

  RETURN,MOMENT(err)
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
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetMethod

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,(*self.obj)[0]->GetMethod()
END

;+
; Retrieves the distribution used for permutating errors.
;
; :Returns:
;   string
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::GetDistribution

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.distribution
END

;+
; Clean-up an AmesPAHdbIDLSuite_MCFitted_Spectrum-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_MCFitted_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup

  IF PTR_VALID(self.obj) THEN OBJ_DESTROY,*self.obj
END

;+
; Create an AmesPAHdbIDLSuite_MCFitted_Spectrum-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_MCFitted_Spectrum-instance
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   obj: in, optional, type=obj-array
;     Array holding AmesPAHdbIDLSuiteFittedSpectrum instances
;   distribution: in, optional, type=string
;     Distribution used for permutating errors
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_MCFitted_Spectrum::Init,Struct,Type=Type,Obj=Obj,Distribution=Distribution

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Obj=Obj,Distribution=Distribution $
  ELSE self->Set,Type=Type,Obj=Obj,Distribution=Distribution

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_MCFitted_Spectrum Class
;
; :Fields:
;   state: type=long
;     Internal state
;   obj: type=obj-array
;     Array holding AmesPAHdbIDLSuiteFittedSpectrum instances
;   distribution: type=string
;     Distribution used for permutating errors
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_MCFitted_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_MCFitted_Spectrum, $
          INHERITS IDL_Object, $
          INHERITS AmesPAHdbIDLSuite_Plot, $
          state:0L, $
          obj:PTR_NEW(), $
          distribution:''}
END

; END OF AmesPAHdbIDLSuite_MCFitted_Spectrum__define.pro
