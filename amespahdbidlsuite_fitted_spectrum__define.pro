;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Fitted_Spectrum
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Class to hold a fitted spectrum
;
; CATEGORY:
;    DATA
;
; SUPERCLASSES:
;    AMESPAHDBIDLSUITE_SPECTRUM
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;    AMESPAHDBIDLSUITE_SPECTRUM
;
; OPTIONAL:
;   NONE
;
; CREATION:
;    fitted = OBJ_NEW('AmesPAHdbIDLSuite_Fitted_Spectrum')
;
; METHODS
;    PUBLIC:
;      DESCRIPTION (PROCEDURE)
;      PLOT (PROCEDURE)
;      WRITE (PROCEDURE)
;      SORT (PROCEDURE)
;      GET (FUNCTION)
;      SET (PROCEDURE)
;      GETWEIGHTS (FUNCTION)
;      GETNORM (FUNCTION)
;      GETCHISQUARED (FUNCTION)
;      GETMETHOD (FUNCTION)
;      GETOBSERVATION (FUNCTION)
;      GETFIT (FUNCTION)
;      GETRESIDUAL (FUNCTION)
;      GETCLASSES (FUNCTION)
;      GETBREAKDOWN (FUNCTION)
;
;    PRIVATE:
;      SELECT (FUNCTION)
;      AMESPAHDBIDLSUITE_FITTED_SPECTRUM__INTERSECTION (FUNCTION)
;
;  EXAMPLE
;     fitted = OBJ_NEW('AmesPAHdbIDLSuite_Fitted_Spectrum')
;     fitted->Set,data
;     fitted->Plot
;     OBJ_DESTORY,fitted
;
; MODIFICATION HISTORY
;
;   07-12-2015
;   Added GETMETHOD and its infrastructure. Christiaan Boersma
;   05-20-2015
;   Fixed missing Struct in SET. Christiaan Boersma.
;   05-03-2015
;   Updated formatting for DESCRIPTION. Christiaan Boersma.
;   04-24-2015
;   Refactored GETRESIDUAL and GETBREAKDOWN to use GETFIT and other
;   small fixes. Christiaan Boersma
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

;; OUTPUT

PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  self->AmesPAHdbIDLSuite_Spectrum::Description,Str
  
  Str = [Str, STRING(FORMAT='(A-12,":",X,A0)', "fit", self.getMethod())]
  
  Str = [Str, STRING(FORMAT='(A-12,":",X,g-8.3)', "|_norm", self->getNorm())]

  Str = [Str, STRING(FORMAT='(A-12,":",X,g-8.3)', "|_chisquared", self->getChiSquared())]

  Str = STRJOIN(Str, "!C")
  
  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Plot,Residual=Residual,Size=Size,Charge=Charge,Composition=Composition,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Legend=Legend,Color=Color,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400
  
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

     classes = self->getClasses()

     IF KEYWORD_SET(Size) THEN BEGIN

        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.small,Stick=Stick,Fill=Fill,COLOR=Color

        XYOUTS,0.25,0.75,'small',COLOR=Color,/Normal
        
        self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.observation).data.continuum+classes.large,Stick=Stick,Fill=Fill,COLOR=Color+1

        XYOUTS,0.25,0.70,'large',COLOR=Color+1,/NORMAL
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

     ystdev = -100 * (*self.observation).data.ystdev / (*self.observation).data.y

     self->AmesPAHdbIDLSuite_Plot::Plot,x,y,XRANGE=[MIN(x - obs_xstdev), MAX(x + obs_xstdev)],YRANGE=[MIN(y - ystdev), MAX(y + ystdev)],XTITLE=xunits,YTITLE='residual!C[%]',POSITION=[0.2,0.2,0.95,0.45],/NoData,/NOERASE,_EXTRA=EXTRA

     self->AmesPAHdbIDLSuite_Plot::OplotError,x,0*x,100*(*self.observation).data.ystdev/(*self.observation).data.y,obs_xstdev,Color=14    
                                                                                                                                                                
     self->AmesPAHdbIDLSuite_Plot::Oplot,!X.CRANGE,[0,0],Color=14,LINESTYLE=5

     self->AmesPAHdbIDLSuite_Plot::Oplot,x,y,Stick=Stick,Fill=Fill,COLOR=Color
  ENDIF ELSE IF NOT (KEYWORD_SET(Size) OR KEYWORD_SET(Charge) OR KEYWORD_SET(COMPOSITION)) THEN BEGIN
     
     PLOT,[0,1],[0,1],/NODATA,POSITION=[0.05,0.2,0.25,0.9],XTICKS=1,YTICKS=1,XMINOR=1,YMINOR=1,XTICKFORMAT='(A1)',YTICKFORMAT='(A1)',/NOERASE

     !P.CHARSIZE = 1.25
  
     XYOUTS,0.055,0.87,STRING(FORMAT='(A3,4X,A6)', 'UID', 'WEIGHT'),/NORMAL

     i = 0

     ypos = 0.87 - 0.03

     WHILE i LT self.nuids AND ypos GT 0.225 DO BEGIN

        XYOUTS,0.055,ypos,STRING(FORMAT='(I03,4X,g-8.3)', (*self.weights)[i].uid, (*self.weights)[i].weight),COLOR=Color+i++,/NORMAL

        ypos -= 0.025
     ENDWHILE
     
     IF i NE self.nuids THEN XYOUTS,0.055,ypos,STRING(FORMAT='("+",I0,X,"more...")', self.nuids - i),/NORMAL
  ENDIF

  
  self->AmesPAHdbIDLSuite_Plot::Restore
END

PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Write,Prefix

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() LT 1 THEN Prefix = OBJ_CLASS(self)

  n = N_ELEMENTS(*self.grid)
  
  intensities = REFORM((*self.data).intensity, n, self.nuids)

  srt = SORT(*self.uids)

  intensities = intensities[*, srt]
  
  filename = STRING(FORMAT='(A0,".txt")', Prefix)
  
  OPENW,funit,filename,/GET_LUN

  PRINTF,funit,OBJ_CLASS(self)

  PRINTF,funit,FORMAT='(A-20,2X,'+STRING(self.nuids)+'(I-40,2X))',"UID:",(*self.uids)[srt]

  srt = SORT((*self.weights).uid)
  
  PRINTF,funit,FORMAT='(A-20,2X,'+STRING(self.nuids)+'(G-40.6,2X))',"WEIGHT:",(*self.weights)[srt].weight
  
  PRINTF,funit,FORMAT='(A-20,2X,'+STRING(self.nuids)+'(A-40,2X))',self.units.abscissa.str,MAKE_ARRAY(self.nuids, /STRING, VALUE=self.units.ordinate.str)

  FOR i = 0L, n - 1 DO PRINTF,funit,FORMAT='(G12.6,10X,'+STRING(n)+'(G12.6,30X))',(*self.grid)[i],intensities[i,*]

  CLOSE,funit

  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"                WRITTEN: ", filename
  PRINT,"========================================================="
  PRINT
END

;; SORT

PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Sort,Weights,Flux=Flux

  COMPILE_OPT IDL2

  ON_ERROR,2

  Weights = REPLICATE({AmesPAHdbIDLSuite_Weights_S, uid:0L, weight:0D}, self.nuids)
  
  IF NOT KEYWORD_SET(Flux) THEN Weights.weight = (*self.weights).weight $
  ELSE BEGIN

     grid = *self.grid
     
     FOR i = 0L, self.nuids - 1 DO BEGIN

        select = WHERE((*self.data).uid EQ (*self.uids)[i])
        
        Weights[i].weight = INT_TABULATED(grid, (*self.data)[select].intensity, /SORT)        
     ENDFOR
  ENDELSE

  Weights.uid = *self.uids

  srt = REVERSE(SORT(Weights.weight))

  Weights = weights[srt]

  IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights
  
  self.weights = PTR_NEW(Weights[srt])

  *self.uids = (*self.uids)[srt]
END

;; GET/SET

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0
  
  struct = self->AmesPAHdbIDLSuite_Spectrum::Get()

  struct.type = OBJ_CLASS(self)+'_S'
  
  RETURN,CREATE_STRUCT(struct, 'observation', *self.observation, 'weights', *self.weights, 'norm', self->getNorm(), 'chisquared', self->getChiSquared(), 'method', self.method)
END

PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN
        
        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Observation) THEN BEGIN

              IF PTR_VALID(self.observation) THEN PTR_FREE,self.observation
           
              self.observation = PTR_NEW(Struct.observation)
           ENDIF
        
           IF NOT KEYWORD_SET(Weights) THEN BEGIN

              IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights
           
              self.weights = PTR_NEW(Struct.weights)
           ENDIF
        ENDIF

        IF NOT KEYWORD_SET(Method) THEN self.method = Struct.method
        
        self->AmesPAHdbIDLSuite_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM
     ENDIF
  ENDIF ELSE BEGIN

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
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetResidual

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,((*self.observation).data.y - (*self.observation).data.continuum) - self->getFit()
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetFit

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,TOTAL(REFORM((*self.data).intensity, N_ELEMENTS(*self.grid), self.nuids), 2)
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetObservation

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.observation) THEN RETURN,*self.observation
  
  RETURN,0
END

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

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::Select,selector,property,Count

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  intersect = AmesPAHdbIDLSuite_Fitted_Spectrum__Intersection((*self.database).data.species[selector].uid, *self.uids, Count)

  IF Count GT 0 THEN BEGIN

     nitag = N_ELEMENTS(property)
     
     idx = LINDGEN(nitag, Count)
 
     select = WHERE(property[idx MOD nitag].uid EQ intersect[idx / nitag]) MOD nitag

     RETURN,select
  ENDIF

  Count = 0
  
  RETURN,0
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetBreakdown,Small=Small,Flux=Flux,Absolute=Absolute

  COMPILE_OPT IDL2

  ON_ERROR,2

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
               large:0D, $
               pure:0D, $
               nitrogen:0D, $
               solo:0L, $
               duo: 0L, $
               trio: 0L, $
               quartet: 0L, $
               quintet: 0L}

  ndata = N_ELEMENTS((*self.database).data.species)

  idx = LINDGEN(ndata, self.nuids)

  select = WHERE((*self.database).data.species[idx MOD ndata].uid EQ (*self.uids)[idx / ndata]) MOD ndata

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
     
     classes = self->getClasses(Small=Small)

     ntags = N_TAGS(classes)
     
     IF NOT KEYWORD_SET(Absolute) THEN total = INT_TABULATED(*self.grid, self->getFit(), /SORT)
     
     FOR i = 0, ntags - 1 DO breakdown.(i) = INT_TABULATED(*self.grid, classes.(i), /SORT) / total
     
     RETURN, breakdown
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

  IF NOT KEYWORD_SET(Small) THEN Small = 50

  select = self->Select(WHERE((*self.database).data.species.nc LE Small), $
                        *self.weights, nselect)

  IF nselect NE 0 THEN breakdown.small = TOTAL((*self.weights)[select].weight) / total

  select = self->Select(WHERE((*self.database).data.species.nc GT Small), $
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
  
  RETURN,breakdown 
END
  
FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetClasses,Small=Small

  COMPILE_OPT IDL2

  ON_ERROR,2

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

  IF NOT KEYWORD_SET(Small) THEN Small = 50

  select = self->Select(WHERE((*self.database).data.species.nc LE Small), $
                        *self.data, nselect)

  IF nselect NE 0 THEN classes.small = TOTAL(REFORM((*self.data)[select].intensity, ngrid, nselect), 2)

  select = self->Select(WHERE((*self.database).data.species.nc GT Small), $
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
  
  RETURN,classes
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetWeights

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.weights) THEN RETURN,*self.weights
  
  RETURN,0
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetNorm

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,TOTAL(((*self.observation).data.y-self->getFit())^2, /NAN)
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetChiSquared

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.observation) THEN RETURN,-1

  RETURN,TOTAL(((*self.observation).data.y-self->getFit())^2 / (*self.observation).data.ystdev, /NAN)
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::GetMethod

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,self.method
END

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite_Fitted_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.observation) THEN PTR_FREE,self.observation

  IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights
  
  self->AmesPAHdbIDLSuite_Spectrum::Cleanup 
END

FUNCTION AmesPAHdbIDLSuite_Fitted_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method $
  ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Observation=Observation,Weights=Weights,Method=Method

  RETURN,self.state 
END
  
PRO AmesPAHdbIDLSuite_Fitted_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Fitted_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Spectrum, $
          observation:PTR_NEW(), $
          weights:PTR_NEW(), $
          method:''}
END

; END OF amespahdbidlsuite_fitted_spectrum__define.pro
