;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Spectrum
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Class to hold the convolved spectroscopic data
;
; CATEGORY:
;    DATA
;
; SUPERCLASSES:
;   AMESPAHDBIDLSUITE_DATA
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;    AMESPAHDBIDLSUITE_COADDED_SPECTRUM
;    AMESPAHDBIDLSUITE_FITTED_SPECTRUM
;    AMESPAHDBIDLSUITE_OBSERVATION
;
; OPTIONAL:
;   NONE
;
; CREATION:
;    spectrum = OBJ_NEW('AmesPAHdbIDLSuite_Spectrum')
;
; METHODS
;    PUBLIC:
;      DESCRIPTION (PROCEDURE)
;      PLOT (PROCEDURE)
;      WRITE
;      COADD (FUNCTION)
;      FIT (FUNCTION)
;      GET (FUNCTION)
;      SET (PROCEDURE)
;      GETGRID (FUNCTION)
;
;    PRIVATE:
;      NNLS
;      
; EXAMPLE
;    spectrum = OBJ_NEW('AmesPAHdbIDLSuite_Spectrum')
;    spectrum->Set,data
;    spectrum->Coadd()
;    spectrum->Plot
;    OBJ_DESTROY,spectrum
;
; MODIFICATION HISTORY
;
;   07-12-2015
;   Added method to FIT. Christiaan Boersma.
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

;; OUTPUT

PRO AmesPAHdbIDLSuite_Spectrum::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  self->AmesPAHdbIDLSuite_Data::Description,Str

  Str = [Str, STRING(FORMAT='(A-12,":",X,g-8.4,X,A-0)', "shift", self.shift, "cm!U-1!N")]
  
  Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "profile", self.profile)]

  IF SIZE(*self.fwhm, /TYPE) EQ 8 THEN BEGIN

     IF N_ELEMENTS((*self.fwhm)) GT 4 THEN fwhm = (*self.fwhm)[0:3].fwhm $
     ELSE fwhm = (*self.fwhm).fwhm
     
     fwhm = STRJOIN(STRTRIM(STRING(FORMAT='(g-7.3)', fwhm), 2), ",")

     fwhm += ",..."
        
     Str = [Str, STRING(FORMAT='(A-12,":",X,A-0,X,A-0)', "FWHM", fwhm, "cm!U-1!N")]

     Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "|_sectioned", "yes")]
     
  ENDIF ELSE Str = [Str, STRING(FORMAT='(A-12,":",X,g-8.4,X,A-0)', "FWHM", *self.fwhm, "cm!U-1!N")]

  Str = STRJOIN(Str, "!C")
  
  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

PRO AmesPAHdbIDLSuite_Spectrum::Plot,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Oplot=Oplot,Legend=Legend,Color=Color,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400
  
  x = *self.grid

  nx = N_ELEMENTS(x)
  
  xunits = self.units.abscissa.str

  xrange = [MAX(x, MIN=xmin), xmin]
  
  IF KEYWORD_SET(Wavelength) THEN BEGIN

     x = 1D4 / x

     xrange = [MIN(x, MAX=xmax), xmax]
     
     xunits = 'wavelength [!Mm!Xm]'
  ENDIF

  IF NOT KEYWORD_SET(Oplot) THEN self->AmesPAHdbIDLSuite_Plot::Plot,REFORM(REBIN(x,nx,self.nuids),self.nuids*nx),(*self.data).intensity,Color=Color,XRANGE=xrange,XTITLE=xunits,YTITLE=self.units.ordinate.str,/NoData,Stick=Stick,_EXTRA=EXTRA

  IF NOT KEYWORD_SET(Color) THEN Color = 2

  FOR i = 0L, self.nuids - 1 DO BEGIN

     select = WHERE((*self.data).uid EQ (*self.uids)[i])

     self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.data)[select].intensity,Stick=Stick,Fill=Fill,COLOR=Color+i 
  ENDFOR
 
  IF SIZE(Legend, /TYPE) EQ 0 THEN Legend = 1

  IF Legend THEN BEGIN

     self->Description,outs
          
     self->AmesPAHdbIDLSuite_Plot::Legend,outs
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::Restore
END

PRO AmesPAHdbIDLSuite_Spectrum::Write,Prefix

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() LT 1 THEN Prefix = OBJ_CLASS(self)

  n = N_ELEMENTS(*self.grid)
  
  intensities = REFORM((*self.data).intensity, n, self.nuids)

  filename = STRING(FORMAT='(A0,".txt")', Prefix)
  
  OPENW,funit,filename,/GET_LUN

  PRINTF,funit,OBJ_CLASS(self)
  
  self->Description,description

  description = STRJOIN(STRSPLIT(description, "!C", /EXTRACT, /REGEX), STRING(10B))
  
  PRINTF,funit,"DESCRIPTION:",description

  PRINTF,funit,FORMAT='(A-20,2X,'+STRING(self.nuids)+'(I-40,2X))',"UID:",*self.uids
  
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

;; FIT

PRO AmesPAHdbIDLSuite_Spectrum::NNLS,A,b,tol,max_iter

  COMPILE_OPT IDL2

  ON_ERROR, 2

  IF N_PARAMS() LT 4 THEN BEGIN
     max_iter = 100
     IF N_PARAMS() LT 3 THEN tol = 1D-16
  ENDIF

  A        = DOUBLE(A)
  b        = DOUBLE(b)
  tol      = DOUBLE(tol)
  max_iter = LONG(max_iter)

  s_struct = SIZE(A, /STRUCTURE)
  m        = s_struct.dimensions[0]
  n        = s_struct.dimensions[1]
  w        = MAKE_ARRAY(m, VALUE=-1D)
  x        = DBLARR(m)
  P        = [ ]
  Z        = INDGEN(m)

  k        = 0L

  WHILE (Z NE [ ]) AND (k LT max_iter) DO BEGIN

     IF WHERE(-w[Z] GT tol, /NULL) EQ [ ] THEN BREAK

     k += 1L
 
     tapbp = A#b
     
     IF P EQ [ ] THEN BEGIN
        w     = -tapbp
     ENDIF ELSE BEGIN
        tapap = A#TRANSPOSE(A[P, *])
        w     = -tapbp + tapap#x[P]
     ENDELSE
     
     IF WHERE(-w[Z] GT tol, /NULL) EQ [ ] THEN BREAK
   
     wi      = MAX(-w[Z], i)
     P       = [P, Z[i]]
     nz      = N_ELEMENTS(Z)
     IF nz EQ 1 THEN BEGIN
        Z = [ ]
     ENDIF ELSE BEGIN
        Z = Z[WHERE(HISTOGRAM([i], MIN=0, MAX=nz-1) EQ 0)]  
     ENDELSE
   
     WHILE 1 DO BEGIN
        
        zz    = DBLARR(m)
        zz[P] = LA_LEAST_SQUARES(A[P,*],b)
        
        IF WHERE(zz[P] LE 0, /NULL) EQ [ ] THEN BEGIN
           x = zz
           BREAK
        ENDIF
        
        P_ZN    = P[WHERE(zz[P] LE tol, /NULL)]
        alpha   = MIN(x[P_ZN]/(x[P_ZN] - zz[P_ZN]))
        x[P]    = x[P] + alpha*(zz[P]-x[P])
        temp    = WHERE(ABS(x[P]) LE tol, /NULL)
        Z       = [Z, P[temp]]
        np      = N_ELEMENTS(P)
        IF np EQ N_ELEMENTS(temp) THEN BEGIN
           P = [ ]
        ENDIF ELSE IF temp NE [ ] THEN BEGIN
           P = P[WHERE(HISTOGRAM(temp, MIN=0, MAX=np-1) EQ 0)]
        ENDIF
     ENDWHILE
  ENDWHILE
  b=x
  max_iter=k
END

FUNCTION AmesPAHdbIDLSuite_Spectrum::Fit,observation,error

  COMPILE_OPT IDL2

  ON_ERROR,2

  type = SIZE(observation, /STRUCTURE)
  
  has_error = 0

  IF type.type_name EQ 'OBJREF' THEN BEGIN

     IF OBJ_CLASS(observation) EQ 'AMESPAHDBIDLSUITE_OBSERVATION' THEN BEGIN

        observation->AbscissaUnitsTo,1
        
        observation_s = observation->get()

        IF NOT ARRAY_EQUAL(*self.grid, observation_s.data.x) THEN BEGIN
           PRINT
           PRINT,"========================================================="
           PRINT,"      DATA AND OBSERVATION GRIDS ARE NOT THE SAME        "
           PRINT,"========================================================="
           PRINT
           RETURN,OBJ_NEW()
        ENDIF
        
        IF TOTAL(observation_s.data.ystdev) GT 0 THEN has_error = 1
     ENDIF ELSE BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT," OBJECT SHOULD BE AN AMESPAHDBIDLSUITE_OBSERVATION: "+type.type_name
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,OBJ_NEW()
     ENDELSE
  ENDIF ELSE BEGIN

     IF N_PARAMS() GT 1 THEN BEGIN

        tmp = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                      X=*self.grid, $
                      Y=observation, $
                      ErrY=error)
       
        has_error = 1
     ENDIF ELSE tmp = OBJ_NEW('AmesPAHdbIDLSuite_Observation', $
                              X=*self.grid, $
                              Y=observation)

     observation_s = tmp->get()
        
     OBJ_DESTROY,tmp
  ENDELSE
     
  ny = N_ELEMENTS(observation_s.data.y)

  nparam = self.nuids

  matrix = DBLARR(nparam, ny, /NOZERO)

  FOR i = 0L, nparam - 1 DO BEGIN

    select = WHERE((*self.data).uid EQ (*self.uids)[i])

    matrix[i, *] = (*self.data)[select].intensity
  ENDFOR

  m = matrix

  b = observation_s.data.y - observation_s.data.continuum 

  weights = DBLARR(nparam, /NOZERO)
 
  IF has_error THEN BEGIN
     
     PRINT
     PRINT,"========================================================="
     PRINT,"                     DOING NNLC                          "
     PRINT,"========================================================="
     PRINT
     
     b /= observation_s.data.ystdev

     FOR i = 0L, nparam - 1 DO m[i, *] /= observation_s.data.ystdev

     method = 'NNLC'
  ENDIF ELSE BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                     DOING NNLS                          "
     PRINT,"========================================================="
     PRINT

     method = 'NNLS'
  ENDELSE

  READS,!VERSION.RELEASE,idl_version
     
  IF idl_version LT 8.0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"         FIT REQUIRES IDL VERSION 8.0 OR HIGHER: "+!VERSION.RELEASE
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN,OBJ_NEW()
  ENDIF

  self->NNLS,m,b

  weights = b

  valid = WHERE(weights GT 0, nvalid)

  IF nvalid EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                UNABLE TO FIND SOLUTION                  "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN,OBJ_NEW()
  ENDIF
  
  _weights = REPLICATE({AmesPAHdbIDLSuite_Weights_S, $
                        uid:0L, $
                        weight:0D}, nvalid)

  _weights.uid = (*self.uids)[valid]

  _weights.weight = weights[valid]

  data = REPLICATE({AmesPAHdbIDLSuite_Fitted_S, $
                    intensity:0D, $
                    uid:0L}, ny * nvalid)

  FOR i = 0, nvalid - 1 DO BEGIN

     data[i*ny:(i+1)*ny-1].uid = (*self.uids)[valid[i]]

     data[i*ny:(i+1)*ny-1].intensity = weights[valid[i]] * REFORM(matrix[valid[i], *], /OVERWRITE)
  ENDFOR

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Fitted_Spectrum', $
                 Type=self.type, $
                 Version=self.version, $
                 Data=data, $
                 PAHdb=self.database, $
                 Uids=(*self.uids)[valid], $
                 Model=*self.model, $
                 Units=self.units, $
                 Shift=self.shift, $
                 Grid=*self.grid, $
                 Profile=self.profile, $
                 FWHM=*self.fwhm, $
                 Observation=observation_s, $
                 Weights=_weights, $
                 Method=method)
END

;; COADD

FUNCTION AmesPAHdbIDLSuite_Spectrum::Coadd,Weights=weights,Average=Average
  
  COMPILE_OPT IDL2

  ON_ERROR,2

  select1 = WHERE((*self.data).uid EQ (*self.uids)[0], nselect1)

  data = REPLICATE({AmesPAHdbIDLSuite_Coadd_S, intensity:0D}, nselect1)

  data.intensity = (*self.data)[select1].intensity
  
  IF KEYWORD_SET(Weights) THEN BEGIN

     select2 = WHERE(Weights.uid EQ (*self.uids)[0], nselect2)

     IF nselect2 EQ 0 THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"          NO WEIGHT DEFINED FOR UID: "+STRING(FORMAT='(I-0)',(*self.uids)[0])
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,0
     ENDIF
     
     data.intensity *= Weights[select2].weight
  ENDIF
  
  FOR i = 1, self.nuids - 1 DO BEGIN

     select1 = WHERE((*self.data).uid EQ (*self.uids)[i])

     IF KEYWORD_SET(Weights) THEN BEGIN

        select2 = WHERE(Weights.uid EQ (*self.uids)[i], nselect2)

        IF nselect2 EQ 0 THEN BEGIN
           PRINT
           PRINT,"========================================================="
           PRINT,"          NO WEIGHT DEFINED FOR UID: "+STRING(FORMAT='(I-0)',(*self.uids)[i])
           PRINT,"========================================================="
           PRINT
           self.state = 0
           RETURN,0
        ENDIF

       data.intensity += Weights[select2].weight * (*self.data)[select1].intensity
    ENDIF ELSE data.intensity += (*self.data)[select1].intensity
  ENDFOR

  IF KEYWORD_SET(Average) THEN data.intensity /= self.nuids

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Coadded_Spectrum', $
                 Type=self.type, $
                 Version=self.version, $
                 Data=data, $
                 PAHdb=self.database, $
                 Uids=*self.uids, $
                 Model=*self.model, $
                 Units=self.units, $
                 Shift=self.shift, $
                 Grid=*self.grid, $
                 Profile=self.profile, $
                 FWHM=*self.fwhm, $
                 Weights=Weights, $
                 Averaged=KEYWORD_SET(Average))
END

;; GET/SET

FUNCTION AmesPAHdbIDLSuite_Spectrum::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0
  
  struct = self->AmesPAHdbIDLSuite_Data::Get()

  struct.type = OBJ_CLASS(self)+'_S'

  RETURN,CREATE_STRUCT(struct, 'shift', self.shift, 'grid', *self.grid, 'profile', self.profile, 'fwhm', *self.fwhm)
END

PRO AmesPAHdbIDLSuite_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN
        
        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Shift) THEN self.shift = Struct.shift
           
           IF NOT KEYWORD_SET(Grid) THEN BEGIN

              IF PTR_VALID(self.grid) THEN PTR_FREE,self.grid
           
              self.grid = PTR_NEW(Struct.grid)
           ENDIF
        
           IF NOT KEYWORD_SET(Profile) THEN self.profile = Struct.profile
        
           IF NOT KEYWORD_SET(FWHM) THEN BEGIN

              IF PTR_VALID(self.fwhm) THEN PTR_FREE,self.fwhm
           
              self.fwhm = PTR_NEW(Struct.fwhm)
           ENDIF

           s = Struct

           s.type = 'AMESPAHDBIDLSUITE_Data_S'
           
           self->AmesPAHdbIDLSuite_Data::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units          
        ENDIF
     ENDIF
  ENDIF ELSE self->AmesPAHdbIDLSuite_Data::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  IF KEYWORD_SET(Shift) THEN self.shift = Shift
  
  IF KEYWORD_SET(Grid) THEN BEGIN

     IF PTR_VALID(self.grid) THEN PTR_FREE,self.grid
        
     self.grid = PTR_NEW(Grid)
  ENDIF
     
  IF KEYWORD_SET(Profile) THEN self.profile = Profile
     
  IF KEYWORD_SET(FWHM) THEN BEGIN
        
     IF PTR_VALID(self.fwhm) THEN PTR_FREE,self.fwhm
        
     self.fwhm = PTR_NEW(FWHM)
  ENDIF
END

FUNCTION AmesPAHdbIDLSuite_Spectrum::GetGrid

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF PTR_VALID(self.grid) THEN RETURN, *self.grid

  RETURN,0
END

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup
  
  self->AmesPAHdbIDLSuite_Data::Cleanup

  IF PTR_VALID(self.grid) THEN PTR_FREE,self.grid

  IF PTR_VALID(self.fwhm) THEN PTR_FREE,self.fwhm
END

FUNCTION AmesPAHdbIDLSuite_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()

  IF self.state EQ 1 THEN BEGIN
     
     IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM $
     ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM
  ENDIF
  
  RETURN,self.state  
END

PRO AmesPAHdbIDLSuite_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Plot, $
          INHERITS AmesPAHdbIDLSuite_Data, $
          shift:0D, $
          grid:PTR_NEW(), $
          profile:'', $
          fwhm:PTR_NEW()}
END

; END OF amespahdbidlsuite_spectrum__define.pro
