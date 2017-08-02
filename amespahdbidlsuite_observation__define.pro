;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Observations
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Class to hold observations
;
; CATEGORY:
;    DATA
;
; SUPERCLASSES:
;    NONE  
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;    NONE
;
; OPTIONAL:
;    NONE
;
; CREATION:
;   observation = OBJ_NEW('AmesPAHdbIDLSuite_Observation');
;
; METHODS
;    PUBLIC:
;      PLOT (PROCEDURE)
;      WRITE (PROCEDURE)
;      REBIN (PROCEDURE)
;      ABSCISSAUNITSTO (PROCEDURE)
;      SETGRIDRANGE (PROCEDURE)
;      GETGRID (FUNCTION)
;      GET (FUNCTION)
;      SET (PROCEDURE)
;
;    PRIVATE:
;      READFROMFILE
;      READFROMATEXTFILE
;      READFROMFITSFILE  
;
; EXAMPLE
;    observation = OBJ_NEW('AmesPAHdbIDLSuite_Observation')
;    observation->Set,data
;    observation->Plot
;    OBJ_DESTROY,observation
;
; MODIFICATION HISTORY
;
;   05-03-2015
;   Reading from file will now honor overriding keywords in
;   SET. Christiaan Boersma
;   04-07-2015
;   Added SETGRIDRANGE. Christiaan Boersma
;   01-28-2015
;   First version of the file. Christiaan Boersma.
;-

;; OUTPUT

PRO AmesPAHdbIDLSuite_Observation::Plot,Stick=Stick,Fill=Fill,Oplot=Oplot,Color=Color,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400
     
  IF NOT KEYWORD_SET(OPlot) THEN self->AmesPAHdbIDLSuite_Plot::Plot,[0,1],[0,1],Color=Color,XRANGE=[MIN((*self.data).x - (*self.data).xstdev), MAX((*self.data).x + (*self.data).xstdev)],YRANGE=[MIN((*self.data).y - (*self.data).ystdev), MAX((*self.data).y + (*self.data).ystdev)],XTITLE=self.units.abscissa.str,YTITLE=self.units.ordinate.str,/NoData,_EXTRA=EXTRA

  self->AmesPAHdbIDLSuite_Plot::OplotError,(*self.data).x,(*self.data).y,(*self.data).ystdev,(*self.data).xstdev,Stick=Stick,Fill=Fill,COLOR=14
 
  self->AmesPAHdbIDLSuite_Plot::Oplot,(*self.data).x,(*self.data).continuum,LINESTYLE=5
  
  self->AmesPAHdbIDLSuite_Plot::Oplot,(*self.data).x,(*self.data).y

  self->AmesPAHdbIDLSuite_Plot::Restore
END

PRO AmesPAHdbIDLSuite_Observation::Write,Prefix

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() LT 1 THEN Prefix = OBJ_CLASS(self)

  n = N_ELEMENTS((*self.data).x)
  
  filename = STRING(FORMAT='(A0,".txt")', Prefix)
  
  OPENW,funit,filename,/GET_LUN

  PRINTF,funit,OBJ_CLASS(self)
  
  PRINTF,funit,FORMAT='(A-20,2X,4(A-40,2X))',self.units.abscissa.str,self.units.ordinate.str,"continuum","ordinate-uncertainty", "abscissa-uncertainty"

  FOR i = 0L, n - 1 DO PRINTF,funit,FORMAT='(G12.6,10X,'+STRING(n)+'(G12.6,30X))',(*self.data)[i].x,(*self.data)[i].y,(*self.data)[i].continuum,(*self.data)[i].ystdev,(*self.data)[i].xstdev

  CLOSE,funit

  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"                WRITTEN: ",filename
  PRINT,"========================================================="
  PRINT
END

;; UNITS JUGGLING

PRO AmesPAHdbIDLSuite_Observation::AbscissaUnitsTo,xunit

  COMPILE_OPT IDL2

  ON_ERROR,2

  xunits = [{unit:1, $
             str:'frequency [cm!U-1!N]'}, $
            {unit:2, $
             str:'frequency [Hz]'}, $
            {unit:3, $
             str:'wavelength [micron]'}, $
            {unit:4, $
             str:'wavelength [A]'}]

  IF N_PARAMS() EQ 0 THEN BEGIN

     xunit = 0

     PRINT
     PRINT,"========================================================="
     PRINT,"                 PLEASE SELECT X-UNITS                   "
     PRINT,"========================================================="
     PRINT
     
     nxunits = N_ELEMENTS(xunits)
     
     FOR i = 0, nxunits - 1 DO PRINT,FORMAT='(I1,":",4X,A-0)',xunits[i].unit,xunits[i].str

     PRINT
     
     READ,xunit,PROMPT='ABSCISSA-UNITS: '
  ENDIF

  select = WHERE(xunits.unit EQ xunit, nselect)

  IF nselect EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"          INVALID ABSCISSA-UNITS: "+STRING(xunit)
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  IF self.units.abscissa.unit EQ xunit THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"  ABSCISSA-UNITS ALREADY: "+xunits[select].str+" - NOTHING DONE"
     PRINT,"========================================================="
     PRINT
     RETURN
  ENDIF

  ; first convert everything to MICRON

  CASE self.units.abscissa.unit OF

     1: (*self.data).x = 1D4 / (*self.data).x ; /cm

     2: (*self.data).x = 2.9979246D14 / (*self.data).x ; Hz

     3: BREAK ; um

     4: (*self.data).x = 2.9979246D18 / (*self.data).x ; A
  ENDCASE

  ; now actual conversion

  CASE xunit OF

     1: (*self.data).x = 1D4 / (*self.data).x

     2: (*self.data).x = 2.9979246D14 / (*self.data).x

     3: BREAK

     4: (*self.data).x = 1D4 * (*self.data).x
  ENDCASE

  
  self.units.abscissa.unit = xunit

  self.units.abscissa.str = xunits[select].str

  PRINT
  PRINT,"========================================================="
  PRINT," CONVERTED ABSCISSA-UNITS TO: "+self.units.abscissa.str
  PRINT,"========================================================="
  PRINT
END

;; REBIN

PRO AmesPAHdbIDLSuite_Observation::Rebin,x,Uniform=Uniform

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF NOT PTR_VALID(self.data) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                         NO DATA                         "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF
  
  IF KEYWORD_SET(Uniform) THEN BEGIN

     PRINT
     PRINT,"========================================================="
     PRINT,"    REBINNING TO UNIFORM GRID: DELTA="+STRING(FORMAT='(F-0)',x)
     PRINT,"========================================================="
     PRINT
     
     min = MIN((*self.data).x, MAX=max)

     nx = CEIL((max - min) / x)

     x = min + x * DINDGEN(nx)

     x = [x, max]

     nx += 1   
  ENDIF ELSE BEGIN

     nx = N_ELEMENTS(x)

     PRINT
     PRINT,"========================================================="
     PRINT,"                  REBINNING TO SET GRID                  "
     PRINT,"========================================================="
     PRINT
  ENDELSE

  srt = SORT((*self.data).x)

  (*self.data) = (*self.data)[srt]
  
  Data = REPLICATE({AmesPAHdb_Observation_S, x:0D, $
                    y:0D, $
                    xstdev:0D, $
                    ystdev:0D, $
                    continuum:0D}, nx)

  Data.x = x 
  
  n = N_ELEMENTS(*self.data)

  i = 0
  
  FOR j = 0, nx - 1 DO BEGIN

     idx = i

     WHILE i + 1 LT n - 1 DO BEGIN

        IF (*self.data)[i+1].x GT x[j] THEN BREAK

        idx = [idx, ++i]
     ENDWHILE

     IF N_ELEMENTS(idx) GT 1 THEN BEGIN

        Data[j].y = MEAN((*self.data)[idx].y)

        Data[j].continuum = MEAN((*self.data)[idx].continuum)

        Data[j].ystdev = SQRT(MEAN((*self.data)[idx].ystdev)^2)

        Data[j].xstdev = SQRT(MEAN((*self.data)[idx].xstdev)^2)
     ENDIF
  ENDFOR
  
  sel = WHERE(Data.y EQ 0, nsel, COMPLEMENT=cmp)

  IF nsel GT 0 THEN BEGIN

     Data[sel].y = INTERPOL(Data[cmp].y, Data[cmp].x, x[sel])

     Data[sel].continuum = INTERPOL(Data[cmp].continuum, Data[cmp].x, x[sel])

     Data[sel].ystdev = SQRT(INTERPOL(Data[cmp].ystdev^2, Data[cmp].x, x[sel]))

     Data[sel].xstdev = SQRT(INTERPOL(Data[cmp].xstdev^2, Data[cmp].x, x[sel]))
  ENDIF

  PTR_FREE,self.data
  
  self.data = PTR_NEW(Data)
END

;; FILE READING

PRO AmesPAHdbIDLSuite_Observation::ReadFromFITSFile,Units=Units

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF FILE_WHICH('headfits.pro', /INCLUDE_CURRENT_DIR) EQ '' THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"          ASTROLIB REQUIRED TO READ FITS FILES           "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF
     
  header = HEADFITS(self.filename)

  IF PTR_VALID(self.header) THEN PTR_FREE,self.header
 
  self.header= PTR_NEW(header)

  telescope = STRTRIM(SXPAR(header, 'TELESCOP'))

  instrument = STRTRIM(SXPAR(header, 'INSTRUME'))

  IF telescope EQ 'ISO' AND instrument EQ 'SWS' THEN BEGIN

     IF FILE_WHICH('read_faar.pro', /INCLUDE_CURRENT_DIR) EQ '' THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"       OSIA REQUIRED TO READ ISO-SWS FITS FILES          "
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     header = HEADFITS(self.filename, /EXT)

     IF PTR_VALID(self.header) THEN PTR_FREE,self.header
 
     self.header= PTR_NEW(header)

     Data = REPLICATE({AmesPAHdb_Observation_S, x:0D, $
                       y:0D, $
                       xstdev:0D, $
                       ystdev:0D, $
                       continuum:0D}, SXPAR(header, 'NAXIS2'))
          
     aar = READ_FAAR(self.filename)

     Data.x = aar.data.wave

     Data.y = aar.data.flux

     Data.ystdev = aar.data.stdev

     IF PTR_VALID(self.data) THEN PTR_FREE,self.data

     self.data = PTR_NEW(Data)

     IF NOT KEYWORD_SET(Units) THEN BEGIN

        CASE SXPAR(header, 'TUNIT1') OF

           'um      ': xunit = 3

           ELSE: xunit = 0
        ENDCASE
        
        CASE SXPAR(header, 'TUNIT2') OF

           'MJy/sr  ' : yunit = 1

           'Jy      ' : yunit = 3
           
           ELSE: yunit = 0
        ENDCASE

        self.units = AmesPAHdbIDLSuite_CREATE_OBSERVATION_UNITS_S(XUNITS=xunit, YUNITS=yunit)
        
     ENDIF ELSE self.units = Units
  ENDIF ELSE IF telescope EQ 'Spitzer' AND instrument EQ 'IRSX' THEN BEGIN

     IF FILE_WHICH('read_yaaar.pro', /INCLUDE_CURRENT_DIR) EQ '' THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"      SMART REQUIRED TO READ SPITZER-IRSX FITS FILES     "
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     PRINT
     PRINT,"========================================================="
     PRINT,"          CANNOT READ SPITZER-IRSX FITS FILES            "
     PRINT,"========================================================="
     PRINT
     self.state = 0
  ENDIF

  self.state = 1
END

PRO AmesPAHdbIDLSuite_Observation::ReadFromTextFile,Units=Units

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  OPENR,funit,self.filename,/GET_LUN

  Data = {AmesPAHdb_Observation_S, x:0D, $
          y:0D, $
          xstdev:0D, $
          ystdev:0D, $
          continuum:0D}

  line = ''

  WHILE NOT EOF(funit) DO BEGIN

     READF,funit,line
    
     IF STRLEN(line) EQ 0 THEN CONTINUE

     IF STRMID(0, 1) EQ '#' THEN CONTINUE
     
     strs = STRSPLIT(line, /EXTRACT, COUNT=nstrs)

     d = Data[0]
     
     d.x = DOUBLE(strs[0])
   
     IF nstrs GT 1 THEN BEGIN

        d.y = DOUBLE(strs[1])

        IF nstrs GT 2 THEN BEGIN

           d.continuum = DOUBLE(strs[2])

           IF nstrs GT 3 THEN BEGIN

              d.yerr = DOUBLE(strs[3])

              IF nstrs GT 4 THEN BEGIN

                 d.xerr = DOUBLE(strs[4])
              ENDIF
           ENDIF
        ENDIF
     ENDIF
     
     Data = [Data, d]
  ENDWHILE
  
  Data = Data[1:*]
  
  CLOSE,funit
  
  FREE_LUN,funit

  IF KEYWORD_SET(Units) THEN self.units = Units
  
  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  self.data = PTR_NEW(Data)

  self.state = 1
END

PRO AmesPAHdbIDLSuite_Observation::ReadFromFile,Filename,Units=Units

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.filename = Filename

  mimetype = ''

  SPAWN,['file','-b', '--mime-type', Filename],mimetype,err,/NOSHELL
  
  CASE mimetype OF

     'text/plain': self.ReadFromTextFile,Units=Units

     'application/octet-stream': self.ReadFromFITSFile,Units=Units

     ELSE: BEGIN

        PRINT
        PRINT,"========================================================="
        PRINT,"  MIME-TYPE NOT RECOGNIZED: "+Filename+" ("+mimetype+")"
        PRINT,"========================================================="
        PRINT
        self.state = 0
     END
  ENDCASE
END

;; GET/SET

PRO AmesPAHdbIDLSuite_Observation::SetGridRange,min,max

  COMPILE_OPT IDL2

  ;ON_ERROR,2
  
  IF N_PARAMS() LT 2 THEN max = MAX((*self.data).x)

  select = WHERE((*self.data).x GE min AND (*self.data).x LE max, nselect)

  IF nselect EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                  NO DATA IN RANGE                       "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  Data = REPLICATE({AmesPAHdb_Observation_S, x:0D, $
                    y:0D, $
                    xstdev:0D, $
                    ystdev:0D, $
                    continuum:0D}, nselect)
 
  Data.x = (*self.data)[select].x

  Data.y = (*self.data)[select].y

  Data.xstdev = (*self.data)[select].xstdev

  Data.ystdev = (*self.data)[select].ystdev

  Data.continuum = (*self.data)[select].continuum

  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  self.data = PTR_NEW(Data)

  PRINT
  PRINT,"========================================================="
  PRINT," GRID: (XMIN,XMAX)=(",STRTRIM(STRING(MIN((*self.data).x)),2),",",STRTRIM(STRING(MAX((*self.data).x)),2),")"
  PRINT,"========================================================="
  PRINT
END

FUNCTION AmesPAHdbIDLSuite_Observation::GetGrid

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0
                                        
  RETURN,(*self.data).x
END

FUNCTION AmesPAHdbIDLSuite_Observation::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0
                                        
  RETURN,{type:OBJ_CLASS(self)+'_S', $
          data:*self.data, $
          units:self.units, $
          header:PTR_VALID(self.header) ? *self.header : 0, $
          filename:self.filename}
END

PRO AmesPAHdbIDLSuite_Observation::Set,Struct,X=X,Y=Y,ErrX=ErrX,ErrY=ErrY,Continuum=Continuum,Units=Units,Header=Header

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN BEGIN

     IF SIZE(Struct, /TNAME)  EQ 'STRING' THEN self->ReadFromFile,Struct,Units=Units $
     ELSE BEGIN
     
        tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

        IF ntype EQ 1 THEN BEGIN
     
           IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

              IF NOT KEYWORD_SET(X) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(X)))

                 (*self.data).x = Struct.data.x      
              ENDIF

              IF NOT KEYWORD_SET(Y) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Y)))

                 (*self.data).y = Struct.data.y     
              ENDIF
     
              IF NOT KEYWORD_SET(ErrX) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(X)))
           
                 (*self.data).xstdev = Struct.data.xstdev      
              ENDIF
        
              IF NOT KEYWORD_SET(ErrY) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(ErrY)))

                 (*self.data).ystdev = Struct.data.ystdev        
              ENDIF
           
              IF NOT KEYWORD_SET(Continuum) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Continuum)))
        
                 (*self.data).continuum = Struct.data.continuum        
              ENDIF

              IF NOT KEYWORD_SET(Units) THEN self.units = Struct.units

              IF NOT KEYWORD_SET(Header) THEN BEGIN

                 IF  PTR_VALID(self.header) THEN PTR_FREE,self.header
        
                 self.header = PTR_NEW(Struct.header)
              ENDIF
           ENDIF
        ENDIF
     ENDELSE
  ENDIF
  
  IF KEYWORD_SET(X) THEN BEGIN

     IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(X)))

     (*self.data).x = X
  ENDIF

  IF KEYWORD_SET(Y) THEN BEGIN

     IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Y)))

     (*self.data).y = Y     
  ENDIF
     
  IF KEYWORD_SET(ErrX) THEN BEGIN

     IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(ErrX)))

     (*self.data).xstdev = ErrX      
  ENDIF
   
  IF KEYWORD_SET(ErrY) THEN BEGIN

     IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(ErrY)))

     (*self.data).ystdev = ErrY        
  ENDIF

  IF KEYWORD_SET(Continuum) THEN BEGIN

     IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Continuum)))

     (*self.data).continuum = Continuum        
  ENDIF

  IF KEYWORD_SET(Units) THEN self.units = Units

  IF KEYWORD_SET(Header) THEN BEGIN

     IF PTR_VALID(self.header) THEN PTR_FREE,self.header

     self.header = PTR_NEW(Header)
  ENDIF

  self.state = 1
END

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite_Observation::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup
  
  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  IF PTR_VALID(self.header) THEN PTR_FREE,self.header
END

FUNCTION AmesPAHdbIDLSuite_Observation::Init,Struct,X=X,Y=Y,ErrX=ErrX,ErrY=ErrY,Continuum=Continuum,Units=Units,Header=Header

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()

  IF self.state EQ 1 THEN BEGIN

     IF N_PARAMS() GT 0 THEN self->Set,Struct,X=X,Y=Y,ErrX=ErrX,ErrY=ErrY,Continuum=Continuum,Units=Units,Header=Header $
     ELSE self->Set,X=X,Y=Y,ErrX=ErrX,ErrY=ErrY,Continuum=Continuum,Units=Units,Header=Header
  ENDIF
  
  RETURN,self.state
END

PRO AmesPAHdbIDLSuite_Observation__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Observation, $
          INHERITS AmesPAHdbIDLSuite_Plot, $
          state:0L, $
          data:PTR_NEW(),$
          units:{AmesPAHdb_Observation_Units_S, $
                 abscissa:{AmesPAHdb_Unit_S, $
                    unit:0, $
                    str:''}, $
                 ordinate:{AmesPAHdb_Unit_S, $
                    unit:0, $
                    str:''}}, $
          header:PTR_NEW(), $
          filename:''}

END

; END OF amespahdbidlsuite_observation__define.pro
