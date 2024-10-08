; docformat = 'rst'

;+
;
; Class to hold an astronomical spectrum.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create and destroy an
;   AmesPAHdbIDLSuite_Observation-instance::
;
;     IDL> obs = OBJ_NEW('AmesPAHdbIDLSuite_Observation')
;     IDL> obs->Set,data
;     IDL> obs->Plot
;     IDL> OBJ_DESTROY,obs
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
;     08-28-2024
;     Honor NOTICE-keyword in ABSCISSAUNITSTO. Christiaan Boersma.
;     06-15-2022:
;     Properly include grid end point in REBIN. Christiaan Boersma.
;     04-27-2022
;     Added NOTICE-keyword and check for no abscissa units ABSCISSAUNITSTO.
;     Christiaan Boersma.
;     04-30-2021
;     Fix parsing of ASCII-files and Accommodate IPAC tables.
;     Sort data when chaning units. Christiaan Boersma.
;     07-07-2016
;     Removed extraneous spaces after last column when writing to file
;     in WRITE. Now using TEMPORARY where appropriate. Christiaan Boersma.
;     04-21-2016
;     Added Resolution keyword to REBIN. Added message to
;     ReadFromFITSFile and ReadFromTextFile printing filename. Fixed bug
;     in Init not handeling input struct correctly. Christiaan Boersma.
;     03-18-2016
;     Fixed bug in ReadFromFile using now '->' instead of
;     '.'. Christiaan Boersma.
;     11-05-2015
;     Fixed REBIN to handle data outside original grid. Christiaan Boersma.
;     05-03-2015
;     Reading from file will now honor overriding keywords in
;     SET. Christiaan Boersma.
;     04-07-2015
;     Added SETGRIDRANGE. Christiaan Boersma.
;     01-28-2015
;     First version of the file. Christiaan Boersma.
;-

;+
;  Plot the astronomical spectrum.
;
;  :Keywords:
;    Stick: in, optional, type=int
;      Whether to plot the spectrum as sticks
;    Fill: in, optional, type=int
;       Whether to solid-fill the spectrum
;    Oplot: in, optional, type=int
;      Whether to draw over a previous plot
;    Color: in, optional, type=int
;      Color to plot the spectrum with
;    _EXTRA: in, optional, type=struct
;      Required for IDL's keyword-inheritance mechanism
;
; :Categories:
;   PLOTTING
;-
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

;+
; Write the astronomical spectrum to file as an IPAC-table.
;
; :Params:
;   Filename: in, optional, type=string
;     Output filename
;
; :Categories:
;   OUTPUT
;-
PRO AmesPAHdbIDLSuite_Observation::Write,Filename

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

  IF self.units.abscissa.str THEN $
     abscissa = STREGEX(self.units.abscissa.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT) $
  ELSE $
     abscissa = ['', 'abscissa', '']

  IF self.units.ordinate.str THEN $
     ordinate = STREGEX(self.units.ordinate.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT) $
  ELSE $
     ordinate = ['', 'ordinate', '']

  half_abscissa_len = STRLEN(abscissa[1]) / 2
  half_ordinate_len = STRLEN(ordinate[1]) / 2

  fmt1 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 +          4       ) + ',' + STRING(FORMAT='(I0)', 13 -          4       ) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 14 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 11 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 14 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 11 - half_abscissa_len) + 'X,' + $
          '"|")'

  fmt2 = '("|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|")'

  half_abscissa_len = STRLEN(abscissa[2]) / 2
  half_ordinate_len = STRLEN(ordinate[2]) / 2

  fmt3 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|")'

  cols = [STRING(FORMAT=fmt1,STRUPCASE(abscissa[1]),STRUPCASE(ordinate[1]),'CONTINUUM',STRUPCASE(ordinate[1])+'_UNC',STRUPCASE(abscissa[1])+'_UNC'), $
          STRING(FORMAT=fmt2,"double","double","double","double","double"), $
          STRING(FORMAT=fmt3,abscissa[2],ordinate[2],ordinate[2],ordinate[2],abscissa[2])]

  OPENW,funit,Filename,/GET_LUN
  PRINTF,funit,FORMAT='("\",A0)',hdr[0:WHERE(STRPOS(hdr, 'END') EQ 0)]
  PRINTF,funit,STRJOIN(cols, STRING( 10B ))

  n = N_ELEMENTS((*self.data).x)

  FOR i = 0L, n - 1L DO PRINTF,FORMAT='(X,F25.6,X,F25.6,X,F25.6,X,F25.6,X,F25.6)',funit,(*self.data)[i].x,(*self.data)[i].y,(*self.data)[i].continuum,(*self.data)[i].ystdev,(*self.data)[i].xstdev
  CLOSE,funit
  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"    WRITTEN IPAC TABLE: ", Filename
  PRINT,"========================================================="
END

;+
;  Convert abscissa units.
;
;  :Params:
;    xunit: in, optional, type=int
;      Type to convert to
;
; :Categories:
;   MANIPULATE
;-
PRO AmesPAHdbIDLSuite_Observation::AbscissaUnitsTo,xunit,Notice=Notice

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF SIZE(Notice, /TYPE) EQ 0 THEN Notice = 1

  IF self.units.abscissa.unit EQ 0 THEN BEGIN
    IF Notice THEN BEGIN
      PRINT
      PRINT,"========================================================="
      PRINT,"            NO ABSCISSA-UNITS - NOTHING DONE             "
      PRINT,"========================================================="
      PRINT
     ENDIF
     RETURN
  ENDIF

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

     PRINT,FORMAT='(I1,":",4X,A-0)',xunits

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
    IF Notice THEN BEGIN
      PRINT
      PRINT,"========================================================="
      PRINT,"  ABSCISSA-UNITS ALREADY: "+xunits[select].str+" - NOTHING DONE"
      PRINT,"========================================================="
      PRINT
     ENDIF
     RETURN
  ENDIF

  ; first convert everything to MICRON

  CASE self.units.abscissa.unit OF

     1: (*self.data).x = 1D4 / TEMPORARY((*self.data).x) ; /cm

     2: (*self.data).x = 2.9979246D14 / TEMPORARY((*self.data).x) ; Hz

     3: BREAK ; um

     4: (*self.data).x = 2.9979246D18 / TEMPORARY((*self.data).x) ; A
  ENDCASE

  ; now actual conversion

  CASE xunit OF

     1: (*self.data).x = 1D4 / TEMPORARY((*self.data).x)

     2: (*self.data).x = 2.9979246D14 / TEMPORARY((*self.data).x)

     3: BREAK

     4: (*self.data).x = 1D4 * TEMPORARY((*self.data).x)
  ENDCASE


  ; sort

  srt = SORT((*self.data).x)

  (*self.data) = (*self.data)[srt]

  ; update units

  self.units.abscissa.unit = xunit

  self.units.abscissa.str = xunits[select].str

  IF Notice THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT," CONVERTED ABSCISSA-UNITS TO: "+self.units.abscissa.str
     PRINT,"========================================================="
     PRINT
  ENDIF
END

;+
;  Rebin astronomical spectrum.
;
;  :Params:
;    x: in, optional, type="float array"
;      Either grid, grid spacing or resolution depending on the
;      set keywords
;
;  :Keywords:
;    Uniform: in, optional, type=int
;      Whether to generate a equally spaced grid
;    Resolution: in, optional, type=int
;      Whether to generate a grid with a fixed resolution
;
; :Categories:
;   MANIPULATE
;-
PRO AmesPAHdbIDLSuite_Observation::Rebin,x,Uniform=Uniform,Resolution=Resolution

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

     IF x[-1] NE max THEN BEGIN

        x = [x, max]

        nx += 1

     ENDIF

  ENDIF ELSE IF KEYWORD_SET(Resolution) THEN BEGIN

     r = DOUBLE(x)

     PRINT
     PRINT,"========================================================="
     PRINT,"    REBINNING TO RESOLUTION: R="+STRING(FORMAT='(F-0)',r)
     PRINT,"========================================================="
     PRINT

     min = MIN((*self.data).x, MAX=max)

     x = min

     nx = 1

     WHILE x[nx-1] LT max DO x = [x, x[nx-1] + (x[nx++ - 1] / r)]

     x[-1] = max

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

        idx = [TEMPORARY(idx), ++i]
     ENDWHILE

     IF N_ELEMENTS(idx) EQ 1 THEN BEGIN

        IF (*self.data)[i].x GE Data[0].x AND (*self.data)[i].x LE Data[nx-1].x THEN BEGIN

           Data[j].y = (*self.data)[idx].y

           Data[j].continuum = (*self.data)[i].continuum

           Data[j].ystdev = (*self.data)[i].ystdev

           Data[j].xstdev = (*self.data)[i].xstdev
        ENDIF
     ENDIF ELSE BEGIN

        Data[j].y = MEAN((*self.data)[idx].y)

        Data[j].continuum = MEAN((*self.data)[idx].continuum)

        Data[j].ystdev = SQRT(MEAN((*self.data)[idx].ystdev)^2)

        Data[j].xstdev = SQRT(MEAN((*self.data)[idx].xstdev)^2)
     ENDELSE
  ENDFOR

  sel = WHERE(Data.y EQ 0, nsel, COMPLEMENT=cmp)

  IF nsel GT 0 THEN BEGIN

     Data[sel].y = INTERPOL(Data[cmp].y, Data[cmp].x, Data[sel].x)

     Data[sel].continuum = INTERPOL(Data[cmp].continuum, Data[cmp].x, Data[sel].x)

     Data[sel].ystdev = SQRT(INTERPOL(Data[cmp].ystdev^2, Data[cmp].x, Data[sel].x))

     Data[sel].xstdev = SQRT(INTERPOL(Data[cmp].xstdev^2, Data[cmp].x, Data[sel].x))
  ENDIF

  PTR_FREE,self.data

  self.data = PTR_NEW(TEMPORARY(Data))
END

;+
;  Read astronomical spectrum from FITS-file.
;
;  :Keywords:
;    Units: in, optional, type=AmesPAHdb_"Observation_Units_S"
;      Units describing structure
;
; :Categories:
;   INPUT
;
; :PRIVATE:
;-
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

  self.header= PTR_NEW(TEMPORARY(header))

  telescope = STRTRIM(SXPAR(self.header, 'TELESCOP'))

  instrument = STRTRIM(SXPAR(self.header, 'INSTRUME'))

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

     PRINT
     PRINT,"========================================================="
     PRINT,"    READING ISO-SWS FITS FILE: '"+self.filename+"'"
     PRINT,"========================================================="
     PRINT

     header = HEADFITS(self.filename, /EXT)

     IF PTR_VALID(self.header) THEN PTR_FREE,self.header

     self.header= PTR_NEW(TEMPORARY(header))

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

     self.data = PTR_NEW(TEMPORARY(Data))

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
     PRINT,"    READING SPITZER-IRSX FITS FILE: '"+self.filename+"'"
     PRINT,"========================================================="
     PRINT

     PRINT
     PRINT,"========================================================="
     PRINT,"          CANNOT READ SPITZER-IRSX FITS FILES            "
     PRINT,"========================================================="
     PRINT
     self.state = 0
  ENDIF

  self.state = 1
END

;+
;  Read astronomical spectrum from ASCII-file.
;
;  :Keywords:
;    Units: in, optional, type="AmesPAHdb_Observation_Units_S"
;      Units describing structure
;
; :Categories:
;   INPUT
;
; :PRIVATE:
;-
PRO AmesPAHdbIDLSuite_Observation::ReadFromTextFile,Units=Units

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  PRINT
  PRINT,"========================================================="
  PRINT,"    READING TEXT FILE: '"+self.filename+"'"
  PRINT,"========================================================="
  PRINT

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

     IF STRMID(line, 0, 1) EQ '#' OR $
        STRMID(line, 0, 1) EQ '\' OR $
        STRMID(line, 0, 1) EQ '|' THEN CONTINUE

     strs = STRSPLIT(line, /EXTRACT, COUNT=nstrs)

     d = Data[0]

     d.x = DOUBLE(strs[0])

     IF nstrs GT 1 THEN BEGIN

        d.y = DOUBLE(strs[1])

        IF nstrs GT 2 THEN BEGIN

           d.continuum = DOUBLE(strs[2])

           IF nstrs GT 3 THEN BEGIN

              d.ystdev = DOUBLE(strs[3])

              IF nstrs GT 4 THEN BEGIN

                 d.xstdev = DOUBLE(strs[4])
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

  self.data = PTR_NEW(TEMPORARY(Data))

  self.state = 1
END

;+
;  Read astronomical spectrum from a file.
;
;  :Params:
;    Filename: required, type=string
;      File to read
;
;  :Keywords:
;    Units: in, optional, type="AmesPAHdb_Observation_Units_S"
;      Units describing structure
;
; :Categories:
;   INPUT
;
;-
PRO AmesPAHdbIDLSuite_Observation::ReadFromFile,Filename,Units=Units

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.filename = Filename

  mimetype = ''

  SPAWN,['file','-b', '--mime-type', Filename],mimetype,err,/NOSHELL

  CASE mimetype OF

     'text/plain': self->ReadFromTextFile,Units=Units

     'application/octet-stream': self->ReadFromFITSFile,Units=Units

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

;+
; Truncates the data to the given range.
;
; :Params:
;   min: required, type=flaot
;     Minimum abscissa value
;   max: required, type=float
;     Maximum abscissa value
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Observation::SetGridRange,min,max

  COMPILE_OPT IDL2

  ON_ERROR,2

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

  self.data = PTR_NEW(TEMPORARY(Data))

  PRINT
  PRINT,"========================================================="
  PRINT," GRID: (XMIN,XMAX)=(",STRTRIM(STRING(MIN((*self.data).x)),2),",",STRTRIM(STRING(MAX((*self.data).x)),2),")"
  PRINT,"========================================================="
  PRINT
END

;+
; Retrieves the abscissa values.
;
; :Returns:
;   float array
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Observation::GetGrid

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  RETURN,(*self.data).x
END

;+
; Retrieves the AmesPAHdbIDLSuite_Observation representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
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

;+
; Populates the AmesPAHdbIDLSuite_Observation-instance.
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   X: in, optional, type="float array (1D)"
;     Abscissa values
;   Y: in, optional, type="float array (1D)"
;     Ordinate values
;   ErrX: in, optional, type="float array (1D)"
;     Uncertainties associated with the abscissa values
;   ErrY: in, optional, type="float array (1D)"
;     Uncertainties associated with the ordinate values
;   Continuum: in, optional, type="float array (1D)"
;     Continuum values
;   Units: in, optional, type="AmesPAHdb_Observation_Units_S"
;     Units describing structure
;   Header: in, optional, type="string array (1D)"
;     FITS header associated with the data
;
; :Categories:
;   SET/GET
;-
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

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Struct.data.x)))

                 (*self.data).x = Struct.data.x
              ENDIF

              IF NOT KEYWORD_SET(Y) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Struct.data.y)))

                 (*self.data).y = Struct.data.y
              ENDIF

              IF NOT KEYWORD_SET(ErrX) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Struct.data.xstdev)))

                 (*self.data).xstdev = Struct.data.xstdev
              ENDIF

              IF NOT KEYWORD_SET(ErrY) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Struct.data.ystdev)))

                 (*self.data).ystdev = Struct.data.ystdev
              ENDIF

              IF NOT KEYWORD_SET(Continuum) THEN BEGIN

                 IF NOT PTR_VALID(self.data) THEN self.data = PTR_NEW(REPLICATE({AmesPAHdb_Observation_S, x:0D, y:0D, xstdev:0D, ystdev:0D, continuum:0D}, N_ELEMENTS(Struct.data.continuum)))

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

;+
; Clean-up an AmesPAHdbIDLSuite_Observation-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Observation::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup

  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  IF PTR_VALID(self.header) THEN PTR_FREE,self.header
END

;+
; Create an AmesPAHdbIDLSuite_Observation-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Observation-instance
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   X: in, optional, type="float array (1D)"
;     Abscissa values
;   Y: in, optional, type="float array (1D)"
;     Ordinate values
;   ErrX: in, optional, type="float array (1D)"
;     Uncertainties associated with the abscissa values
;   ErrY: in, optional, type="float array (1D)"
;     Uncertainties associated with the ordinate values
;   Continuum: in, optional, type="float array (1D)"
;     Continuum values
;   Units: in, optional, type="AmesPAHdb_Observation_Units_S"
;     Units describing structure
;   Header: in, optional, type="string array (1D)"
;     FITS header associated with the data
;
; :Categories:
;   CLASS
;-
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

;+
; Defines the AmesPAHdbIDLSuite_Observation Class
;
; :Fields:
;   state: type=long
;    Internal state
;   data: type=pointer
;    Data pointer
;   units: type="AmesPAHdb_Observation_Units_S"
;    Units describing structure
;   header: type=pointer
;    Header pinter
;   filename: type=string
;    Filename
;
; :Categories:
;   CLASS
;
; :Private:
;-
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
