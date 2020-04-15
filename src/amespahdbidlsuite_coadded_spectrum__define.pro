; docformat = 'rst'

;+
;
; Class to manage a co-added spectrrum.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create and destroy an
;   AmesPAHdbIDLSuite_CoAdded_Spectrum-instance::
;
;     IDL> coadded = OBJ_NEW('AmesPAHdbIDLSuite_Coadded_Spectrum')
;     IDL> OBJ_DESTROY,coadded
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
;     12-13-2018
;     Now writing files as IPAC-tables. Christiaan Boersma.
;     05-03-2015
;     Updated weights formatting for DESCRIPTION. Christiaan Boersma.
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
PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Spectrum::Description,Str

  Str = [Str, STRING(FORMAT='(A-12,":",X,I-0)', "n", self.nuids)]

  Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "weighted", PTR_VALID(self.weights) ? "yes" : "no")]

  IF PTR_VALID(self.weights) THEN BEGIN

     weights_str = STRTRIM(STRING(FORMAT='(g-7.2)', (*self.weights)[0].weight), 2)

     nweights = N_ELEMENTS((*self.weights))

     IF nweights GT 4 THEN weights = (*self.weights)[1:2].weight $
     ELSE weights = (*self.weights).weight

     IF nweights GT 1 THEN weights_str += STRJOIN(STRTRIM(STRING(FORMAT='(",!C|",13X,g-7.2)', weights), 2), "")

     IF nweights GT 2 THEN weights_str += STRING(FORMAT='("!C|",13X,A0)', "...")

  ENDIF ELSE weights_str = '1.0'

  Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "|_weights", weights_str)]

  Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "|_averaged", self.averaged ? "yes" : "no")]

  Str = STRJOIN(Str, "!C")

  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

;+
;  Plot the coadded spectrum.
;
;  :Keywords:
;    Wavelength: in, optional, type=int
;      Whether to set the abscissa units to wavelength
;    Stick: in, optional, type=int
;      Whether to plot the spectrum as sticks
;    Fill: in, optional, type=int
;       Whether to solid-fill the spectrum
;    Oplot: in, optional, type=int
;      Whether to over-plot the spectrum on a previous defined plot
;    Legend: in, optional, type=int, default=1B
;      Whether to show a legend
;    Color: in, optional, type=int
;      Color to plot the spectrum with
;    _EXTRA: in, optional, type=struct
;      Required for IDL's keyword-inheritance mechanism
;
; :Categories:
;   PLOTTING
;-
PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Plot,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Oplot=Oplot,Legend=Legend,Color=Color,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400

  x = *self.grid

  xunits = self.units.abscissa.str

  xrange = [MAX(x, MIN=xmin), xmin]

  IF KEYWORD_SET(Wavelength) THEN BEGIN

     x = 1D4 / x

     xrange = [MIN(x, MAX=xmax), xmax]

     xunits = 'wavelength [!Mm!Xm]'
  ENDIF

  IF NOT KEYWORD_SET(Oplot) THEN self->AmesPAHdbIDLSuite_Plot::Plot,x,(*self.data).intensity,Color=Color,XRANGE=xrange,XTITLE=xunits,YTITLE=self.units.ordinate.str,/NoData,_EXTRA=EXTRA

  IF NOT KEYWORD_SET(Color) THEN Color = 2

  self->AmesPAHdbIDLSuite_Plot::Oplot,x,(*self.data).intensity,Stick=Stick,Fill=Fill,COLOR=Color

  IF SIZE(Legend, /TYPE) EQ 0 THEN Legend = 1

  IF Legend THEN BEGIN

     self->Description,outs

     self->AmesPAHdbIDLSuite_Plot::Legend,outs
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::Restore
END

;+
; Write the co-added spectrum to file as an IPAC-table.
;
; :Params:
;   Filename: in, optional, type=string
;     Output filename
;
; :Categories:
;   OUTPUT
;-
PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Write,Filename

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

  IF PTR_VALID(self.weights) THEN BEGIN

     srt = SORT((*self.weights).uids)

     FXADDPAR,hdr,"COMMENT",STRING(FORMAT='(A-12,":",X,'+STRING(self.nuids)+'(G-10.3,","))',"WEIGHTS:",(*self.weights)[srt].weight)
  ENDIF ELSE BEGIN

     srt = SORT(*self.uids)

     FXADDPAR,hdr,"COMMENT",STRING(FORMAT='(A-12,":",X,'+STRING(self.nuids)+'(I0,","))',"UIDs",(*self.uids)[srt])
  ENDELSE

  abscissa = STREGEX(self.units.abscissa.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT)

  ordinate = STREGEX(self.units.ordinate.str, '(.*) \[(.*)\]', /SUBEXPR, /EXTRACT)

  half_abscissa_len = STRLEN(abscissa[1]) / 2
  half_ordinate_len = STRLEN(ordinate[1]) / 2

  fmt1 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|")'

  fmt2 = '("|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|")'

  half_abscissa_len = STRLEN(abscissa[2]) / 2
  half_ordinate_len = STRLEN(ordinate[2]) / 2

  fmt3 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|")'

  cols = [STRING(FORMAT=fmt1,STRUPCASE(abscissa[1]),STRUPCASE(ordinate[1])), $
          STRING(FORMAT=fmt2,"double","double"), $
          STRING(FORMAT=fmt3,abscissa[2],ordinate[2])]

  OPENW,funit,Filename,/GET_LUN
  PRINTF,funit,FORMAT='("\",A0)',hdr[0:WHERE(STRPOS(hdr, 'END') EQ 0)]
  PRINTF,funit,STRJOIN(cols, STRING( 10B ))
  n = N_ELEMENTS(*self.grid)
  FOR i = 0L, n - 1 DO PRINTF,funit,FORMAT='(X,F25.6,X,F25.6)',(*self.grid)[i],(*self.data).intensity[i]
  CLOSE,funit
  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"    WRITTEN IPAC TABLE: ", Filename
  PRINT,"========================================================="
  PRINT
END

;+
; Retrieves the AmesPAHdbIDLSuite_CoAdded_Spectrum representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Coadded_Spectrum::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  struct = self->AmesPAHdbIDLSuite_Spectrum::Get()

  struct.type = OBJ_CLASS(self)+'_S'

  struct = CREATE_STRUCT(struct, 'averaged', self.averaged)

  IF PTR_VALID(self.weights) THEN struct = CREATE_STRUCT(struct, 'weights', *self.weights)

  RETURN,struct
END

;+
; Populates the AmesPAHdbIDLSuite_CoAdded_Spectrum-instance.
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
;     Comments
;   Shift: in, optional, type=float
;     Shift
;   Grid: in, optional, type="float array"
;     Grid
;   Profile: in, optional, type=string
;     Profile
;   FWHM: in, optional, type=float
;     FWHM
;   Weights: in, optional, type=struct
;     Weights
;   Averaged: in, optional, type=int
;     Whether the spectra were averaged
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged

  IF N_PARAMS() GT 0 THEN BEGIN

     tags = TAG_NAMES(Struct)

     tag = WHERE(tags EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           tag = WHERE(tags EQ 'WEIGHTS', nweights)

           IF NOT KEYWORD_SET(Weights) AND nweights EQ 1 THEN BEGIN

              IF PTR_VALID(weights) THEN PTR_FREE,self.weights

              self.weights = PTR_NEW(Struct.(tag))
           ENDIF

           IF NOT KEYWORD_SET(Averaged) THEN self.averaged = Struct.averaged
        ENDIF

        self->AmesPAHdbIDLSuite_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM
     ENDIF
  ENDIF ELSE self->AmesPAHdbIDLSuite_Spectrum::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM

  IF KEYWORD_SET(Weights) THEN BEGIN

     IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights

     self.weights = PTR_NEW(Weights)
  ENDIF

  IF KEYWORD_SET(Averaged) THEN self.averaged = Averaged
END

;+
; Clean-up an AmesPAHdbIDLSuite_Coadded_Spectrum-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights

  self->AmesPAHdbIDLSuite_Spectrum::Cleanup
END

;+
; Create an AmesPAHdbIDLSuite_Coadded_Spectrum-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Coadded_Spectrum-instance
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
;     Comments
;   Shift: in, optional, type=float
;     Shift
;   Grid: in, optional, type="float array"
;     Grid
;   Profile: in, optional, type=string
;     Profile
;   FWHM: in, optional, type=float
;     FWHM
;   Weights: in, optional, type=struct
;     Weights
;   Averaged: in, optional, type=int
;     Whether the spectra were averaged
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged

  IF N_PARAMS() GT 0 THEN BEGIN

     tags = TAG_NAMES(Struct)

     tag = WHERE(tags EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           tag = WHERE(tags EQ 'WEIGHTS', nweights)

           IF NOT KEYWORD_SET(Weights) AND nweights EQ 1 THEN BEGIN

              IF PTR_VALID(weights) THEN PTR_FREE,self.weights

              self.weights = PTR_NEW(Struct.(tag))
           ENDIF

           IF NOT KEYWORD_SET(Averaged) THEN self.averaged = Struct.averaged
        ENDIF

        self->AmesPAHdbIDLSuite_Spectrum::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM
     ENDIF
  ENDIF ELSE self->AmesPAHdbIDLSuite_Spectrum::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM

  IF KEYWORD_SET(Weights) THEN BEGIN

     IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights

     self.weights = PTR_NEW(Weights)
  ENDIF

  IF KEYWORD_SET(Averaged) THEN self.averaged = Averaged
END

;+
; Clean-up an AmesPAHdbIDLSuite_Coadded_Spectrum-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights

  self->AmesPAHdbIDLSuite_Spectrum::Cleanup
END

;+
; Create an AmesPAHdbIDLSuite_Coadded_Spectrum-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Data-instance
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
;     Comments
;   Shift: in, optional, type=float
;     Shift
;   Grid: in, optional, type="float array"
;     Grid
;   Profile: in, optional, type=string
;     Profile
;   FWHM: in, optional, type=float
;     FWHM
;   Weights: in, optional, type=struct
;     Weights
;   Averaged: in, optional, type=int
;     Whether the spectra were averaged
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Coadded_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged $
  ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Coadded_Spectrum Class
;
; :Fields:
;   weights: type=pointer
;    Pointer to an array of weights structure
;   averaged: type=int
;     Whether the spectra were averaged
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Coadded_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Coadded_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Spectrum, $
          weights:PTR_NEW(), $
          averaged:0}
END

; END OF amespahdbidlsuite_coadded_spectrum__define.pro
