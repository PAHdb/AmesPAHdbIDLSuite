; docformat = 'rst'

;+
;
; Class to manage a laboratory spectrum.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create and destroy an
;   AmesPAHdbIDLSuite_Laboratory_Spectrum-instance::
;
;     IDL> lab = OBJ_NEW('AmesPAHdbIDLSuite_Laboratory_Spectrum')
;     IDL> lab->Set,data
;     IDL> lab->Plot
;     IDL> OBJ_DESTROY,lab
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
;     04-27-2015
;     Fixed INIT to call AmesPAHdbSuite_Plot initializer. Christiaan Boersma
;     01-28-2015
;     First version of the file. Christiaan Boersma.
;-

;+
;  Plot the laboratory spectrum.
;
;  :Keywords:
;    Wavelength: in, optional, type=int
;      Whether to set the abscissa units to wavelength
;    Stick: in, optional, type=int
;      Whether to plot the spectrum as sticks
;    Fill: in, optional, type=int
;       Whether to solid-fill the spectrum
;    Oplot: in, optional, type=int
;      Whether to draw over a previous plot
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
PRO AmesPAHdbIDLSuite_Laboratory_Spectrum::Plot,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Oplot=Oplot,Legend=Legend,Color=Color,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,XSIZE=600,YSIZE=400

  x = (*self.data).frequency

  xunits = self.units.abscissa.str

  xrange = [MAX(x, MIN=xmin), xmin]

  IF KEYWORD_SET(Wavelength) THEN BEGIN

     x = 1D4 / x

     xrange = [MIN(x, MAX=xmax), xmax]

     xunits = 'wavelength [!Mm!Xm]'
  ENDIF

  IF NOT KEYWORD_SET(Oplot) THEN self->AmesPAHdbIDLSuite_Plot::Plot,x,(*self.data).intensity,Color=Color,XRANGE=xrange,XTITLE=xunits,YTITLE=self.units.ordinate.str,/NoData,_EXTRA=EXTRA

  IF NOT KEYWORD_SET(Color) THEN Color=2

  FOR i = 0, self.nuids - 1 DO BEGIN

     select = WHERE((*self.data).uid EQ (*self.uids)[i], nselect)

     self->AmesPAHdbIDLSuite_Plot::Oplot,x[select],(*self.data)[select].intensity,Stick=Stick,Fill=Fill,COLOR=Color+i,_EXTRA=EXTRA
  ENDFOR

  IF SIZE(Legend, /TYPE) EQ 0 THEN Legend = 1

  IF Legend THEN BEGIN

     self->Description,outs

     self->AmesPAHdbIDLSuite_Plot::Legend,outs
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::Restore
END


;+
; Write the laboratory spectrum to file as an IPAC-table.
;
; :Params:
;   Filename: in, optional, type=string
;     Output filename
;
; :Categories:
;   OUTPUT
;-
PRO AmesPAHdbIDLSuite_Laboratory_Spectrum::Write,Filename

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
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  fmt2 = '("|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + 3) + ',' + STRING(FORMAT='(I0)', 13 - 3) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  half_abscissa_len = STRLEN(abscissa[2]) / 2
  half_ordinate_len = STRLEN(ordinate[2]) / 2

  fmt3 = '("|",A' + STRING(FORMAT='(I0)', 12 + half_abscissa_len) + ',' + STRING(FORMAT='(I0)', 13 - half_abscissa_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)', 12 + half_ordinate_len) + ',' + STRING(FORMAT='(I0)', 13 - half_ordinate_len) + 'X,' + $
          '"|",A' + STRING(FORMAT='(I0)',  6 + 3) + ',' + STRING(FORMAT='(I0)',  6 - 3) + 'X,' + $
          '"|")'

  cols = [STRING(FORMAT=fmt1,STRUPCASE(abscissa[1]),STRUPCASE(ordinate[1]),'UID'), $
          STRING(FORMAT=fmt2,"double","double","int"), $
          STRING(FORMAT=fmt3,abscissa[2],ordinate[2],"")]

  OPENW,funit,Filename,/GET_LUN
  PRINTF,funit,FORMAT='("\",A0)',hdr[0:WHERE(STRPOS(hdr, 'END') EQ 0)]
  PRINTF,funit,STRJOIN(cols, STRING( 10B ))

  FOR i = 0L, self.nuids - 1L DO BEGIN
     select = WHERE((*self.data).uid EQ (*self.uids)[i], nsel)
     FOR j = 0L, nsel - 1L DO PRINTF,FORMAT='(X,F25.6,X,F25.6,X,I)',funit,(*self.data)[select[j]].frequency,(*self.data)[select[j]].intensity,(*self.uids)[i]
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
; Clean-up an AmesPAHdbIDLSuite_Laboratory_Spectrum-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Laboratory_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup

  self->AmesPAHdbIDLSuite_Data::Cleanup
END

;+
; Create an AmesPAHdbIDLSuite_Laboratory_Spectrum-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Laboratory_Spectrum-instance
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
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Laboratory_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()

  IF N_PARAMS() GT 0 THEN self->AmesPAHdbIDLSuite_Data::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units $
  ELSE self->AmesPAHdbIDLSuite_Data::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Fitted_Spectrum Class
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Laboratory_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Laboratory_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Data, $
          INHERITS AmesPAHdbIDLSuite_Plot}
END

; END OF amespahdbidlsuite_laboratory__define.pro
