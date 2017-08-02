;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Laboratory_Spectrum
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Class to hold the raw Ames PAH Laboratory data
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
;    lab = OBJ_NEW('AmesPAHdbIDLSuite_Laboratory_Spectrum');
;
; METHODS
;    PUBLIC:
;      PLOT (PROCEDURE)
;      WRITE (PROCEDURE)
;
;    PRIVATE:
;  
; EXAMPLE
;    lab = OBJ_NEW('AmesPAHdbIDLSuite_Laboratory_Spectrum')
;    lab->Set,data
;    lab->Plot
;    OBJ_DESTORY,lab   
;
; MODIFICATION HISTORY
;
;   04-27-2015
;   Fixed INIT to call AmesPAHdbSuite_Plot initializer. Christiaan Boersma
;   01-28-2015
;   First version of the file. Christiaan Boersma.
;-

;; OUTPUT

PRO AmesPAHdbIDLSuite_Laboratory_Spectrum::Write,Prefix

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() LT 1 THEN Prefix = OBJ_CLASS(self)+'_UID'

  self->Description,description

  description = STRJOIN(STRSPLIT(description, "!C", /EXTRACT, /REGEX), STRING(10B))
  
  FOR i = 0L, self.nuids - 1 DO BEGIN

     filename = STRING(FORMAT='(A0,I04,".txt")', Prefix, (*self.uids)[i])
     
     OPENW,funit,filename,/GET_LUN

     PRINTF,funit,OBJ_CLASS(self)
     
     PRINTF,funit,"DESCRIPTION:",description

     PRINTF,funit,"UID: "+STRING(FORMAT='(I-0)', (*self.uids)[i])

     PRINTF,funit,FORMAT='(A-30,2X,A0)', self.units.abscissa.str, self.units.ordinate.str
 
     select = WHERE((*self.data).uid EQ (*self.uids)[i], nselect)

     FOR j = 0L, nselect - 1 DO PRINTF,FORMAT='(G10.6,22X,G0)',funit,(*self.data)[select[j]].frequency,(*self.data)[select[j]].intensity

     CLOSE,funit

     FREE_LUN,funit

     PRINT
     PRINT,"========================================================="
     PRINT,"                WRITTEN: ", filename
     PRINT,"========================================================="
     PRINT
  ENDFOR
END

PRO AmesPAHdbIDLSuite_Laboratory_Spectrum::Plot,Wavelength=Wavelength,Stick=Stick,Fill=Fill,Oplot=Oplot,Color=Color,_EXTRA=EXTRA

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

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite_Laboratory_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup

  self->AmesPAHdbIDLSuite_Data::Cleanup
END

FUNCTION AmesPAHdbIDLSuite_Laboratory_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()
  
  IF N_PARAMS() GT 0 THEN self->AmesPAHdbIDLSuite_Data::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units $
  ELSE self->AmesPAHdbIDLSuite_Data::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units
 
  RETURN,self.state
END

PRO AmesPAHdbIDLSuite_Laboratory_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Laboratory_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Data, $
          INHERITS AmesPAHdbIDLSuite_Plot}
END

; END OF amespahdbidlsuite_laboratory__define.pro
