;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Coadded_Spectrum
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Class to hold the coadded spectroscopic data
;
; CATEGORY:
;    DATA
;
; SUPERCLASSES:
;    AMESPAHDBIDLSUITE_DATA
;    AMESPAHDBIDLSUITE_PLOT
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
;    coadded = OBJ_NEW('AmesPAHdbIDLSuite_Coadded_Spectrum')
;
; METHODS
;    PUBLIC:
;      DESCRIPTION (PROCEDURE)
;      PLOT (PROCEDURE)
;      WRITE (PROCEDURE)
;      GET (FUNCTION)
;      SET (PROCEDURE)
;
;    PRIVATE:
;      
; EXAMPLE
;    coadded = OBJ_NEW('AmesPAHdbIDLSuite_Coadded_Spectrum')
;    coadded->Set,data
;    coadded->Plot
;    OBJ_DESTROY,coadded
;
; MODIFICATION HISTORY
;
;   05-03-2015
;   Updated weights formatting for DESCRIPTION. Christiaan Boersma.
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

;; OUTPUT

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

PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Write,Prefix

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() LT 1 THEN Prefix = OBJ_CLASS(self)
  
  intensities = (*self.data).intensity
  
  filename = STRING(FORMAT='(A0,".txt")', Prefix)
  
  OPENW,funit,filename,/GET_LUN

  PRINTF,funit,OBJ_CLASS(self)
  
  self->Description,description

  description = STRJOIN(STRSPLIT(description, "!C", /EXTRACT, /REGEX), STRING(10B))

  PRINTF,funit,"DESCRIPTION:",description

  srt = SORT(*self.uids)

  PRINTF,funit,FORMAT='(A-10,2X,'+STRING(self.nuids)+'(I-4,2X))',"UIDs:",(*self.uids)[srt]

  IF PTR_VALID(self.weights) THEN BEGIN
  
     srt = SORT((*self.weights).uids)
  
     PRINTF,funit,FORMAT='(A-10,2X,'+STRING(self.nuids)+'(G-10.3,2X))',"WEIGHTS:",(*self.weights)[srt].weight
  
  ENDIF

  PRINTF,funit,FORMAT='(A-10,2X,A-40,2X)',self.units.abscissa.str, self.units.ordinate.str

  n = N_ELEMENTS(*self.grid)
  
  FOR i = 0L, n - 1 DO PRINTF,funit,FORMAT='(G12.6,10X,G12.6,30X)',(*self.grid)[i],intensities[i]

  CLOSE,funit

  FREE_LUN,funit

  PRINT
  PRINT,"========================================================="
  PRINT,"                WRITTEN: ", filename
  PRINT,"========================================================="
  PRINT
END

;; GET/SET

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

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite_Coadded_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.weights) THEN PTR_FREE,self.weights
  
  self->AmesPAHdbIDLSuite_Spectrum::Cleanup  
END

FUNCTION AmesPAHdbIDLSuite_Coadded_Spectrum::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged $
  ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift,Grid=Grid,Profile=Profile,FWHM=FWHM,Weights=Weights,Averaged=Averaged

  RETURN,self.state  
END

PRO AmesPAHdbIDLSuite_Coadded_Spectrum__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Coadded_Spectrum, $
          INHERITS AmesPAHdbIDLSuite_Spectrum, $
          weights:PTR_NEW(), $
          averaged:0}
END

; END OF amespahdbidlsuite_coadded_spectrum__define.pro
