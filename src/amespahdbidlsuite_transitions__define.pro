; docformat = 'rst'

;+
;
; Class to manage transitions.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create, apply an emission model and destroy an
;   AmesPAHdbIDLSuite_Spectrum-instance::
;
;     IDL> transitions = OBJ_NEW('AmesPAHdbIDLSuite_Transitions')
;     IDL> transitions->Set,data,database
;     IDL> transitions->Shift,-15D
;     IDL> transitions->Cascade,4D * 1.603D-12 (4 eV in CGS UNITS)
;     IDL> transitions->Plot
;     IDL> OBJ_DESTROY,transitions
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
;     07-06-2021
;     Cleaned up progress bar. Christiaan Boersma.
;     06-02-2021
;     Add message to show when blackbody stellar model is used for
;     CASCADE. Christiaan Boersma.
;     Speed-up a number of methods by avoiding repeated linear
;     searches with WHERE and work around self.uids and self.data.uid 
;     not having the same order. Christiaan Boersma.
;     05-02-2021
;     Changed formatting strings to avoid glitch. Christiaan
;     Boersma.
;     11-09-2020
;     Fix formatting strings to properly display 4-digit UIDs.
;     Christiaan Boersma
;     01-21-2020
;     Fix equations for the Drude profiles, making sure they are in
;     frequency-space. Christiaan Boersma
;     03-26-2019
;     Correct log-file naming scheme in CASCADE_IDLBRIDGE to
;     accommodate more than 9 CPUs/threads. Christiaan
;     Boersma.
;     10-02-2018
;     Allow CASCADE to run on the spectroscopic libaries of the
;     experimental database as long as the APPROXIMATE-keyword is
;     set. Christiaan Boersma.
;     07-29-2016
;     PRINT now shows symmetry and scale for transitions. Christiaan
;     Boersma.
;     07-11-2015
;     CASCADE and CALCULATEDTEMPERATURE now print calculated effective
;     temperature as an INTEGER. Christiaan Boersma
;     05-03-2015
;     FIXEDTEMPERATURE, CALCULATEDTEMPERATURE and CASCADE now print used
;     emission model. Fixed formatting string in CASCADE. Christiaan
;     Boersma
;     04-21-2015
;     Updated CASCADE_IDLBRIDGE and
;     AMESPAHDBIDLSUITE_TRANSITIONS__IDLBRIDGE_CALLBACK progress
;     indicators to not use a fixed width counters. Christiaan Boersma
;     04-15-2015
;     Refactored CONVOLE. Christiaan Boersma
;     04-14-2015
;     Changed id to uid and xmin + xmax to DOUBLE in CONVOLVE.
;     Christiaan Boersma
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;-

;+
;  Output transitions description.
;
;  :Params:
;     Str: out, optional, type="string array"
;       Ouput to Str
;
; :Categories:
;   INFORMATIVE
;-
PRO AmesPAHdbIDLSuite_Transitions::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Data::Description,Str

  Str = [Str, STRING(FORMAT='(A-12,": ")', "shift") + STRTRIM(STRING(FORMAT='(g-8.3)', self.shift), 2) + " cm!U-1!N"]

  Str = STRJOIN(Str, "!C")

  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

;+
;  Plot the transitions.
;
;  :Keywords:
;    Wavelength: in, optional, type=int
;      Whether to set the abscissa units to wavelength
;    Stick: in, optional, type=int
;      Whether to plot the transitions as sticks
;    Oplot: in, optional, type=int
;      Whether to draw over a previous plot
;    Legend: in, optional, type=int
;      Whether to show a legend
;    Color: in, optional, type=int
;      Color to plot the transitions with
;    _EXTRA: in, optional, type=struct
;      Required for IDL's keyword-inheritance mechanism
;
; :Categories:
;   PLOTTING
;-
PRO AmesPAHdbIDLSuite_Transitions::Plot,Wavelength=Wavelength,Stick=Stick,Oplot=Oplot,Legend=Legend,Color=Color,_EXTRA=EXTRA

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

  IF SIZE(Stick, /TYPE) EQ 0 THEN Stick = 1

  IF NOT KEYWORD_SET(Oplot) THEN self->AmesPAHdbIDLSuite_Plot::Plot,x,(*self.data).intensity,Color=Color,XRANGE=xrange,XTITLE=xunits,YTITLE=self.units.ordinate.str,/NoData,_EXTRA=EXTRA

  IF NOT KEYWORD_SET(Color) THEN Color=2

  range = [-1, UNIQ((*self.data).uid)]

  FOR i = 0, self.nuids - 1 DO BEGIN

     u0 = range[i] + 1

     u1 = range[i + 1]

     select = LINDGEN(u1 - u0 + 1) + u0

     self->AmesPAHdbIDLSuite_Plot::Oplot,x[select],(*self.data)[select].intensity,COLOR=Color+i,Stick=Stick
  ENDFOR

  IF SIZE(Legend, /TYPE) EQ 0 THEN Legend = 1

  IF Legend THEN BEGIN

     self->Description,outs

     self->AmesPAHdbIDLSuite_Plot::Legend,outs
  ENDIF

  self->AmesPAHdbIDLSuite_Plot::Restore
END

;+
;  Output transtions description.
;
; :Categories:
;   INFORMATIVE
;-
PRO AmesPAHdbIDLSuite_Transitions::Print

  COMPILE_OPT IDL2

  ON_ERROR,2

  range = [-1, UNIQ((*self.data).uid)]

  FOR i = 0L, self.nuids - 1 DO BEGIN

     uid = (*self.data)[range[i+1]].uid

     PRINT
     PRINT,"========================================================="

     PRINT,OBJ_CLASS(self)

     PRINT,"UID: "+STRING(FORMAT='(I-0)', uid)

     PRINT,FORMAT='(A-20,2X,A-36,$)', self.units.abscissa.str, self.units.ordinate.str

     IF self.type EQ 'theoretical' THEN PRINT,FORMAT='(A8,2X,A0,$)','symmetry','scale'

     PRINT

     u0 = range[i] + 1

     u1 = range[i + 1]

     select = LINDGEN(u1 - u0 + 1) + u0

     nselect = N_ELEMENTS(select)

     FOR j = 0L, nselect - 1 DO BEGIN

        PRINT,FORMAT='(A-20,2X,A-36,$)',STRING(FORMAT='(G0)',(*self.data)[select[j]].frequency), STRING(FORMAT='(G0)',(*self.data)[select[j]].intensity)

        IF self.type EQ 'theoretical' THEN PRINT,FORMAT='(A-8,2X,G0, $)',(*self.data)[select[j]].symmetry,(*self.data)[select[j]].scale

        PRINT
     ENDFOR

     PRINT,"========================================================="
     PRINT
  ENDFOR
END

;+
; Write the transitions to file as an IPAC-table.
;
; :Params:
;   Filename: in, optional, type=string
;     Output filename
;
; :Categories:
;   OUTPUT
;-
PRO AmesPAHdbIDLSuite_Transitions::Write,Filename

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

  range = [-1, UNIQ((*self.data).uid)]
  FOR i = 0L, self.nuids - 1L DO BEGIN
     uid = (*self.data)[range[i+1]].uid
     u0 = range[i] + 1
     u1 = range[i + 1]
     select = LINDGEN(u1 - u0 + 1) + u0
     nselect = N_ELEMENTS(select)
     FOR j = 0L, nsel - 1L DO PRINTF,FORMAT='(X,F25.6,X,F25.6,X,I)',funit,(*self.data)[select[j]].frequency,(*self.data)[select[j]].intensity,uid
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
;  Calculates the PAH absorption cross-section per Li & Draine 2007,
;  ApJ, 657:810-837.
;
;  :Returns:
;    float array
;
;  :Params:
;    f: in, required, type="double array"
;      frequencies in wavenumber
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION AbsorptionCrosssection__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  wave = 1D4 / f & nf = N_ELEMENTS(f)

  A = REBIN([7.97D-17, 1.23D-17, 20D-21, 14D-21, 80D-24, 84D-24, 46D-24, -322D-24], 8, nf)

  W = REBIN([0.195D, 0.217D, 0.0805D, 0.20D, 0.0370D, 0.0450D, 0.0150D, 0.135D], 8, nf)

  C = REBIN([0.0722D, 0.2175D, 1.05D, 1.23D, 1.66D, 1.745D, 1.885D, 1.90D], 8, nf)

  y = 1D / (0.889D + (2.282 / SQRT(0.4D *  nc))) / wave

  wave_r2 = TRANSPOSE(REBIN([wave], nf, 2))

  wave_r6 = TRANSPOSE(REBIN([wave], nf, 6))

  crosssection = ((1D / !DPI) * ATAN((1D3 * (y - 1D)^3) / y) + 0.5D) * (3458D-20 * 10D^(-3.431D * wave) + (2D / !DPI) * TOTAL(W[0:1,*] * C[0:1,*] * A[0:1,*] / (((wave_r2 / C[0:1,*]) - (C[0:1,*] / wave_r2))^2 + W[0:1,*]^2), 1))

  IF charge EQ 0 THEN RETURN,crosssection

  RETURN, crosssection + EXP(-1D-1 / wave^2) * 1.5D-19 * 10D^(-wave) + SQRT(2D / !DPI) * TOTAL(A[2:*,*] * EXP(-2D * (wave_r6 - C[2:*,*])^2 / W[2:*,*]^2) / W[2:*,*], 1)
END

;+
;  Callback function to Calculate the absorption cross-section
;  multiplied by Planck's function.
;
;  :Returns:
;    float array
;
;  :Params:
;    f: in, required, type="double array"
;      frequencies in wavenumber
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION PlanckFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * f^3 / (EXP(1.4387751297850830401D * f / TStar) - 1D)
END

;+
;  Callback function to calculate the absorption cross-section
;  multiplied by Planck's function squared.
;
;  :Returns:
;    float array
;
;  :Params:
;    f: in, required, type="double array"
;      frequencies in wavenumber
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION PlanckSquaredFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * f^4 / (EXP(1.4387751297850830401D * f / TStar) - 1D)
END

;+
;  Callback function to calculate the number of photons using Planck's
;  function.
;
;  :Returns:
;    float array
;
;  :Params:
;    f: in, required, type="double array"
;      frequencies in wavenumber
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION PlanckNumberOfPhotonsFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * f^2 / (EXP(1.4387751297850830401D * f / TStar) - 1D)
END

;+
;  Callback function to calculate the absorption cross-section
;  multiplied by the interstellar radiation field per Mathis et
;  al. 1983, A&A, 128:212.
;
;  :Returns:
;    float array
;
;  :Params:
;    f: in, required, type="double array"
;      frequencies in wavenumber
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION ISRFFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF f GT 1.1D5 THEN RETURN,0D

  IF f GT 1D4 / 0.110D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.202D+23 / f^5.4172D

  IF f GE 1D4 / 0.134D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.366D6 / f^2D

  IF f GE 1D4 / 0.246D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.019D-2 / f^0.3322D

  T = [7500D, 4000D, 3000D, 2.73D] & W = [1D-14, 1.65D-13, 4D-13, 1D]

  RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * f^3 * TOTAL(W / (EXP(1.4387751297850830401D * f / T) - 1D))
END

;+
;  Callback function to calculate the absorption cross-section times
;  the interstellar radiation field per Mathis et al. 1983, A&A,
;  128:212.
;
;  :Returns:
;    float array
;
;  :Params:
;    f: in, required, type="double array"
;      frequencies in wavenumber
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION ISRFSquaredFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF f GT 1.1D5 THEN RETURN,0D

  IF f GT 1D4 / 0.110D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.202D+23 / f^4.4172D

  IF f GE 1D4 / 0.134D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.366D6 / f

  IF f GE 1D4 / 0.246D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.019D-2 * f^0.6678D

  T = [7500D, 4000D, 3000D, 2.73D] & W = [1D-14, 1.65D-13, 4D-13, 1D]

  RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * f^4 * TOTAL(W / (EXP(1.4387751297850830401D * f / T) - 1D))
END

;+
;  Callback function to calculate the number of photons per Mathis et
;  al. 1983, A&A, 128:212.
;
;  :Returns:
;    float array
;
;  :Params:
;    f: in, required, type="double array"
;      frequencies in wavenumber
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION ISRFNumberOfPhotonsFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF f GT 1.1D5 THEN RETURN,0D

  IF f GT 1D4 / 0.110D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.202D+23 / f^6.4172D

  IF f GE 1D4 / 0.134D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.366D6 / f^3D

  IF f GE 1D4 / 0.246D THEN RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * 1.019D-2 / f^1.3322D

  T = [7500D, 4000D, 3000D, 2.73D] & W = [1D-14, 1.65D-13, 4D-13, 1D]

  RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * f^2 * TOTAL(W / (EXP(1.4387751297850830401D * f / T) - 1D))
END

;+
;  Callback function to calculate the mean energy in erg for a given
;  blackbody temperature.
;
;  :Returns:
;    float array
;
;  :Keywords:
;    ISRF: in, optional, type=int
;      Whether to use the interstellar radiation field instead
;    StellarModel: in, optional, type=int
;      Whether to use a stellar model instead
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION MeanEnergyFunc__AmesPAHdbIDLSuite,ISRF=ISRF,StellarModel=StellarModel

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  IF KEYWORD_SET(StellarModel) THEN RETURN,1.9864456023253396D-16 * INT_TABULATED(StarModel.frequency, AbsorptionCrossSection__AmesPAHdbIDLSuite(StarModel.frequency) * StarModel.intensity) / INT_TABULATED(StarModel.frequency, AbsorptionCrosssection__AmesPAHdbIDLSuite(StarModel.frequency) * StarModel.intensity / StarModel.frequency)

  IF KEYWORD_SET(ISRF) THEN RETURN,1.9864456023253396D-16 * QROMB('ISRFFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6) / QROMB('ISRFNumberOfPhotonsFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6)

  RETURN,1.9864456023253396D-16 * QROMB('PlanckFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6) / QROMB('PlanckNumberOfPhotonsFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6)
END

;+
;  Callback function to calculate the mean energy squared in erg^2 for
;  a given blackbody temperature.
;
;  :Returns:
;    float array
;
;  :Keywords:
;    ISRF: in, optional, type=int
;      Whether to use the interstellar radiation field instead
;    StellarModel: in, optional, type=int
;      Whether to use a stellar model instead
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION MeanEnergySquaredFunc__AmesPAHdbIDLSuite,ISRF=ISRF,StellarModel=StellarModel

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  IF KEYWORD_SET(StellarModel) THEN RETURN, 3.945966130997681D-32 * INT_TABULATED(StarModel.frequency, StarModel.frequency * AbsorptionCrossSection__AmesPAHdbIDLSuite(StarModel.frequency) * StarModel.intensity) / INT_TABULATED(StarModel.frequency, AbsorptionCrosssection__AmesPAHdbIDLSuite(StarModel.frequency) * StarModel.intensity / StarModel.frequency)

  IF KEYWORD_SET(ISRF) THEN RETURN, 3.945966130997681D-32 * QROMB('ISRFSquaredFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6) / QROMB('ISRFNumberOfPhotonsFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6)

  RETURN,3.945966130997681D-32 * QROMB('PlanckSquaredFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6) / QROMB('PlanckNumberOfPhotonsFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6)
END

;+
;  Callback function to calculates the heat capacity.
;
;  :Returns:
;    double
;
;  :Params:
;    T: in, required, type=double
;      Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION HeatCapacityFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  val = 1.4387751297850830401D * frequencies / T

  ret = 1.3806505D-16 * TOTAL(EXP(-val) * (val / (1D - EXP(-val)))^2)

  RETURN,ret
END

;+
;  Callback function to alculate a PAH's temperature after absorbing a
;  given amount of energy.
;
;  :Returns:
;    double
;
;  :Params:
;    T: in, required, type=double
;      Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION AttainedTemperatureFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  RETURN,QROMB('HeatCapacityFunc__AmesPAHdbIDLSuite', 2.73D, T, K=7, EPS=1D-6) - Ein
END

;+
;  Callback function to calculate a PAH's temperature after absorbing
;  a given amount of energy using an approximation.
;
;  :Returns:
;    double
;
;  :Params:
;    T: in, required, type=double
;      Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION ApproximateAttainedTemperatureFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  RETURN,nc * (7.54267D-11 * ERF(-4.989231D + 0.41778D * ALOG(T)) + 7.542670D-11) - Ein
END

;+
;  Callback function to calculate a feature's strength covolved with a
;  given blackbody radiation field.
;
;  :Returns:
;    double
;
;  :Params:
;    f: in, required, type=double
;      frequency in wavenumbers
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION PlanckFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  Ein = 1.9864456D-16 * f

  TMax = FX_ROOT([2.73D, 2500, 5000], 'AttainedTemperatureFunc__AmesPAHdbIDLSuite', /DOUBLE, TOL=1D-5)

  RETURN,PlanckNumberOfPhotonsFunc__AmesPAHdbIDLSuite(f) * QROMB('FeatureStrengthFunc__AmesPAHdbIDLSuite', 2.73D, TMax, K=7, EPS=1D-6)
END

;+
;  Callback function to culate a feature's strength covolved with the
;  interstellar radiation field.
;
;  :Returns:
;    double
;
;  :Params:
;    f: in, required, type=double
;      frequency in wavenumbers
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION ISRFFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  Ein = 1.9864456D-16 * f

  TMax = FX_ROOT([2.73D, 2500, 5000], 'AttainedTemperatureFunc__AmesPAHdbIDLSuite', /DOUBLE, TOL=1D-5)

  RETURN,ISRFNumberOfPhotonsFunc__AmesPAHdbIDLSuite(f) * QROMB('FeatureStrengthFunc__AmesPAHdbIDLSuite', 2.73D, TMax, K=7, EPS=1D-6)
END

;+
;  Callback function to calculate a feature's strength covolved with a
;  a given stellar model.
;
;  :Returns:
;    double
;
;  :Params:
;    f: in, required, type=double
;      frequency in wavenumbers
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION StellarModelFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  Ein = 1.9864456D-16 * f

  TMax = FX_ROOT([2.73D, 2500, 5000], 'AttainedTemperatureFunc__AmesPAHdbIDLSuite', /DOUBLE, TOL=1D-5)

  RETURN,AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * INTERPOL(StarModel.intensity / StarModel.frequency, StarModel.frequency, f) * QROMB('FeatureStrengthFunc__AmesPAHdbIDLSuite', 2.73D, TMax, K=7, EPS=1D-6)
END

;+
;  Callback function to calculate a feature's strength covolved with a
;  given blackbody using an approximation.
;
;  :Returns:
;    double
;
;  :Params:
;    f: in, required, type=double
;      frequency in wavenumbers
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION PlanckApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  Ein = 1.9864456D-16 * f

  TMax = FX_ROOT([2.73D, 2500, 5000], 'AttainedTemperatureFunc__AmesPAHdbIDLSuite', /DOUBLE, TOL=1D-5)

  RETURN,PlanckNumberOfPhotonsFunc__AmesPAHdbIDLSuite(f) * QROMB('ApproximateFeatureStrengthFunc__AmesPAHdbIDLSuite', 2.73D, TMax, K=7, EPS=1D-6)
END

;+
;  Callback function to calculate a feature's strength covolved with
;  the interstellar radiation field using an approximation.
;
;  :Returns:
;    double
;
;  :Params:
;    f: in, required, type=double
;      frequency in wavenumbers
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION ISRFApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  Ein = 1.9864456D-16 * f

  TMax = FX_ROOT([2.73D, 2500, 5000], 'AttainedTemperatureFunc__AmesPAHdbIDLSuite', /DOUBLE, TOL=1D-5)

  RETURN,ISRFNumberOfPhotonsFunc__AmesPAHdbIDLSuite(f) * QROMB('ApproximateFeatureStrengthFunc__AmesPAHdbIDLSuite', 2.73D, TMax, K=7, EPS=1D-6)
END

;+
;  Callback function to calculate a feature's strength covolved with a
;  given stellar model using an approximation.
;
;  :Returns:
;    double
;
;  :Params:
;    f: in, required, type=double
;      frequency in wavenumbers
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION StellarModelApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite,f

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  Ein = 1.9864456D-16 * f

  TMax = FX_ROOT([2.73D, 2500, 5000], 'AttainedTemperatureFunc__AmesPAHdbIDLSuite', /DOUBLE, TOL=1D-5)

  RETURN, AbsorptionCrosssection__AmesPAHdbIDLSuite(f) * INTERPOL(StarModel.intensity / StarModel.frequency, StarModel.frequency, f) * QROMB('ApproximateFeatureStrengthFunc__AmesPAHdbIDLSuite', 2.73D, TMax, K=7, EPS=1D-6) / 1.9864456023253396D-16
END

;+
;  Callback function to calculate a feature's strength covolved with a
;  blackbody.
;
;  :Returns:
;    double
;
;  :Params:
;    T: in, required, type=double
;      Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION FeatureStrengthFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  val1 = 1.4387751297850830401D * frequency / T

  val2 = 1.4387751297850830401D * frequencies / T

  RETURN,(HeatCapacityFunc__AmesPAHdbIDLSuite(T) / (EXP(val1) - 1D)) * (1D / TOTAL(intensities * (frequencies)^3 / (EXP(val2) - 1D)))
END

;+
;  Callback function to calculate a feature's strength covolved with a
;  blackbody using an approximation from Bakes, Tielen & Bauschlicher,
;  ApJ, 556:501-514, 2001.
;
;  :Returns:
;    double
;
;  :Params:
;    T: in, required, type=double
;      Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION ApproximateFeatureStrengthFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  a = 0D

  b = 0D

  IF charge NE 0 THEN BEGIN

     IF T GT 1000 THEN BEGIN

        a = 4.8D-4

        b = 1.6119D
     ENDIF ELSE IF T GT 300 AND T LE 1000 THEN BEGIN

        a = 6.38D-7

        b = 2.5556D
    ENDIF ELSE IF T GT 100 AND T LE 300 THEN BEGIN

       a = 1.69D-12

       b = 4.7687D
    ENDIF ELSE IF T GT 40 AND T LE 100 THEN BEGIN

       a = 7.70D-9

       b = 2.9244D
    ENDIF ELSE IF T GT 20 AND T LE 40 THEN BEGIN

       a = 3.47D-12

       b = 5.0428D
    ENDIF ELSE IF T GT 2.7 AND T LE 20 THEN BEGIN

       a = 4.47D-19

       b = 10.3870D
    ENDIF
  ENDIF ELSE BEGIN
    IF T GT 270 THEN BEGIN

       a = 5.52D-7

       b = 2.5270D
    ENDIF ELSE IF T GT 200 AND T LE 270 THEN BEGIN

       a = 1.70D-9

       b = 3.5607D
    ENDIF ELSE IF T GT 60 AND T LE 200 THEN BEGIN

       a = 1.35D-11

       b = 4.4800D
    ENDIF ELSE IF T GT 30 AND T LE 60 THEN BEGIN

       a = 4.18D-8

       b = 2.5217D
    ENDIF ELSE IF T GT 2.7 AND T LE 30 THEN BEGIN

       a = 1.88D-16

       b = 8.1860
    ENDIF
  ENDELSE

  val = 1.4387751297850830401D * frequency / T

  IF a EQ 0D OR val GT ALOG((MACHAR(/DOUBLE)).xmax) THEN RETURN,0D

  RETURN,1D / ((EXP(val) - 1D) * a * T^b)
END

;+
;  IDL_IDLBridge callback for calculating the Cascade PAH emission
;  model.
;
;  :Params:
;    uid: in, required, type=long
;      UID
;    offset: in, required, type=long
;      Offset into shared memory  map
;
; :Categories:
;   EMISSION MODEL,IDL_IDLBridge
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Transitions__IDLBridge_Execute,uid,offset

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON IDLBridge__AmesPAHData, shmmap, data, n_data, nuids, E, doApproximate, doStar, doISRF, doConvolved, doStellarModel, i

  uids = LONG(shmmap, (1 + 2 * n_data) * 8, nuids)

  j = (WHERE(uids EQ uid))[0]

  range = [-1L, LONG(shmmap, (1 + 2 * n_data) * 8 + nuids * 4, nuids)]

  u0 = range[j] + 1

  u1 = range[j + 1]

  select = LINDGEN(u1 - u0 + 1) + u0

  nselect = N_ELEMENTS(select)

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  frequencies = shmmap[1+select]

  intensities = shmmap[1+n_data+select]

  IF doApproximate OR doStar OR doISRF THEN BEGIN

     nc = shmmap[1+2*n_data+4*nuids+offset]

     charge = shmmap[1+2*n_data+5*nuids+offset]
  ENDIF

  IF doStar OR doISRF THEN BEGIN

     IF doStar THEN Tstar = E

     Ein = MeanEnergyFunc__AmesPAHdbIDLSuite(ISRF=doISRF, StellarModel=doStellarModel)

     shmmap[1+2*n_data+3*nuids+offset] = SQRT(MeanEnergySquaredFunc__AmesPAHdbIDLSuite(ISRF=doISRF, StellarModel=doStellarModel) - Ein^2)

     IF doConvolved THEN NPhot = NumberOfPhotonsFunc__AmesPAHdbIDLSuite(ISRF=doISRF, StellarModel=doStellarModel)
  ENDIF ELSE Ein = E

  IF doApproximate THEN BEGIN

     TotalCrossSection = TOTAL(intensities)

     func1 = 'ApproximateAttainedTemperatureFunc__AmesPAHdbIDLSuite'

     func2 = 'ApproximateFeatureStrengthFunc__AmesPAHdbIDLSuite'

     IF doConvolved THEN BEGIN

        IF doISRF THEN func3 = 'ISRFApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE IF doStellarModel THEN func3 = 'StellarModelApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE func3 = 'PlanckApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite'
     ENDIF
  ENDIF ELSE BEGIN

     func1 = 'AttainedTemperatureFunc__AmesPAHdbIDLSuite'

     func2 = 'FeatureStrengthFunc__AmesPAHdbIDLSuite'

     IF doConvolved THEN BEGIN

        IF doISRF THEN func3 = 'ISRFFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE IF doStellarModel THEN func3 = 'StellarModelFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE func3 = 'PlanckFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite'
     ENDIF
  ENDELSE

  shmmap[1+2*n_data+nuids+offset] = FX_ROOT([2.73D, 2500, 5000], func1, /DOUBLE, TOL=1D-5)

  shmmap[1+2*n_data+2*nuids+offset] = Ein

  IF (doStar OR doISRF) AND doConvolved THEN BEGIN

     FOR i = 0L, nselect - 1 DO BEGIN

        IF intensities[i] EQ 0 THEN CONTINUE

        frequency = frequencies[i]

        shmmap[1+n_data+select[i]] *= QROMB(func3, 2.5D3, 1.1D5, K=7, EPS=1D-6)
     ENDFOR

     shmmap[1+n_data+select] /= Nphot
  ENDIF ELSE BEGIN

     FOR i = 0L, nselect - 1 DO BEGIN

        IF intensities[i] EQ 0 THEN CONTINUE

        frequency = frequencies[i]

        shmmap[1+n_data+select[i]] *= QROMB(func2, 2.73D, shmmap[1+2*n_data+nuids+offset], K=7, EPS=1D-6)
     ENDFOR
  ENDELSE

  IF doApproximate THEN shmmap[1+n_data+select] *= 2.48534271218563D-23 * nc / TotalCrossSection
END

;+
;  IDL_IDLBridge callback function for calculating the Cascade PAH
;  emission model.
;
;  :Returns:
;    double
;
;  :Params:
;    Status: in, required, type=int
;      Status
;    Error: in, required, type=int
;      Error code
;    ObjRef: in, required, type="object reference"
;      Calling IDL_IDLBridge-instance
;    Userdata: in, required, type="any"
;      User specfied data
;
; :Categories:
;   EMISSION MODEL,IDL_IDLBridge
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Transitions__IDLBridge_Callback,Status,Error,ObjRef,UserData

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_IDLBridge_C, uids, nuids, i, timer_start

  IF Status EQ 2 THEN BEGIN

     digits_s = STRTRIM(STRING(FIX(ALOG10(nuids)) + 1), 2)

     timer = FLOAT(nuids - i - 1L) * FLOAT(SYSTIME(/SECONDS) - timer_start) / FLOAT(i + 1L)

     IF timer LT 1.0 THEN remaining = STRING(FORMAT='(I03,"ms")', timer * 1E3) $
     ELSE IF timer LT 60.0 THEN remaining = STRING(FORMAT='(I02,"s")', timer) $
     ELSE IF timer LT 3600.0 THEN remaining = STRING(FORMAT='(I02,"m",I02,"s")', timer / 60, timer MOD 60) $
     ELSE IF timer LT 86400.0 THEN remaining = STRING(FORMAT='(I02,"h",I02,"m",I02,"s")', timer / 3600.0, (timer MOD 3600) / 60.0, (timer MOD 3600) MOD 60) $
     ELSE remaining = STRING(FORMAT='(I3,"d",I02,"h",I02,"m")', timer / 86400.0, (timer MOD 86400) / 3600.0, (timer MOD 86400) MOD 3600)

     PRINT,FORMAT='("' + STRING(13B) +'SPECIES                          :",X,I0' + digits_s + ',"/",I0' + digits_s + ',X,"~",A-10,X,"remaining",$)',i+1L,nuids,remaining

     IF i LT nuids THEN Objref->Execute,STRING(FORMAT='("AmesPAHdbIDLSuite_Transitions__IDLBridge_Execute,",I,",",I)', uids[i], i++),/NOWAIT
  ENDIF ELSE IF Status EQ 3 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                IDLBRIDGE ERROR: "+Error
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE IF Status EQ 4 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                    IDLBRIDGE ABORTED                    "
     PRINT,"========================================================="
     PRINT
  ENDIF
END

;+
;  Callback function to calculate the number of photons given a blackbody radiation field.
;
;  :Returns:
;    double
;
;  :Keywords:
;    ISRF: in, optional, type=int
;      Whether to use the interstellar radiation field
;    StellarModel: in, optional, type=int
;      Whether to use a stellar model
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION NumberOfPhotonsFunc__AmesPAHdbIDLSuite,ISRF=ISRF,StellarModel=StellarModel

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  IF KEYWORD_SET(StellarModel) THEN RETURN,INT_TABULATED(StarModel.frequency, AbsorptionCrosssection__AmesPAHdbIDLSuite(StarModel.frequency) * StarModel.intensity / StarModel.frequency)

  IF KEYWORD_SET(ISRF) THEN RETURN,QROMB('ISRFNumberOfPhotonsFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6)

  RETURN,QROMB('PlanckNumberOfPhotonsFunc__AmesPAHdbIDLSuite', 2.5D3, 1.1D5, K=7, EPS=1D-6)
END

;+
;  IDL_IDLBridge procedure for calculating the Cascade PAH emission
;  model.
;
;  :Params:
;    E: in, required, type=double
;      Excitation enery in erg
;
;  :Keywords:
;    Approximate: in, optional, type=int
;     Whether to use an approximation
;    Star: in, optional, type=int
;     Whether to use blackbody
;    ISRF: in, optional, type=int
;     Whether to use the interstellar radiation field
;    Convolved: in, optional, type=int
;     Whether to convolve with the radiation field
;    StellarModel: in, optional, type=int
;     Whether to use a stellar model
;
; :Categories:
;   EMISSION MODEL,IDL_IDLBridge
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Transitions::Cascade_IDLBridge,E,Approximate=Approximate,Star=Star,ISRF=ISRF,Convolved=Convolved,StellarModel=StellarModel

  COMPILE_OPT IDL2

  ON_ERROR,2

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  n_data = N_ELEMENTS(*self.data)

  shmmap_size = 1 + 2 * n_data + 6 * self.nuids

  IF KEYWORD_SET(StellarModel) THEN shmmap_size += 2 * N_ELEMENTS(StarModel) + 1

  n_species = N_ELEMENTS((*self.database).data.species)

  idx = ULINDGEN(n_species, self.nuids)

  u = UNIQ((*self.data).uid)

  select = WHERE((*self.database).data.species[idx MOD n_species].uid EQ ((*self.data)[u].uid)[idx / n_species]) MOD n_species

  SHMMAP,'AmesPAHdbIDLSuite_SHMMAP',shmmap_size,/DOUBLE

  shmmap = SHMVAR('AmesPAHdbIDLSuite_SHMMAP')

  IF KEYWORD_SET(StellarModel) THEN BEGIN

     shmmap[0] = TEMPORARY([DOUBLE(n_data), $
                            (*self.data).frequency, $
                            (*self.data).intensity, $
                            DOUBLE([*(self.data)[u].uid, u], 0, self.nuids), $
                            DBLARR(self.nuids), $
                            DBLARR(self.nuids), $
                            DBLARR(self.nuids), $
                            DOUBLE((*self.database).data.species[select].nc), $
                            DOUBLE((*self.database).data.species[select].charge), $
                            DOUBLE(N_ELEMENTS(StarModel)), $
                            StarModel.frequency, $
                            StarModel.intensity])

     Ein = 0
  ENDIF ELSE BEGIN

     shmmap[0] = TEMPORARY([DOUBLE(n_data), $
                            (*self.data).frequency, $
                            (*self.data).intensity, $
                            DOUBLE([(*self.data)[u].uid, u], 0, self.nuids), $
                            DBLARR(self.nuids), $
                            DBLARR(self.nuids), $
                            DBLARR(self.nuids), $
                            DOUBLE((*self.database).data.species[select].nc), $
                            DOUBLE((*self.database).data.species[select].charge)])

     IF NOT KEYWORD_SET(ISRF) THEN Ein = E $
     ELSE Ein = 0
  ENDELSE

  n_bridges = !CPU.HW_NCPU

  IF self.nuids LT n_bridges THEN n_bridges = self.nuids

  COMMON AmesPAHdbIDLSuite_IDLBridge_C, uids, nuids, i, timer_start

  uids = *self.uids

  nuids = self.nuids

  i = 0

  timer_start = SYSTIME(/SECONDS)

  IDLBridges = OBJARR(n_bridges)

  FOR i = 0, n_bridges - 1 DO BEGIN

     IDLBridges[i] = OBJ_NEW('IDL_IDLBridge', $
                             CALLBACK='AmesPAHdbIDLSuite_Transitions__IDLBridge_Callback', $
                             OUTPUT=OBJ_CLASS(self) + STRING(FORMAT='("_IDLBridge_Core",I0,".txt")',i + 1))

     IDLBridges[i]->Execute,"@" + PREF_GET('IDL_STARTUP')
  ENDFOR

  cmd = 'RESOLVE_ROUTINE,"AmesPAHdbIDLSuite_Transitions__DEFINE",/COMPILE_FULL_FILE & ' + $
        'COMMON IDLBridge__AmesPAHData, shmmap, data, n_data, nuids, E, doApproximate, doStar, doISRF, doConvolved, doStellarModel, i & ' + $
        'SHMMAP,"AmesPAHdbIDLSuite_SHMMAP",' + STRING(shmmap_size) + ',/DOUBLE & ' + $
        'shmmap = SHMVAR("AmesPAHdbIDLSuite_SHMMAP") & ' + $
        'n_data = LONG(shmmap[0]) & ' + $
        'nuids =' + STRING(nuids) + ' & ' + $
        'E = ' + STRING(Ein) + ' & ' + $
        'doApproximate =' + STRING(KEYWORD_SET(Approximate)) + ' & ' + $
        'doStar =' + STRING(KEYWORD_SET(Star)) + ' & ' + $
        'doISRF =' + STRING(KEYWORD_SET(ISRF)) + ' & ' + $
        'doConvolved =' + STRING(KEYWORD_SET(Convolved)) + ' & ' + $
        'doStellarModel =' + STRING(KEYWORD_SET(StellarModel)) + ' & ' + $
        'COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel & ' + $
        'IF (doStar OR doISRF) AND doStellarModel THEN ' + $
        'BEGIN & ' + $
        'StarModel = REPLICATE({AmesPAHdb_StellarModel_S, frequency:0D, intensity:0D}, shmmap[1+2*n_data+6*nuids]) & ' + $
        'StarModel.frequency = shmmap[1+2*n_data+6*nuids+1:1+2*n_data+6*nuids+shmmap[1+2*n_data+6*nuids]] & ' + $
        'StarModel.intensity = shmmap[1+2*n_data+6*nuids+shmmap[1+2*n_data+6*nuids]+1:*] & ' + $
        'ENDIF & ' + $
        '!EXCEPT = 0'

  FOR i = 0, n_bridges - 1 DO IDLBridges[i]->Execute,cmd + STRING(FORMAT='(" & AmesPAHdbIDLSuite_Transitions__IDLBridge_Execute,",I,",",I)', uids[i], i),/NOWAIT
  PRINT
  PRINT,"========================================================="

  digits = STRTRIM(STRING(FIX(ALOG10(nuids)) + 1), 2)

  PRINT,FORMAT='("SPECIES                          :",X,' + digits + '("-"),"/",' + digits + '("-"),$)'

  status = INTARR(n_bridges)

  WHILE 1 DO BEGIN

     FOR j = 0, n_bridges - 1 DO status[j] = IDLBridges[j]->Status()

     IF TOTAL(status, /INTEGER) EQ 0 THEN BREAK

     WAIT,0.25
  ENDWHILE

  PRINT
  PRINT,"========================================================="
  PRINT

  OBJ_DESTROY,IDLBridges

  (*self.data).intensity = shmmap[1+n_data:2*n_data] * (*self.data).frequency^3

  (*self.model).Temperature.uid = *self.uids

  (*self.model).Temperature.T = shmmap[1+2*n_data+self.nuids:2*n_data+2*self.nuids]

  (*self.model).Energy.uid = *self.uids

  (*self.model).Energy.E = shmmap[1+2*n_data+2*self.nuids:2*n_data+3*self.nuids]

  (*self.model).Energy.sigma = shmmap[1+2*n_data+3*self.nuids:2*n_data+4*self.nuids]

  SHMUNMAP,'AmesPAHdbIDLSuite_SHMMAP'
END

;+
;  Applies the Cascade emission model.
;
;  :Params:
;    E: in, required, type=double
;      Excitation energy in erg
;
;  :Keywords:
;    Approximate: in, optional, type=int
;      Whether to use an approximation
;    IDLBridge: in, optional, type=int
;      Whether to use IDL_IDLBridge multi-processing
;    Star: in, optional, type=int
;      Whether to use blackbody
;    ISRF: in, optional, type=int
;      Whether to use the interstellar radiation field
;    Convolved: in, optional, type=int
;      Whether to convolve with the given radiation field
;    StellarModel: in, optional, type=int
;      Whether to use a stellar model
;
; :Categories:
;   EMISSION MODEL
;-
PRO AmesPAHdbIDLSuite_Transitions::Cascade,E,Approximate=Approximate,IDLBridge=IDLBridge,Star=Star,ISRF=ISRF,Convolved=Convolved,StellarModel=StellarModel

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self.type NE 'theoretical' AND NOT KEYWORD_SET(Approximate) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"     THEORETICAL DATABASE REQUIRED FOR EMISSION MODEL    "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  IF (KEYWORD_SET(Star) OR KEYWORD_SET(ISRF)) AND NOT PTR_VALID(self.database) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"   VALID DATABASE POINTER NEEDED FOR USE WITH STAR/ISRF  "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  IF PTR_VALID(self.model) THEN BEGIN

     IF (*self.model).type NE 'AMESPAHDBIDLSUITE_MODEL_ZEROKELVIN_S' THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT," AN EMISSION MODEL HAS ALREADY BEEN APPLIED: "+(*self.model).type
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     PTR_FREE,self.model
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"             APPLYING CASCADE EMISSION MODEL             "
  PRINT,"========================================================="
  PRINT

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  TStar = 0D

  IF KEYWORD_SET(Star) AND KEYWORD_SET(StellarModel) THEN BEGIN

     PRINT
     PRINT,"========================================================="
     PRINT,"  STELLAR MODEL SELECTED: USING FIRST PARAMETER AS MODEL "
     PRINT,"========================================================="
     PRINT

     TStar = (4D * !DPI * INT_TABULATED(E.frequency, E.intensity) / 5.67040D-5)^(0.25)

     select = WHERE(E.frequency GE 2.5D3 AND E.frequency LE 1.1D5, nselect)

     IF nselect EQ 0 THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"    STELLAR MODEL HAS NO DATA BETWEEN 2.5E3-1.1E5 /cm    "
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     PRINT
     PRINT,"========================================================="
     PRINT,"           REBINNING STELLAR MODEL: 100 POINTS           "
     PRINT,"========================================================="
     PRINT

     StarModel = REPLICATE({AmesPAHdb_StellarModel_S, $
                            frequency:0D, $
                            intensity:0D}, 100)

     StarModel.frequency = CONGRID(E[select].frequency, 100)

     StarModel.intensity = CONGRID(E[select].intensity, 100)

     PRINT
     PRINT,"========================================================="
     PRINT," CALCULATED EFFECTIVE TEMPERATURE: "+STRING(FORMAT='(I0,$)', TStar)+" Kelvin"
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE IF KEYWORD_SET(Star) THEN BEGIN
     TStar = E
     PRINT
     PRINT,"========================================================="
     PRINT,"           BLACKBODY TEMPERATURE: "+STRING(FORMAT='(I0,$)', TStar)+" Kelvin"
     PRINT,"========================================================="
     PRINT
  END

  IF KEYWORD_SET(ISRF) AND N_PARAMS() GT 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"         ISRF SELECTED: IGNORING FIRST PARAMETER         "
     PRINT,"========================================================="
     PRINT
  ENDIF

  IF (KEYWORD_SET(ISRF) OR KEYWORD_SET(Star)) AND KEYWORD_SET(Convolved) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"         CONVOLVING WITH ENTIRE RADIATION FIELD          "
     PRINT,"========================================================="
     PRINT
  ENDIF

  self.model = PTR_NEW({type:'AMESPAHDBIDLSUITE_MODEL_CASCADE_S', $
                        Energy:REPLICATE({AmesPAHdbIDLSuite_Energy_S, $
                                          uid:0L, $
                                          E:0D, $
                                          sigma:0D}, self.nuids), $
                        Temperature:REPLICATE({AmesPAHdbIDLSuite_Temperature_S, $
                                               uid:0L, $
                                               T:0D}, self.nuids), $
                        Approximate:KEYWORD_SET(Approximate), $
                        Star:KEYWORD_SET(Star), $
                        ISRF:KEYWORD_SET(ISRF), $
                        Convolved:KEYWORD_SET(Convolved), $
                        StellarModel:KEYWORD_SET(StellarModel), $
                        TStar:TStar, $
                        description:''})

  self.units.ordinate = {AmesPAHdb_Unit_S, $
                         unit:3, $
                         str:'integrated radiant energy [erg]'}

  description = [STRING(FORMAT='(A-12,":",X,A-0)', "model", "Cascade"), $
                 STRING(FORMAT='(A-12,":",X,A-0)', "approximated", KEYWORD_SET(Approximate) ? "yes" : "no")]

  IF (*self.model).ISRF THEN description = [description, STRING(FORMAT='(A-12,":",X,A-0)', "ISRF", "yes"), $
                                            STRING(FORMAT='(A-12,":",X,A-0)', "|_convolved", (*self.model).convolved ? "yes" : "no")] $
  ELSE IF (*self.model).star THEN description = [description, STRING(FORMAT='(A-12,":",X,A-0)', "star", "yes"), $
                                                 STRING(FORMAT='(A-12,": ")', "|_Tstar")+STRING(FORMAT='(I0)', (*self.model).Tstar)+" Kelvin", $
                                                 STRING(FORMAT='(A-12,":",X,A-0)', "|_modelled", KEYWORD_SET(StellarModel) ? "yes" : "no"), $
                                                 STRING(FORMAT='(A-12,":",X,A-0)', "|_convolved", (*self.model).convolved ? "yes" : "no")] $
  ELSE description = [description, STRING(FORMAT='(A-12,": ")', "<E>")+STRTRIM(STRING(FORMAT='(g-8.2)', E / 1.6021765D-12),2)+" eV"]

  (*self.model).description = STRJOIN(description, "!C")

  IF SIZE(IDLBridge, /TYPE) EQ 0 THEN IDLBridge = 1

  IF IDLBridge AND !CPU.HW_NCPU GT 1 AND self.nuids GT 1 THEN BEGIN

     PRINT
     PRINT,"========================================================="
     PRINT,"                  USING IDL_IDLBRIDGE                    "
     PRINT,"========================================================="
     PRINT

     self->Cascade_IDLBridge,E,Approximate=Approximate,Star=Star,ISRF=ISRF,Convolved=Convolved,StellarModel=StellarModel

     RETURN
  ENDIF

  IF KEYWORD_SET(Approximate) THEN BEGIN

     PRINT
     PRINT,"========================================================="
     PRINT,"                  USING APPROXIMATION                    "
     PRINT,"========================================================="
     PRINT

     func1 = 'ApproximateAttainedTemperatureFunc__AmesPAHdbIDLSuite'

     func2 = 'ApproximateFeatureStrengthFunc__AmesPAHdbIDLSuite'

     IF KEYWORD_SET(Convolved) THEN BEGIN

        IF KEYWORD_SET(ISRF) THEN func3 = 'ISRFApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE IF KEYWORD_SET(StellarModel) THEN func3 = 'StellarModelApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE func3 = 'PlanckApproximateFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite'
     ENDIF
  ENDIF ELSE BEGIN

     func1 = 'AttainedTemperatureFunc__AmesPAHdbIDLSuite'

     func2 = 'FeatureStrengthFunc__AmesPAHdbIDLSuite'

     IF KEYWORD_SET(Convolved) THEN BEGIN

        IF KEYWORD_SET(ISRF) THEN func3 = 'ISRFFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE IF KEYWORD_SET(StellarModel) THEN func3 = 'StellarModelFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite' $
        ELSE func3 = 'PlanckFeatureStrengthConvolvedFunc__AmesPAHdbIDLSuite'
     ENDIF
  ENDELSE

  _e = !EXCEPT

  !EXCEPT = 0

  PRINT
  PRINT,"========================================================="

  range = [-1, UNIQ((*self.data).uid)]

  FOR i = 0L, self.nuids - 1 DO BEGIN

    uid = (*self.data)[range[i+1]].uid

    timer = SYSTIME(/SECONDS)

    PRINT,FORMAT='("SPECIES                          :",X,I4,"/",I4)',i+1,self.nuids
    PRINT,FORMAT='("UID                              :",X,I4)',uid

    IF KEYWORD_SET(Approximate) OR KEYWORD_SET(Star) OR KEYWORD_SET(ISRF) THEN BEGIN

       select = WHERE((*self.database).data.species.uid EQ uid)

       nc = (*self.database).data.species[select].nc

       charge = (*self.database).data.species[select].charge
    ENDIF

    IF KEYWORD_SET(Star) OR KEYWORD_SET(ISRF) THEN BEGIN

       Ein = MeanEnergyFunc__AmesPAHdbIDLSuite(ISRF=ISRF, StellarModel=StellarModel)

       (*self.model).energy[i].sigma = SQRT(MeanEnergySquaredFunc__AmesPAHdbIDLSuite(ISRF=ISRF, StellarModel=StellarModel) - Ein^2)

       IF KEYWORD_SET(Convolved) THEN NPhot = NumberOfPhotonsFunc__AmesPAHdbIDLSuite(ISRF=ISRF, StellarModel=StellarModel)
    ENDIF ELSE Ein = E

    (*self.model).energy[i].uid = uid

    (*self.model).energy[i].E = Ein

    PRINT,"MEAN ABSORBED ENERGY             : "+STRTRIM(STRING(FORMAT='(G-8.4)',(*self.model).energy[i].E / 1.6021765D-12), 2)+" +/- "+STRTRIM(STRING(FORMAT='(G8.4)', (*self.model).energy[i].sigma / 1.6021765D-12),2)+" eV"

    u0 = range[i] + 1

    u1 = range[i + 1]

    select = LINDGEN(u1 - u0 + 1) + u0

    nselect = N_ELEMENTS(select)

    IF KEYWORD_SET(Approximate) THEN totalcrosssection = TOTAL((*self.data)[select].intensity)

    frequencies = (*self.data)[select].frequency

    intensities = (*self.data)[select].intensity

    (*self.model).temperature[i].uid = uid

    (*self.model).temperature[i].T = FX_ROOT([2.73D, 2500, 5000], func1, /DOUBLE, TOL=1D-5)

    PRINT,"MAXIMUM ATTAINED TEMPERATURE     : "+STRTRIM(STRING(FORMAT='(G-8.4)', (*self.model).temperature[i].T),2)+" Kelvin"

    IF (KEYWORD_SET(Star) OR KEYWORD_SET(ISRF)) AND KEYWORD_SET(Convolved) THEN BEGIN

       FOR j = 0L, nselect - 1 DO BEGIN

          IF (*self.data)[select[j]].intensity EQ 0 THEN CONTINUE

          frequency = (*self.data)[select[j]].frequency

          (*self.data)[select[j]].intensity *= QROMB(func3, 2.5D3, 1.1D5, K=7, EPS=1D-6)
       ENDFOR

       (*self.data)[select].intensity /= NPhot
    ENDIF ELSE BEGIN

       FOR j = 0L, nselect - 1 DO BEGIN

          IF (*self.data)[select[j]].intensity EQ 0 THEN CONTINUE

          frequency = (*self.data)[select[j]].frequency
          (*self.data)[select[j]].intensity *= QROMB(func2, 2.73D, (*self.model).temperature[i].T, K=7, EPS=1D-6)
       ENDFOR
    ENDELSE

    IF NOT KEYWORD_SET(Approximate) THEN (*self.data)[select].intensity *= (*self.data)[select].frequency^3 $ ; [erg]
    ELSE (*self.data)[select].intensity *= 2.48534271218563D-23 * nc * (*self.data)[select].frequency^3 / totalcrosssection ; [erg]

    PRINT,FORMAT='("ENERGY CONSERVATION IN SPECTRUM  :",X,G-7.2)', TOTAL((*self.data)[select].intensity) / (*self.model).energy[i].E

    timer = SYSTIME(/SECONDS) - timer

    IF timer LT 1.0 THEN PRINT,FORMAT='("TIME                             :",X,I-3,X,"MILLISECONDS")', timer*1E3 $
    ELSE IF timer LT 60 THEN PRINT,FORMAT='("TIME                             :",X,I02,X,"SECONDS")', timer $
    ELSE IF timer LT 3600 THEN PRINT,FORMAT='("TIME                             :",X,I02,X,"MINUTES",X,I02,X,"SECONDS")', timer / 60.0, timer MOD 60 $
    ELSE IF timer LT 86400 THEN PRINT,'("TIME                             :",X,I02,X,"HOURS",X,I02,X,"MINUTES",X,I02,X"SECONDS")', timer / 3600.0, (timer MOD 3600) / 60.0, (timer MOD 3600) MOD 60 $
    ELSE PRINT,'("TIME                             :",X,I3,X,"DAYS",X,I02,X,"HOURS",X,I02,X,"MINUTES")', timer / 86400.0, (timer MOD 86400) / 3600.0, (timer MOD 86400) MOD 3600
 ENDFOR

  PRINT,"========================================================="
  PRINT

  !EXCEPT = _e
END

;+
;  Applies the Calculated Temperature emission model
;
;  :Params:
;    E: in, required, type=double
;     Excitation energy in erg
;
;  :Keywords:
;    Approximate: in, optional, type=int
;      Whether to use an approximation
;    Star: in, optional, type=int
;      Whether to use blackbody
;    ISRF: in, optional, type=int
;      Whether to use the interstellar radiation field
;    StellarModel: in, optional, type=int
;      Whether to use a stellar model
;
; :Categories:
;   EMISSION MODEL
;-
PRO AmesPAHdbIDLSuite_Transitions::CalculatedTemperature,E,Approximate=Approximate,Star=Star,ISRF=ISRF,StellarModel=StellarModel

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self.type NE 'theoretical' THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"     THEORETICAL DATABASE REQUIRED FOR EMISSION MODEL    "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  IF (KEYWORD_SET(Star) OR KEYWORD_SET(ISRF)) AND NOT PTR_VALID(self.database) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"   VALID DATABASE POINTER NEEDED FOR USE WITH STAR/ISRF  "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  IF PTR_VALID(self.model) THEN BEGIN

     IF (*self.model).type NE 'AMESPAHDBIDLSUITE_MODEL_ZEROKELVIN_S' THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT," AN EMISSION MODEL HAS ALREADY BEEN APPLIED: "+(*self.model).type
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     PTR_FREE,self.model
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"     APPLYING CALCULATED TEMPERATURE EMISSION MODEL      "
  PRINT,"========================================================="
  PRINT

  COMMON AmesPAHdbIDLSuite_Models_C, nc, charge, Ein, frequencies, intensities, TStar, frequency, StarModel

  TStar = 0D

  IF KEYWORD_SET(Star) AND KEYWORD_SET(StellarModel) THEN BEGIN

     PRINT
     PRINT,"========================================================="
     PRINT,"  STELLAR MODEL SELECTED: USING FIRST PARAMETER AS MODEL "
     PRINT,"========================================================="
     PRINT

     TStar = (4D * !DPI * INT_TABULATED(E.frequency, E.intensity) / 5.67040D-5)^(0.25)

     select = WHERE(E.frequency GE 2.5D3 AND E.frequency LE 1.1D5, nselect)

     IF nselect EQ 0 THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"    STELLAR MODEL HAS NO DATA BETWEEN 2.5E3-1.1E5 /cm    "
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     PRINT
     PRINT,"========================================================="
     PRINT,"           REBINNING STELLAR MODEL: 100 POINTS           "
     PRINT,"========================================================="
     PRINT

     StarModel = REPLICATE({AmesPAHdbIDL_StellarModel_S, $
                            frequency:0D, $
                            intensity:0D}, 100)

     StarModel.frequency = CONGRID(E[select].frequency, 100)

     StarModel.intensity = CONGRID(E[select].intensity, 100)

     PRINT
     PRINT,"========================================================="
     PRINT," CALCULATED EFFECTIVE TEMPERATURE: "+STRING(FORMAT='(G7.2,$)', TStar)+" Kelvin"
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE IF KEYWORD_SET(Star) THEN TStar = E

  IF KEYWORD_SET(ISRF) AND N_PARAMS() GT 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"         ISRF SELECTED: IGNORING FIRST PARAMETER         "
     PRINT,"========================================================="
     PRINT
  ENDIF

  self.model = PTR_NEW({type:'AMESPAHDBIDLSUITE_MODEL_CALCULATEDTEMPERATURE_S', $
                        Energy:REPLICATE({AmesPAHdbIDLSuite_Energy_S, $
                                          uid:0L, $
                                          E:0D, $
                                          sigma:0D}, self.nuids), $
                        Temperature:REPLICATE({AmesPAHdbIDLSuite_Temperature_S, $
                                               uid:0L, $
                                               T:0D}, self.nuids), $
                        Approximate:KEYWORD_SET(Approximate), $
                        Star:KEYWORD_SET(Star), $
                        ISRF:KEYWORD_SET(ISRF), $
                        StellarModel:KEYWORD_SET(StellarModel), $
                        TStar:TStar, $
                        description:''})

  self.units.ordinate = {AmesPAHdb_Unit_S, $
                         unit:2, $
                         str:'integrated spectral radiance [erg/s/PAH]'}

  IF KEYWORD_SET(Approximate) THEN func = 'ApproximateAttainedTemperatureFunc__AmesPAHdbIDLSuite' $
  ELSE func = 'AttainedTemperatureFunc__AmesPAHdbIDLSuite'

  _e = !EXCEPT

  !EXCEPT = 0

  PRINT
  PRINT,"========================================================="

  range = [-1, UNIQ((*self.data).uid)]

  FOR i = 0L, self.nuids - 1 DO BEGIN

    uid = (*self.data)[range[i+1]].uid

    timer = SYSTIME(/SECONDS)

    PRINT,FORMAT='("SPECIES                          :",X,I4,"/",I4)',i+1,self.nuids
    PRINT,FORMAT='("UID                              :",X,I4)',uid

    IF KEYWORD_SET(Approximate) OR KEYWORD_SET(Star) OR KEYWORD_SET(ISRF) THEN BEGIN

       select = WHERE((*self.database).data.species.uid EQ (*self.uids)[i])

       nc = (*self.database).data.species[select].nc

       charge = (*self.database).data.species[select].charge
    ENDIF

    IF KEYWORD_SET(Star) OR KEYWORD_SET(ISRF) THEN BEGIN

       Ein = MeanEnergyFunc__AmesPAHdbIDLSuite(ISRF=ISRF, StellarModel=StellarModel)

       (*self.model).energy[i].sigma = SQRT(MeanEnergySquaredFunc__AmesPAHdbIDLSuite(ISRF=ISRF, StellarModel=StellarModel) - Ein^2)
    ENDIF ELSE Ein = E

    (*self.model).energy[i].uid = uid

    (*self.model).energy[i].E = Ein

    PRINT,"MEAN ABSORBED ENERGY             : "+STRTRIM(STRING(FORMAT='(G-8.4)',(*self.model).energy[i].E / 1.6021765D-12), 2)+" +/- "+STRTRIM(STRING(FORMAT='(G8.4)', (*self.model).energy[i].sigma / 1.6021765D-12),2)+" eV"

    u0 = range[i] + 1

    u1 = range[i + 1]

    select = LINDGEN(u1 - u0 + 1) + u0

    frequencies = (*self.data)[select].frequency

    (*self.model).temperature[i].uid = uid

    (*self.model).temperature[i].T = FX_ROOT([2.73D, 2500, 5000], func, /DOUBLE, TOL=1D-5)

    PRINT,"MAXIMUM ATTAINED TEMPERATURE     : "+STRTRIM(STRING(FORMAT='(G-8.4)', (*self.model).temperature[i].T),2)+" Kelvin"

    (*self.data)[select].intensity *= 2.4853427121856266D-23 * (*self.data)[select].frequency^3 / (EXP(1.4387751297850830401D * (*self.data)[select].frequency / (*self.model).temperature[i].T) - 1D)

    timer = SYSTIME(/SECONDS) - timer

    IF timer LT 1.0 THEN PRINT,FORMAT='("TIME                             :",X,I-3,X,"MILLISECONDS")', timer*1E3 $
    ELSE IF timer LT 60 THEN PRINT,FORMAT='("TIME                             :",X,I02,X,"SECONDS")', timer $
    ELSE IF timer LT 3600 THEN PRINT,FORMAT='("TIME                             :",X,I02,X,"MINUTES",X,I02,X,"SECONDS")', timer / 60.0, timer MOD 60 $
    ELSE IF timer LT 86400 THEN PRINT,'("TIME                             :",X,I02,X,"HOURS",X,I02,X,"MINUTES",X,I02,X"SECONDS")', timer / 3600.0, (timer MOD 3600) / 60.0, (timer MOD 3600) MOD 60 $
    ELSE PRINT,'("TIME                             :",X,I3,X,"DAYS",X,I02,X,"HOURS",X,I02,X,"MINUTES")', timer / 86400.0, (timer MOD 86400) / 3600.0, (timer MOD 86400) MOD 3600
  ENDFOR

  PRINT,"========================================================="
  PRINT

  description = [STRING(FORMAT='(A-11,X,":",X,A-0)', "model", "CalculatedTemperature"), $
                 STRING(FORMAT='(A-11,X,":",X,A-0)', "approximated", KEYWORD_SET(Approximate) ? "yes" : "no")]

  IF (*self.model).ISRF THEN description = [description, STRING(FORMAT='(A-12,":",X,A-0)', "ISRF", "yes")] $
  ELSE IF (*self.model).star THEN description = [description, STRING(FORMAT='(A-12,":",X,A-0)', "star", "yes"), $
                                                 STRING(FORMAT='(A-12,": ")', "|_Tstar")+STRING(FORMAT='(I0)', (*self.model).Tstar)+" Kelvin", $
                                                 STRING(FORMAT='(A-12,":",X,A-0)', "|_modelled", KEYWORD_SET(StellarModel) ? "yes" : "no")] $
  ELSE description = [description, STRING(FORMAT='(A-12,": ")', "<E>")+STRTRIM(STRING(FORMAT='(g-8.2)', Ein / 1.6021765D-12),2)+" eV"]

  (*self.model).description = STRJOIN(description, "!C")

  !EXCEPT = _e
END

;+
;  Applies the Fixed Temperature emission model
;
;  :Params:
;    Temperature: in, required, type=double
;     Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;-
PRO AmesPAHdbIDLSuite_Transitions::FixedTemperature,Temperature

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.model) THEN BEGIN

     IF (*self.model).type NE 'AMESPAHDBIDLSUITE_MODEL_ZEROKELVIN_S' THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT," AN EMISSION MODEL HAS ALREADY BEEN APPLIED: "+(*self.model).type
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     PTR_FREE,self.model
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"         APPLYING FIXED TEMPERATURE EMISSION MODEL       "
  PRINT,"========================================================="
  PRINT

  _e = !EXCEPT

  !EXCEPT = 0

  self.units.ordinate = {AmesPAHdb_Unit_S, $
                         unit:3, $
                         str:'integrated spectral radiance [erg/s/PAH]'}

  (*self.data).intensity *= 2.4853427121856266D-23 * (*self.data).frequency^3 / (EXP(1.4387751297850830401D * (*self.data).frequency / Temperature) - 1D)

  self.model = PTR_NEW({type:'AMESPAHDBIDLSUITE_MODEL_FIXEDTEMPERATURE_S', $
                        Temperature:REPLICATE({AmesPAHdbIDLSuite_Temperature_S, $
                                               uid:0L, $
                                               T:0D}, self.nuids), $
                        description:''})

  (*self.model).Temperature.uid = *self.uids

  (*self.model).Temperature.T = Temperature

  description = [STRING(FORMAT='(A-12,":",X,A-0)', "model", "FixedTemperature") , $
                 STRING(FORMAT='(A-12,": ")', "temperature")+STRTRIM(STRING(FORMAT='(g-8.3)', Temperature),2)+" Kelvin"]


  (*self.model).description = STRJOIN(description, "!C")

  !EXCEPT = _e
END

;+
;  Shifts the band positions.
;
;  :Params:
;    Shift: in, required, type=double
;      Shift in wavenumbers
;
; :Categories:
;   EMISSION MODEL
;-
PRO AmesPAHdbIDLSuite_Transitions::Shift,Shift

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.data) THEN BEGIN

     self.shift += Shift

     (*self.data).frequency += self.shift
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT," TOTAL SHIFT:", self.shift, " /cm"
  PRINT,"========================================================="
  PRINT
END

;+
; Callback function to calculate an anharmonic Gaussian emission
; profile.
;
;  :Returns:
;    double array
;
;  :Params:
;    T: in, required, type=double
;     Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION GaussianFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  ; Pech et al. 2002, A&A, 388, 639-651

  COMMON AmesPAHdbIDLSuite_Anharmonics_C, frequencies, nu0, nu, gamma, nc, Tmax, ref_freq, ref_e, ref_shift, ref_width, ref_ifreq, ref_ie

  ;x0 = nu0 - (0.0115D + 0.000942D * nc) * T

  ;width = (11D - 0.004D * nu0) + 5D-3 * T

  i = INTERPOL(ref_ifreq, ref_freq, nu0)

  j = INTERPOL(ref_ie, ref_e, TOTAL(frequencies / (EXP(1.4387751D * frequencies / T) - 1D))) ; /cm

  x0 = nu0 - INTERPOLATE(ref_shift, i, j)

  width = INTERPOLATE(ref_width, i, j)

  IF ABS(nu - x0) GE 11D * width THEN RETURN,0D

  RETURN,nu^3 * (1D / (width * SQRT(2D * !DPI))) * EXP(-(nu - x0)^2 / (2D * width^2)) / (EXP(1.4387751297850830401D * nu / T) - 1D)
END

;+
; Callback function to calculate an anharmonic Drude emission
; profile.
;
;  :Returns:
;    double array
;
;  :Params:
;    T: in, required, type=double
;     Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION DrudeFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  ; Pech et al. 2002, A&A, 388, 639-651

  COMMON AmesPAHdbIDLSuite_Anharmonics_C, frequencies, nu0, nu, gamma, nc, Tmax, ref_freq, ref_e, ref_shift, ref_width, ref_ifreq, ref_ie

  ;x0 = nu0 - (0.0115D + 0.000942D * nc) * T

  ;width = (11D - 0.004D * nu0) + 5D-3 * T

  i = INTERPOL(ref_ifreq, ref_freq, nu0)

  j = INTERPOL(ref_ie, ref_e, TOTAL(frequencies / (EXP(1.4387751D * frequencies / T) - 1D))) ; /cm

  x0 = nu0 - INTERPOLATE(ref_shift, i, j)

  width = INTERPOLATE(ref_width, i, j)

  frac_width = width / x0

  RETURN,nu^3 * (2D / (!DPI * x0 * width)) * frac_width^2 / ((nu / x0 - x0 / nu)^2 + frac_width^2) / (EXP(1.4387751297850830401D * nu / T) - 1D)
END

;+
; Callback function to calculate an anharmonic Lorentzian emission
; profile.
;
;  :Returns:
;    double array
;
;  :Params:
;    T: in, required, type=double
;     Excitation temperature in Kelvin
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION LorentzianFunc__AmesPAHdbIDLSuite,T

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  ; Pech et al. 2002, A&A, 388, 639-651

  COMMON AmesPAHdbIDLSuite_Anharmonics_C, frequencies, nu0, nu, gamma, nc, Tmax, ref_freq, ref_e, ref_shift, ref_width, ref_ifreq, ref_ie

   ;x0 = nu0 - (0.0115D + 0.000942D * nc) * T

   ;width = (11D - 0.004D * nu0) + 5D-3 * T

  i = INTERPOL(ref_ifreq, ref_freq, nu0)

  j = INTERPOL(ref_ie, ref_e, TOTAL(frequencies / (EXP(1.4387751D * frequencies / T) - 1D))) ; /cm

  x0 = nu0 - INTERPOLATE(ref_shift, i, j)

  width = INTERPOLATE(ref_width, i, j)

  RETURN,nu^3 * width / (EXP(1.4387751297850830401D * nu / T) - 1D) / ((nu - x0)^2 + width^2)
END

;+
; Function to calculate an anharmonic Lorentzian line profile.
;
;  :Returns:
;    double array
;
;  :Params:
;    x: in, required, type=double
;     Excitation temperature in Kelvin
;    x0: in, required, type=double
;     Excitation temperature in Kelvin
;    width: in, required, type=double
;     Excitation temperature in Kelvin
;
;  :Keywords:
;    Gaussian: in, optional, type=int
;     Whether to use a Gaussian line profile
;    Drude: in, optional, type=int
;     Whether to use a Drude line profile
;    Conserve: in, optional, type=int
;     Whether to force integrated area to one
;    Anharmonics: in, optional, type=int
;     Whether to emulate anharmonics
;
; :Categories:
;   EMISSION MODEL
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Transitions::LineProfile,x,x0,width,Gaussian=Gaussian,Drude=Drude,Conserve=Conserve,Anharmonics=Anharmonics

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  IF NOT KEYWORD_SET(Anharmonics) THEN BEGIN

     IF KEYWORD_SET(Gaussian) THEN BEGIN

        profile = (1D / (width * SQRT(2D * !DPI))) * EXP(-(x - x0)^2 / (2D * width^2))

     ENDIF ELSE IF KEYWORD_SET(Drude) THEN BEGIN

        frac_width = width / x0

        profile = (2D / (width * !DPI)) * frac_width^2 / ((x / x0 - x0 / x)^2 + frac_width^2)

     ENDIF ELSE BEGIN

        profile = (width / !DPI) / ((x - x0)^2 + width^2)

     ENDELSE

     IF KEYWORD_SET(CONSERVE) THEN profile /= INT_TABULATED(x, profile) ; area should be one!

     RETURN,profile
  ENDIF

  IF KEYWORD_SET(Gaussian) THEN BEGIN

     func = 'GaussianFunc__AmesPAHdbIDLSuite'

  ENDIF ELSE IF KEYWORD_SET(Drude) THEN BEGIN

     func = 'DrudeFunc__AmesPAHdbIDLSuite'

  ENDIF ELSE BEGIN

     func = 'LorentzianFunc__AmesPAHdbIDLSuite'

  ENDELSE

  COMMON AmesPAHdbIDLSuite_Anharmonics_C, frequencies, nu0, nu, gamma, nc, Tmax, ref_freq, ref_e, ref_shift, ref_width, ref_ifreq, ref_ie

  nu0 = x0 / 0.958D

  gamma = width

  nx = N_ELEMENTS(x)

  profile = DBLARR(nx, /NOZERO)

  FOR i = 0, nx - 1 DO BEGIN

     nu = x[i]

     profile[i] = QROMB(func, 2.73D, Tmax, K=7, EPS=1D-2)

  ENDFOR

  RETURN,profile / INT_TABULATED(x, profile) ; area should be one!
END

;+
; Convolve transitions with a line profile.
;
;  :Returns:
;    AmesPAHdbIDLSuite_Spectrum-instance
;
;  :Keywords:
;    XRange: in, optional, type="double array (2D)"
;     Range in wavenumberrs
;    FWHM: in, optional, type="double or array of struct (1D)"
;     Width parameter in wavenumbers
;    NPoints: in, optional, type=int
;     Number of sample points
;    Grid: in, optional, type="double array (1D)"
;     Use given grid
;    Conserve: in, optional, type=int
;     Whether to force integrated band area to one
;    Lorentzian: in, optional, type=int
;     Whether to use Lorentzian profile
;    Gaussian: in, optional, type=int
;     Whether to use Gaussian profile
;    Drude: in, optional, type=int
;     Whether to use Drude profile
;    Anharmonics: in, optional, type=int
;     Whether to emulate anharmonics
;
; :Categories:
;   EMISSION MODEL
;-
FUNCTION AmesPAHdbIDLSuite_Transitions::Convolve,XRange=XRange,FWHM=FWHM,Npoints=NPoints,Grid=Grid,Conserve=Conserve,Lorentzian=Lorentzian,Gaussian=Gaussian,Drude=Drude,Anharmonics=Anharmonics

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF KEYWORD_SET(Conserve) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"             CONSERVING INTEGRATED INTENSITY             "
     PRINT,"========================================================="
     PRINT
  ENDIF

  IF NOT KEYWORD_SET(ANHARMONICS) AND NOT KEYWORD_SET(FWHM) THEN FWHM = 15D

  IF NOT KEYWORD_SET(GRID) THEN BEGIN

     IF KEYWORD_SET(XRANGE) THEN xmin = MIN(XRange, MAX=xmax) ELSE BEGIN

        xmin = 1D & xmax = 4000D
     ENDELSE

     IF NOT KEYWORD_SET(NPOINTS) THEN NPoints = 400L

     x = xmin + (xmax - xmin) * DINDGEN(NPoints) / DOUBLE(NPoints - 1)
  ENDIF ELSE BEGIN

     xmin = MIN(Grid, MAX=xmax)

     x = Grid

     NPoints = N_ELEMENTS(x)
  ENDELSE

  IF KEYWORD_SET(Gaussian) THEN BEGIN

     IF NOT KEYWORD_SET(ANHARMONICS) THEN BEGIN
        IF SIZE(FWHM, /TYPE) NE 8 THEN width = fwhm / 2.3548D $
        ELSE width = FWHM.fwhm / 2.3548D
     ENDIF

     clip = 3D

     profile = 'Gaussian'

     PRINT
     PRINT,"========================================================="
     PRINT,"             USING GAUSSIAN LINE PROFILES                "
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE IF KEYWORD_SET(Drude) THEN BEGIN

     IF NOT KEYWORD_SET(ANHARMONICS) THEN BEGIN
        IF SIZE(FWHM, /TYPE) NE 8 THEN width = fwhm $
        ELSE width = FWHM.fwhm
     ENDIF

     clip = 11D

     profile = 'Drude'

     PRINT
     PRINT,"========================================================="
     PRINT,"                USING DRUDE LINE PROFILES                "
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE BEGIN

     IF NOT KEYWORD_SET(ANHARMONICS) THEN BEGIN
        IF SIZE(FWHM, /TYPE) NE 8 THEN width = 0.5D * fwhm $
        ELSE width = 0.5 * FWHM.fwhm
     ENDIF

     clip = 22D

     profile = 'Lorentzian'

     PRINT
     PRINT,"========================================================="
     PRINT,"             USING LORENTZIAN LINE PROFILES              "
     PRINT,"========================================================="
     PRINT
  ENDELSE

  PRINT
  PRINT,"========================================================="
  PRINT," GRID: (XMIN,XMAX)=("+STRTRIM(STRING(FORMAT='(G-7.4)',xmin),2)+","+STRTRIM(STRING(FORMAT='(G-7.4)',xmax),2)+"); "+STRTRIM(STRING(FORMAT='(I-4)',NPoints),2)+" POINTS"
  PRINT,"========================================================="
  PRINT

  IF KEYWORD_SET(Anharmonics) THEN BEGIN

     profile += " (anharmonic)"

     IF KEYWORD_SET(FWHM) THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"          FWHM NOT SUPPORTED WITH ANHARMONICS            "
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,0
     ENDIF

     IF self.type NE 'theoretical' THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"      THEORETICAL DATABASE REQUIRED FOR ANHARMONICS      "
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN, OBJ_NEW()
     ENDIF

     IF (*self.model).type EQ 'AMESPAHDBIDLSUITE_MODEL_ZEROKELVIN_S' THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"     NO EMISSION MODEL HAS BEEN APPLIED: "+(*self.model).type
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,OBJ_NEW()
     ENDIF

     IF self.Shift NE 0 THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"       A SHIFT HAS ALREADY BEEN APPLIED: "+STRING(FORMAT='(g7.2)', self.Shift)+" /cm"
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN,OBJ_NEW()
     ENDIF

     PRINT
     PRINT,"========================================================="
     PRINT,"   ANHARMONICS IS EXPERIMENTAL - PLEASE USE WITH CARE    "
     PRINT,"========================================================="
     PRINT

     _e = !EXCEPT

     !EXCEPT = 0

     COMMON AmesPAHdbIDLSuite_Anharmonics_C, frequencies, nu0, nu, gamma, nc, Tmax, ref_freq, ref_e, ref_shift, ref_width, ref_ifreq, ref_ie

     RESTORE,'energy_surfaces.sav'
     istart = 168
     ref_freq = pos_vs_e_x[istart:*,0]
     ref_e = REFORM(tem[0,*])
     ref_shift = pos_vs_e_fit[istart:*, *]
     ref_width = wid_vs_e_fit_upp[istart:*, *]
     ref_ifreq = DINDGEN(N_ELEMENTS(ref_freq))
     ref_ie = DINDGEN(N_ELEMENTS(ref_e))
     width = 20D
     digits_s = STRTRIM(STRING(FIX(ALOG10(self.nuids)) + 1), 2)
     timer_start = SYSTIME(/SECONDS)

     PRINT
     PRINT,"========================================================="
     PRINT,FORMAT='("SPECIES                          :",X,' + digits_s + '("-"),"/",' + digits_s + '("-"),$)'
  ENDIF

  data = REPLICATE({AmesPAHdbSpectrum_S, $
                    intensity:0D, $
                    uid:0L}, self.nuids * NPoints)

  range = [-1, UNIQ((*self.data).uid)]

  IF SIZE(FWHM, /TYPE) NE 8 THEN BEGIN

     IF NOT KEYWORD_SET(Anharmonics) THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT," FWHM: "+STRTRIM(STRING(FORMAT='(G-7.4)',FWHM),2)+" /cm"
        PRINT,"========================================================="
        PRINT
     ENDIF

     FOR i = 0L, self.nuids - 1 DO BEGIN

        uid = (*self.data)[range[i+1]].uid

        u0 = range[i] + 1

        u1 = range[i + 1]

        select = LINDGEN(u1 - u0 + 1) + u0

        IF KEYWORD_SET(Anharmonics) THEN BEGIN

           tsel = WHERE((*self.model).temperature.uid EQ uid, nselect)

           IF nselect EQ 0 THEN BEGIN
              PRINT
              PRINT,"========================================================="
              PRINT,"    TEMPERATURE NOT FOUND FOR SPECIES: UID="+STRTRIM(STRING(uid),2)
              PRINT,"========================================================="
              PRINT
              self.state = 0
              RETURN,OBJ_NEW()
           ENDIF

           Tmax = (*self.model).temperature[tsel].T

           ;;ssel = WHERE((*self.database).data.species.uid EQ uid)

           ;;csel = DOUBLE((*self.database).data.species[select].nc)

           frequencies = (*self.data)[select].frequency
        ENDIF

        data[i*NPoints:(i + 1)*NPoints-1].uid = uid

        fsel = WHERE((*self.data)[select].frequency GE xmin - clip * width AND (*self.data)[select].frequency LE xmax + clip * width, ndata)

        FOR j = 0L, ndata - 1 DO BEGIN

           IF (*self.data)[select[fsel[j]]].intensity GT 0 THEN data[i*NPoints:(i + 1)*NPoints-1].intensity += (*self.data)[select[fsel[j]]].intensity * self->LineProfile(x, (*self.data)[select[fsel[j]]].frequency, width, Gaussian=Gaussian, Drude=Drude, Conserve=Conserve, Anharmonics=Anharmonics)
        ENDFOR

        IF KEYWORD_SET(Anharmonics) THEN BEGIN

           timer = FLOAT(self.nuids - i - 1L) * FLOAT(SYSTIME(/SECONDS) - timer_start) / FLOAT(i + 1L)

           IF timer LT 1.0 THEN remaining = STRING(FORMAT='(I03,"ms")', timer * 1E3) $
           ELSE IF timer LT 60.0 THEN remaining = STRING(FORMAT='(I02,"s")', timer) $
           ELSE IF timer LT 3600.0 THEN remaining = STRING(FORMAT='(I02,"m",I02,"s")', timer / 60, timer MOD 60) $
           ELSE IF timer LT 86400.0 THEN remaining = STRING(FORMAT='(I02,"h",I02,"m",I02,"s")', timer / 3600.0, (timer MOD 3600) / 60.0, (timer MOD 3600) MOD 60) $
           ELSE remaining = STRING(FORMAT='(I3,"d",I02,"h",I02,"m")', timer / 86400.0, (timer MOD 86400) / 3600.0, (timer MOD 86400) MOD 3600)

           PRINT,FORMAT='("' + STRING(13B) +'SPECIES                          :",X,I0' + digits_s + ',"/",I0' + digits_s + ',X,"~",A-10,X,"remaining",$)',i+1L,self.nuids,remaining
        ENDIF
     ENDFOR

     IF KEYWORD_SET(Anharmonics) THEN BEGIN

        PRINT
        PRINT,"========================================================="
        PRINT

        !EXCEPT = _e
     ENDIF
  ENDIF ELSE BEGIN

     nregion = N_ELEMENTS(FWHM)

     PRINT
     PRINT,"========================================================="
     PRINT," USING FWHM SECTIONS: "+STRTRIM(STRING(FORMAT='(I0)',nregion),2)
     PRINT,"========================================================="
     PRINT

     IF nregion EQ 1 THEN BEGIN

        FWHM = [FWHM, {treshold:DOUBLE(xmax), fwhm:FWHM[0].fwhm}]

        width = [width, width]

        nregion = 2
     ENDIF

     FOR i = 0L, self.nuids - 1 DO BEGIN

        uid = (*self.data)[range[i+1]].uid

        u0 = range[i] + 1

        u1 = range[i + 1]

        select = LINDGEN(u1 - u0 + 1) + u0

        data[i*NPoints:(i + 1)*NPoints-1].uid = uid

        fsel = WHERE((*self.data)[select].frequency GE xmin - clip * width[0] AND (*self.data)[select].frequency LE FWHM[1].treshold, ndata)

        FOR k = 0, ndata - 1 DO BEGIN

           IF (*self.data)[select[fsel[k]]].intensity GT 0 THEN data[i*NPoints:(i + 1)*NPoints-1].intensity += (*self.data)[select[fsel[k]]].intensity * self->LineProfile(x, (*self.data)[select[fsel[k]]].frequency, width[0], Gaussian=Gaussian, Drude=Drude, Conserve=Conserve, Anharmonics=Anharmonics)
        ENDFOR

        FOR j = 1, nregion - 2 DO BEGIN

           fsel = WHERE((*self.data)[select].frequency GE FWHM[j].treshold AND (*self.data)[select].frequency LT FWHM[j+1].treshold, ndata)

           FOR k = 0, ndata - 1 DO BEGIN

              IF (*self.data)[select[fsel[k]]].intensity GT 0 THEN data[i*NPoints:(i + 1)*NPoints-1].intensity += (*self.data)[select[fsel[k]]].intensity * self->LineProfile(x, (*self.data)[select[fsel[k]]].frequency, width[j], Gaussian=Gaussian, Drude=Drude, Conserve=Conserve, Anharmonics=Anharmonics)
           ENDFOR
        ENDFOR

        fsel = WHERE((*self.data)[select].frequency GE FWHM[j].treshold AND (*self.data)[select].frequency LT xmax + clip * width[j], ndata)

        FOR k = 0, ndata - 1 DO BEGIN

           IF (*self.data)[select[fsel[k]]].intensity GT 0 THEN data[i*NPoints:(i + 1)*NPoints-1].intensity += (*self.data)[select[fsel[k]]].intensity * self->LineProfile(x, (*self.data)[select[fsel[k]]].frequency, width[j], Gaussian=Gaussian, Drude=Drude, Conserve=Conserve, Anharmonics=Anharmonics)
        ENDFOR
     ENDFOR
  ENDELSE

  units = self.units

  CASE (*self.model).type OF

     "AMESPAHDBIDLSUITE_MODEL_ZEROKELVIN_S": units.ordinate = {AmesPAHdb_Unit_S, $
                                                               unit:3, $
                                                               str:'cross-section [x10!U5!N cm!U2!N/mol]'}

     "AMESPAHDBIDLSUITE_MODEL_FIXEDTEMPERATURE_S": units.ordinate = {AmesPAHdb_Unit_S, $
                                                                     unit:3, $
                                                                     str:'spectral radiance [erg/s.cm/PAH]'}

     "AMESPAHDBIDLSUITE_MODEL_CALCULATEDTEMPERATURE_S": units.ordinate = {AmesPAHdb_Unit_S, $
                                                                          unit:3, $
                                                                          str:'spectral radiance [erg/s.cm/PAH]'}

     "AMESPAHDBIDLSUITE_MODEL_CASCADE_S": units.ordinate = {AmesPAHdb_Unit_S, $
                                                            unit:3, $
                                                            str:'radiant energy [erg.cm/PAH]'}
  ENDCASE

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Spectrum', $
                 Type=self.type, $
                 Version=self.version, $
                 Data=data, $
                 PAHdb=self.database, $
                 Uids=(*self.data)[range[1:*]].uid, $
                 Model=*self.model, $
                 Units=units, $
                 Shift=self.shift, $
                 Grid=x, $
                 Profile=profile, $
                 FWHM=fwhm)
END

;+
; Retrieves the AmesPAHdbIDLSuite_Transitions representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Transitions::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  struct = self->AmesPAHdbIDLSuite_Data::Get()

  struct.type = OBJ_CLASS(self)+'_S'

  RETURN,CREATE_STRUCT(struct, 'shift', self.shift)
END

;+
; Populates the AmesPAHdbIDLSuite_Transitions-instance.
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
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Transitions::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Shift) THEN self.shift = Struct.shift

           self->AmesPAHdbIDLSuite_Data::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units
        ENDIF
     ENDIF
  ENDIF ELSE self->AmesPAHdbIDLSuite_Data::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  IF KEYWORD_SET(Shift) THEN self.shift = Shift
END

;+
; Clean-up an AmesPAHdbIDLSuite_Transitions-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Transitions::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_Plot::Cleanup

  self->AmesPAHdbIDLSuite_Data::Cleanup
END

;+
; Create an AmesPAHdbIDLSuite_Transitions-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Transitions-instance
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
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Transitions::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift

  COMPILE_OPT IDL2

  ON_ERROR,2

  self.state = self->AmesPAHdbIDLSuite_Plot::Init()

  IF self.state EQ 1 THEN BEGIN

     IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift $
     ELSE self->AmesPAHdbIDLSuite_Transitions::Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units,Shift=Shift
  ENDIF

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Transitions Class
;
; :Fields:
;   shift: type=double
;     Applied band shift
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Transitions__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Transitions, $
          INHERITS AmesPAHdbIDLSuite_Plot, $
          INHERITS AmesPAHdbIDLSuite_Data, $
          shift:0D}
END

; END OF amespahdbsuite_transitions__define.pro
