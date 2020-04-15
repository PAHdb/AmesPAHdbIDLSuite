; docformat = 'rst'

;+
;
; Helper function for creating an AmesPAHdb_Observation_Units_S
; structure.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Creating an AmesPAHdb_Observation_Units_S structure::
;
;      IDL> units = AMESPAHDBIDLSUITE_CREATE_OBSERVATION_UNITS_S(AUNIT=3, $
;                                                                OUNITS=2)
;
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
;     07-07-2016
;     Use PRINT without for-loop. Christiaan Boersma.
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;-

;+
; Creates an AmesPAHdb_Observation_Units_S
; structure.
;
; :Returns:
;   Structure
;
; :Keywords:
;   AUNIT: in, required, type=int
;     Abscissa units: 1=frequency [cm!U-1!N], 2=frequency [Hz],
;                     3=wavelength [um], and 4=wavelength [A]
;   OUNIT: in, required, type=int
;     Ordinate units: 1=surface brightness [MJy/sr],
;                     2=surface brightness [W/cm!U2!N/sr],
;                     3=flux [Jy], and 4=flux [W/cm!U2!N]
;
; :Categories:
;   HELPER
;-
FUNCTION AmesPAHdbIDLSuite_CREATE_OBSERVATION_UNITS_S,AUNIT=aunit,OUNIT=ounit

  units = {AmesPAHdb_Observation_Units_S, $
           abscissa:{AmesPAHdb_Unit_S, $
                     unit:0, $
                     str:''}, $
           ordinate:{AmesPAHdb_Unit_S, $
                     unit:0, $
                     str:''}}

  aunits = [{unit:1, str:'frequency [cm!U-1!N]'}, $
            {unit:2, str:'frequency [Hz]'}, $
            {unit:3, str:'wavelength [micron]'}, $
            {unit:4, str:'wavelength [A]'}]

  IF NOT KEYWORD_SET(AUNIT) THEN BEGIN

     aunit = 0

     PRINT
     PRINT,"========================================================="
     PRINT,"             PLEASE SELECT ABSCISSA-UNITS                   "
     PRINT,"========================================================="
     PRINT

     PRINT,FORMAT='(I1,":",4X,A-0)',aunits

     PRINT

     READ,aunit,PROMPT='ABSCISSA-UNITS: '
  ENDIF

  select = WHERE(aunits.unit EQ aunit, nselect)

  IF nselect EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"               INVALID ABSCISSA-UNITS: "+STRING(aunit)
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE BEGIN

     units.abscissa.unit = aunit

     units.abscissa.str = aunits[select].str

     PRINT
     PRINT,"========================================================="
     PRINT,"       ABSCISSA-UNITS: "+units.abscissa.str
     PRINT,"========================================================="
     PRINT
  ENDELSE

  ounits = [{unit:1, str:'surface brightness [MJy/sr]'}, $
            {unit:2, str:'surface brightness [W/cm!U2!N/sr]'}, $
            {unit:3, str:'flux [Jy]'}, $
            {unit:4, str:'flux [W/cm!U2!N]'}]

  IF NOT KEYWORD_SET(OUNIT) THEN BEGIN

     ounit = 0

     PRINT
     PRINT,"========================================================="
     PRINT,"                 PLEASE SELECT ORDINATE-UNITS                   "
     PRINT,"========================================================="
     PRINT

     PRINT,FORMAT='(I1,":",4X,A-0)',ounits,ounits

     PRINT

     READ,ounit,PROMPT='ORDINATE-UNITS: '
  ENDIF

  select = WHERE(ounits.unit EQ ounit, nselect)

  IF nselect EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                   INVALID ORDINATE-UNITS"+STRING(ounit)
     PRINT,"========================================================="
     PRINT
  ENDIF ELSE BEGIN

     units.ordinate.unit = ounit

     units.ordinate.str = ounits[select].str

     PRINT
     PRINT,"========================================================="
     PRINT,"       ORDINATE-UNITS: "+ounits[select].str
     PRINT,"========================================================="
     PRINT
  ENDELSE

  RETURN,units
END

; END OF amespahdbidlsuite_create_observation_units_s.pro