;+
; FUNCTION_NAME:
;    AmesPAHdbIDLSuite_CREATE_OBSERVATION_UNITS_S
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION: Helper function for creating a
;    AmesPAHdb_Observation_Units_S structure
;
; CATEGORY:
;    HELPER
;
; EXAMPLE
;   units = AMESPAHDBIDLSUITE_CREATE_OBSERVATION_UNITS_S(AUNIT=3, OUNITS=2)
;
; MODIFICATION HISTORY
;
;   02-01-2015
;   First version of the file. Christiaan Boersma.
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
     
     naunits = N_ELEMENTS(aunits)
     
     FOR i = 0, naunits - 1 DO PRINT,FORMAT='(I1,":",4X,A-0)',aunits[i].unit,aunits[i].str

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
 
     nounits = N_ELEMENTS(ounits)
     
     FOR i = 0, nounits - 1 DO PRINT,FORMAT='(I1,":",4X,A-0)',ounits[i].unit,ounits[i].str

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
