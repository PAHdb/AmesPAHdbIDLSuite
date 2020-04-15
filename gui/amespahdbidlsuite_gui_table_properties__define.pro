; docformat = 'rst'

;+
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Private:
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
;     12-31-2015
;     First version of the file. Christiaan Boersma.
;-


PRO AmesPAHdbIDLSuite_GUI_Table_Properties::Update,UID

  COMPILE_OPT IDL2

  ;ON_ERROR,2

  IF NOT PTR_VALID(self.delegate) THEN RETURN

  species = (*self.delegate)->getSpeciesByUID(UID)

  geometry = (*self.delegate)->getGeometryByUID(UID)

  rings = geometry->Rings()

  void = EXECUTE("rings = CREATE_STRUCT((TAG_NAMES(rings))[1:*]," + STRJOIN(STRING(FORMAT='("rings.(",I0,")")', INDGEN(N_TAGS(rings) - 1) + 1), ", ")+ ")")

  ;;reference = SIZE((species->references())[0], /TYPE) EQ 8 ? (species->references())[0].str : ''

  ;;properties = CREATE_STRUCT((species->Get()).data, 'mass', (geometry->Mass()).mass, 'area', (geometry->Area()).area, rings, 'reference', reference)

  properties = CREATE_STRUCT((species->Get()).data, 'mass', (geometry->Mass()).mass, 'area', (geometry->Area()).area, rings)

  OBJ_DESTROY,[geometry, species]

  WIDGET_CONTROL,self.base,UPDATE=0

  WIDGET_CONTROL,self.table,/DESTROY

  self.table = WIDGET_TABLE(self.base, VALUE=[properties], /NO_COLUMN_HEADERS, /NO_COPY, /COLUMN_MAJOR, ROW_LABELS=TAG_NAMES(properties), SCR_XSIZE=200, SCR_YSIZE=400, COLUMN_WIDTHS=[200])

  WIDGET_CONTROL,self.base,/UPDATE
END

FUNCTION AmesPAHdbIDLSuite_GUI_Table_Properties::Init,parent,_Extra=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_View::Init(_Extra=extra) EQ 0 THEN RETURN,0

  self.base = WIDGET_BASE(parent)

  self.table = WIDGET_TABLE(self.base, VALUE=[{uid:0}], ROW_LABELS=['UID'], /NO_COLUMN_HEADERS, /NO_COPY, /COLUMN_MAJOR, SCR_XSIZE=200, SCR_YSIZE=400, COLUMN_WIDTHS=200)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Table_Properties__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Table_Properties, $
           INHERITS AmesPAHdbIDLSuite_GUI_View, $
           table:0L}
END

; END OF amespahdbidlsuite_gui_table_properties__define.pro
