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


PRO AmesPAHdbIDLSuite_GUI_Table_Transitions::Update,UID

  COMPILE_OPT IDL2

  ;ON_ERROR,2

  IF NOT PTR_VALID(self.delegate) THEN RETURN

  transitions = (*self.delegate)->getTransitionsByUID(UID)

  transitions_s = transitions->get()

  OBJ_DESTROY,transitions

  ndata = N_ELEMENTS(transitions_s.data)

  data = REPLICATE({w:0E, f:0E, i:0E, s:'', sc:0E}, ndata)

  data.w = 1D4 / transitions_s.data.frequency

  data.f = transitions_s.data.frequency

  data.i = transitions_s.data.intensity

  data.s = transitions_s.data.symmetry

  data.sc = transitions_s.data.scale

  fmt = STRARR(5, ndata)

  FOR i = 0, ndata - 1 DO fmt[*, i] = ['(G0)', '(G0)', '(G0)', '(A0)', '(G0)']

  WIDGET_CONTROL,self.base,UPDATE=0

  WIDGET_CONTROL,self.table,/DESTROY

  self.table = WIDGET_TABLE(self.base, VALUE=data, FORMAT=fmt, /RESIZEABLE_COLUMNS, /NO_ROW_HEADERS, /NO_COPY, /ROW_MAJOR, COLUMN_LABELS=['wav. um', 'nu /cm', 'I km/mol', 'sym.', 'scl.'], SCR_XSIZE=300, SCR_YSIZE=400, COLUMN_WIDTHS=[64, 63, 63, 42, 47])

  WIDGET_CONTROL,self.base,/UPDATE
END

FUNCTION AmesPAHdbIDLSuite_GUI_Table_Transitions::Init,parent,_Extra=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_View::Init(_Extra=extra) EQ 0 THEN RETURN,0

  self.base = WIDGET_BASE(parent)

  self.table = WIDGET_TABLE(self.base, VALUE=[{w:0, f:0, i:0, s:'', sc:0}], /RESIZEABLE_COLUMNS, /NO_ROW_HEADERS, /NO_COPY, /ROW_MAJOR, COLUMN_LABELS=['wav. um', 'nu /cm', 'I km/mol', 'sym.', 'scl.'], COLUMN_WIDTHS=[64, 63, 63, 42, 47], SCR_XSIZE=300, SCR_YSIZE=400, /SCROLL)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Table_Transitions__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Table_Transitions, $
           INHERITS AmesPAHdbIDLSuite_GUI_View, $
           table:0L}
END

; END OF amespahdbidlsuite_gui_table_transitions__define.pro
