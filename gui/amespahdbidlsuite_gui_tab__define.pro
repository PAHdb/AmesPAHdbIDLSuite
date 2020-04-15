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


PRO AmesPAHdbIDLSuite_GUI_Tab::Realize
  COMPILE_OPT IDL2

  ON_ERROR,2
END

PRO AmesPAHdbIDLSuite_GUI_Tab::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_GUI_View::Cleanup
END

FUNCTION AmesPAHdbIDLSuite_GUI_Tab::Init,window_obj

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_View::Init() EQ 0 THEN RETURN,0

  STRUCT_ASSIGN,window_obj,self

  self.window_obj = window_obj

  self.window_obj->addTab,self

  self.base = WIDGET_BASE(self.window_obj->getProperty(/TBAR), /COL)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Tab__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Tab,$
           INHERITS AmesPAHdbIDLSuite_GUI_View, $
           window_obj:OBJ_NEW()}
END

; END OF amespahdbidlsuite_gui_tab__define.pro
