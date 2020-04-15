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


PRO AmesPAHdbIDLSuite_GUI_Application::Run

  COMPILE_OPT IDL2

  ;ON_ERROR,2

  self->Realize

  self->Manage
END

PRO AmesPAHdbIDLSuite_GUI_Application::Cleanup

  COMPILE_OPT IDL2

  ;ON_ERROR,2

  self->AmesPAHdbIDLSuite_GUI_Window::Cleanup
END

FUNCTION AmesPAHdbIDLSuite_GUI_Application::Init

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_Window::Init() EQ 0 THEN RETURN,0

  self.application = self

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Application__DEFINE,class

  COMPILE_OPT IDL2

  ;ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Application,$
           INHERITS AmesPAHdbIDLSuite_GUI_Window}
END

; END OF amespahdbidlsuite_gui_application__define.pro
