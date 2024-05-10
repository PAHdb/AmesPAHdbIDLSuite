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
;     05-10-2024
;     Read Suite version date. Christiaan Boersma.
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

  file = FILE_EXPAND_PATH( $
           FILE_DIRNAME( $
             ROUTINE_FILEPATH(), /MARK_DIRECTORY) + $
             '..' + PATH_SEP() + 'VERSION')

  IF FILE_TEST(file , /READ) THEN BEGIN

    OPENR,funit,file,/GET_LUN

    str = ""

    READF,funit,str

    CLOSE,funit

    self.version = str
  ENDIF ELSE self.version = 'unknown'

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Application__DEFINE,class

  COMPILE_OPT IDL2

  ;ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Application,$
           INHERITS AmesPAHdbIDLSuite_GUI_Window, $
           version:''}
END

; END OF amespahdbidlsuite_gui_application__define.pro
