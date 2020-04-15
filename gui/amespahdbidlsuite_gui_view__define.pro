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


FUNCTION AmesPAHdbIDLSuite_GUI_View::GetProperty,_EXTRA=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  property = (TAG_NAMES(extra))[0]

  CALL_PROCEDURE,OBJ_CLASS(self)+'__define',struct

  index = WHERE(STRPOS(TAG_NAMES(struct), property) EQ 0)

  RETURN, self.(index[0])
END

PRO AmesPAHdbIDLSuite_GUI_View::SetProperty,_EXTRA=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  properties = TAG_NAMES(extra)

  CALL_PROCEDURE, OBJ_CLASS(self)+'__define',struct

  n_tags = N_TAGS(extra)

  tag_names = TAG_NAMES(struct)

  FOR i = 0L, n_tags - 1 DO BEGIN

     property = properties[i]

     index = WHERE(STRPOS(tag_names, property) EQ 0)

     self.(index) = extra.(i)
  ENDFOR
END

PRO AmesPAHdbIDLSuite_GUI_View::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2
END

FUNCTION AmesPAHdbIDLSuite_GUI_View::Init,_EXTRA=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_TAGS(extra) GT 0 THEN self->SetProperty,_EXTRA=extra

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_View__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_View,$
           base:0L, $
           application:OBJ_NEW(), $
           delegate:PTR_NEW()}
END

; END OF amespahdbidlsuite_gui_view__define.pro
