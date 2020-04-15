; docformat = 'rst'

;+
;
; Class used to inherit from in IDL versions before 8.0, allowing
; operator overloaded classes to still compile on earlier IDL
; versions.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;  Creating and destroying and IDL_Object-instance::
;
;     IDL> obj = OBJ_NEW('IDL_Object')
;     IDL> OBJ_DESTROY,obj
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
;   04-07-2015
;   First version of the file. Christiaan Boersma.
;
; :Private:
;-

;+
; Clean-up an IDL_Object-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO IDL_Object::Cleanup

  COMPILE_OPT  IDL2

  ON_ERROR,2
END

;+
; Create an IDL_Object-instance
;
; :Returns:
;   Object-instance
;
; :Categories:
;   CLASS
;-
FUNCTION IDL_Object::Init

  COMPILE_OPT  IDL2

  ON_ERROR,2

  RETURN,1
END

;+
; Defines the IDL_Object Class
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO IDL_Object__DEFINE

  COMPILE_OPT  IDL2

  ON_ERROR,2

  void = {IDL_Object, $
          _$dummy:0B}
END

; END OF IDL_Object__define.pro
