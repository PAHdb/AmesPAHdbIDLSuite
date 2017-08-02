;+
; CLASS_NAME:
;    IDL_Object
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic 
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;     Class used to inherit from in IDL versions before 8.0,
;     allowing operator overloaded classes to still compile on
;     earlier versions
;
; CATEGORY:
;    INTERFACE
;
; SUPERCLASSES:
;    NONE  
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;    NONE
;
; OPTIONAL:
;    NONE
;
; CREATION:
;    obj = OBJ_NEW('IDL_Object')
;
; METHODS
;    PUBLIC:
;      NONE
;
;    PRIVATE:
;      NONE
;      
; EXAMPLE
;    obj = OBJ_NEW('IDL_Object')
;    OBJ_DESTROY,obj
;
; MODIFICATION HISTORY
;
;   04-07-2015
;   First version of the file. Christiaan Boersma.
;-

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP


PRO IDL_Object::Cleanup

  COMPILE_OPT  IDL2

  ON_ERROR,2
END

FUNCTION IDL_Object::Init

  COMPILE_OPT  IDL2

  ON_ERROR,2  

  RETURN,1
END

PRO IDL_Object__DEFINE

  COMPILE_OPT  IDL2

  ON_ERROR,2

  void = {IDL_Object, $
          _$dummy:0B}
END

; END OF IDL_Object__define.pro
