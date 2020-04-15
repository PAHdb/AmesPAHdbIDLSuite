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


PRO CREATE_SAV

  COMPILE_OPT IDL2

  ON_ERROR,2

  gui_classes_pro = FILE_SEARCH("*__define.pro", COUNT=n)

  gui_classes = STRMID(gui_classes_pro, INTARR(1, n), REFORM(STRPOS(gui_classes_pro, "__define.pro"), 1, n))

  RESOLVE_ALL,CLASS=gui_classes

  CD,'..',CURRENT=cwd

  amespahdb_classes_pro = FILE_SEARCH("*__define.pro", COUNT=n)

  CD,cwd

  amespahdb_classes = STRMID(amespahdb_classes_pro, INTARR(1, n), REFORM(STRPOS(amespahdb_classes_pro, "__define.pro"), 1, n))

  RESOLVE_ALL,CLASS=amespahdb_classes


  RESOLVE_ALL,RESOLVE_PROCEDURE='amespahdbidlsuite'

  SAVE,FILENAME='amespahdbidlsuite.sav',/ROUTINES
END
