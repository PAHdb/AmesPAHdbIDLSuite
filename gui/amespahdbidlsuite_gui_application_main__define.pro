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
;     07-06-2021
;     Cleaned up some progress bar. Christiaan Boersma.
;     12-31-2015
;     First version of the file. Christiaan Boersma.
;-


PRO AmesPAHdbIDLSuite_GUI_Application_Main::EventHandler,Event

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  IF Event.id EQ self.open THEN BEGIN

     file = DIALOG_PICKFILE(/READ, FILTER=['*.xml'])

     IF file EQ '' THEN RETURN

     IF PTR_VALID(self.delegate) THEN BEGIN

        IF OBJ_VALID(*self.delegate) THEN OBJ_DESTROY,*self.delegate
     ENDIF ELSE self.delegate = PTR_NEW(/ALLOCATE)

     *self.delegate = OBJ_NEW('AmesPAHdbIDLSuite', Filename=file)

     WIDGET_CONTROL,self.mlabel,SET_VALUE=(*self.delegate)->GetVersion(/Str)

     self->LoadIcons

     IF PTR_VALID(self.tabs) THEN BEGIN

        ntabs = N_ELEMENTS(*self.tabs)

        FOR i = 0, ntabs - 1 DO (*self.tabs)[i]->Realize
     ENDIF
  ENDIF ELSE IF Event.id EQ self.about THEN BEGIN

     ok = DIALOG_MESSAGE(['This is the Graphical User Interface (GUI) to the AmesPAHdbIDLSuite.','This version of the suite is dated '+(*self.delegate).version+'.','','The suite has been created and is maintained by Dr. Christiaan Boersma.', 'Please contact the maintainer at Christiaan.Boerma@nasa.gov', 'for questions and/or comments/suggestions.'], DIALOG_PARENT=self.base, TITLE="About", /INFORMATION)
  ENDIF ELSE IF Event.id EQ self.tbar THEN BEGIN

     CASE Event.tab OF

        0: BEGIN

           RETURN
        END

        1: BEGIN

           selection = ((self.browse_obj)->GetProperty(/ULIST_OBJ))->GetProperty(/SELECTION)

           self.spectrum_obj->SetProperty,SELECTION=PTR_NEW(*selection)

           self->AmesPAHdbIDLSuite_GUI_Window::EventHandler,Event
        END
     ENDCASE
  ENDIF ELSE self->AmesPAHdbIDLSuite_GUI_Window::EventHandler,Event
END

PRO AmesPAHdbIDLSuite_GUI_Application_Main::LoadIcons

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  p = (*self.delegate)->Pointer()

  species = (*self.delegate)->GetSpeciesByUID(-1)

  species_s = species->Get()

  srt = SORT(species_s.data.uid)

  species_s.data = species_s.data[srt]

  hash = (*self.delegate)->GetHash()

  IF FILE_TEST(GETENV('IDL_TMPDIR')+hash+'_icons.sav', /READ) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"              RESTORING ICONS FROM CACHE                 "
     PRINT,"========================================================="
     PRINT
     RESTORE,FILENAME=GETENV('IDL_TMPDIR')+hash+'_icons.sav'
  ENDIF ELSE BEGIN

     geometries = species->Geometry()

     OBJ_DESTROY,species

     geometries->Diagonalize

     icons = BYTARR(species_s.nuids, 100, 32, 3)

     old_d = !D.NAME

     SET_PLOT,'Z'

     PRINT,"========================================================="

     PRINT,FORMAT='("        GENERATING ICONS...........",I04,"/",I04,$)', 1, species_s.nuids

     DEVICE,SET_RESOLUTION=[100, 32],SET_PIXEL_DEPTH=24,DECOMPOSED=0

     has_str_replace = FILE_WHICH('str_replace.pro', /INCLUDE_CURRENT_DIR) NE ""

     FOR i = 0, species_s.nuids - 1 DO BEGIN

        ERASE,192

        structure = geometries->Structure(species_s.data[i].uid, BACKGROUND=192, RESOLUTION=[32,32])

        TV,structure,/TRUE

        IF has_str_replace THEN BEGIN

           formula = STR_REPLACE(species_s.data[i].formula, "([A-Z])([0-9]+)", "$1!L$2!N", /GLOBAL)

           formula = STR_REPLACE(formula, "((\+)+|(\+[0-9])|(-)+|(-[0-9]))", "!U$1!N")
        ENDIF ELSE formula = species_s.data[i].formula

        XYOUTS,33,12,formula,/DEVICE,COLOR=0

        icons[i, *, *, *] = TVRD(TRUE=3)

        PRINT,FORMAT='("'+STRING(13B)+'",I04,"/",I04,$)', i + 1, species_s.nuids
     ENDFOR

     PRINT

     DEVICE,/CLOSE

     PRINT,"========================================================="

     SET_PLOT,old_d

     OBJ_DESTROY,geometries

     PRINT
     PRINT,"========================================================="
     PRINT,"                 STORING ICONS IN CACHE                  "
     PRINT,"========================================================="
     PRINT

     SAVE,icons,FILENAME=GETENV('IDL_TMPDIR')+hash+'_icons.sav'
  ENDELSE

  *self.icons = TEMPORARY(icons)

  *self.uids = TEMPORARY(species_s.data.uid)
END

PRO AmesPAHdbIDLSuite_GUI_Application_Main::CreateMBar

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  self.file = WIDGET_BUTTON(self.mbar, VALUE='File', /MENU)

  self.open = WIDGET_BUTTON(self.file, VALUE='Open')

  self.about = WIDGET_BUTTON(self.file, VALUE='About')

  self.exit = WIDGET_BUTTON(self.file, VALUE='Exit')
END

PRO AmesPAHdbIDLSuite_GUI_Application_Main::Cleanup

  COMPILE_OPT IDL2

  ;ON_ERROR,2

  self->AmesPAHdbIDLSuite_GUI_Application::Cleanup

  IF OBJ_VALID(self.browse_obj) THEN OBJ_DESTROY,self.browse_obj

  IF OBJ_VALID(self.spectrum_obj) THEN OBJ_DESTROY,self.spectrum_obj

  IF PTR_VALID(self.delegate) THEN BEGIN

     IF OBJ_VALID(*self.delegate) THEN OBJ_DESTROY,*self.delegate

     PTR_FREE,self.delegate
  ENDIF

  IF PTR_VALID(self.icons) THEN PTR_FREE,self.icons
END

FUNCTION AmesPAHdbIDLSuite_GUI_Application_Main::Init,Filename=Filename,Check=Check,Cache=Cache

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_Application::Init() EQ 0 THEN RETURN,0

  pahdb = OBJ_NEW('AmesPAHdbIDLSuite', Filename=Filename, Check=Check, Cache=Cache)

  self.uids = PTR_NEW(/ALLOCATE)

  self.icons = PTR_NEW(/ALLOCATE)

  IF OBJ_VALID(pahdb) THEN BEGIN

     self.delegate = PTR_NEW(pahdb)

     WIDGET_CONTROL,self.base,TLB_SET_TITLE='NASA Ames PAH IR Spectroscopic Database - '+'Suite dated ' + ((*self.delegate)->GetVersion()).date

     WIDGET_CONTROL,self.mlabel,SET_VALUE=(*self.delegate)->GetVersion(/Str)

     self->LoadIcons
  ENDIF

  self.browse_obj = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Tab_Browse', self, ICONS=self.icons, UIDS=self.uids)

  self.spectrum_obj = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Tab_Spectrum', self, UIDS=self.uids)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Application_Main__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Application_Main,$
           INHERITS AmesPAHdbIDLSuite_GUI_Application, $
           open:0L, $
           about:0L, $
           browse_obj:OBJ_NEW(), $
           spectrum_obj:OBJ_NEW(), $
           uids:PTR_NEW(), $
           icons:PTR_NEW()}
END

; END OF amespahdbidlsuite_gui_application_main__define.pro
