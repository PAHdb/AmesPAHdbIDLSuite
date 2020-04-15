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


PRO AmesPAHdbIDLSuite_GUI_Tab_Browse::Realize

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  IF OBJ_VALID(self.ulist_obj) THEN self.ulist_obj->Load
END

FUNCTION AmesPAHdbIDLSuite_GUI_Tab_Browse::EventHandler,Event

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  IF Event.id EQ self.ulist_obj->GetProperty(/BASE) THEN BEGIN

     IF OBJ_VALID(self.ulist_obj) AND Event.index NE -1 THEN BEGIN

        IF OBJ_VALID(self.sdraw_obj) THEN self.sdraw_obj->Update,(*self.uids)[Event.index]

        IF OBJ_VALID(self.gdraw_obj) THEN self.gdraw_obj->Update,(*self.uids)[Event.index]

        IF OBJ_VALID(self.ttable_obj) THEN self.ttable_obj->Update,(*self.uids)[Event.index]

        IF OBJ_VALID(self.ptable_obj) THEN self.ptable_obj->Update,(*self.uids)[Event.index]
     ENDIF
  ENDIF ELSE IF Event.id EQ self.sbutton OR Event.id EQ self.sfield THEN BEGIN

     WIDGET_CONTROL,self.sfield,GET_VALUE=value

     IF OBJ_VALID(self.ulist_obj) THEN BEGIN

        IF NOT PTR_VALID(self.delegate) THEN RETURN,0

        uids = (*(self.window_obj->getProperty(/Delegate)))->Search(value[0], nuids)

        IF nuids GT 0 THEN self.ulist_obj->Update,VALUE_LOCATE(*self.uids, uids) ELSE self.ulist_obj->Update
     ENDIF
  ENDIF ELSE IF Event.id EQ self.labutton THEN BEGIN

     IF OBJ_VALID(self.ulist_obj) THEN self.ulist_obj->Update,-1

     WIDGET_CONTROL,self.sfield,SET_VALUE=""
  ENDIF ELSE IF Event.id EQ self.sabutton THEN BEGIN

     IF OBJ_VALID(self.ulist_obj) THEN self.ulist_obj->SetSelection,1B
  ENDIF ELSE IF Event.id EQ self.scbutton THEN BEGIN

     IF OBJ_VALID(self.ulist_obj) THEN self.ulist_obj->SetSelection,0B
    ENDIF ELSE IF Event.id EQ self.slbutton THEN BEGIN

     IF OBJ_VALID(self.ulist_obj) THEN self.ulist_obj->SetSelection,1B,/Showing
  ENDIF ELSE IF Event.id EQ self.lsbutton THEN BEGIN

     IF OBJ_VALID(self.ulist_obj) THEN self.ulist_obj->Update,-1,/Selection
  ENDIF
  RETURN,0
END

FUNCTION Eventhandler__AmesPAHdbIDLSuite_GUI_Tab_Browse,Event

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  WIDGET_CONTROL,Event.handler,GET_UVALUE=obj

  RETURN,CALL_METHOD('EventHandler', obj, Event)
END

PRO AmesPAHdbIDLSuite_GUI_Tab_Browse::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF OBJ_VALID(self.ulist_obj) THEN OBJ_DESTROY,self.ulist_obj

  IF OBJ_VALID(self.ttable_obj) THEN OBJ_DESTROY,self.ttable_obj

  IF OBJ_VALID(self.ptable_obj) THEN OBJ_DESTROY,self.ptable_obj

  IF OBJ_VALID(self.sdraw_obj) THEN OBJ_DESTROY,self.sdraw_obj

  IF OBJ_VALID(self.gdraw_obj) THEN OBJ_DESTROY,self.gdraw_obj

  self->AmesPAHdbIDLSuite_GUI_Tab::Cleanup
END

FUNCTION AmesPAHdbIDLSuite_GUI_Tab_Browse::Init,window_obj,_Extra=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_Tab::Init(window_obj) EQ 0 THEN RETURN,0

  WIDGET_CONTROL,self.base,BASE_SET_TITLE="Browse", EVENT_FUNC='EventHandler__AmesPAHdbIDLSuite_GUI_Tab_Browse', SET_UVALUE=self


  self.tbar = WIDGET_BASE(self.base, /ROW, FRAME=1)


  self.lmenu = WIDGET_BUTTON(self.tbar, VALUE="List", /Menu)

  self.labutton = WIDGET_BUTTON(self.lmenu, VALUE="All")

  self.lsbutton = WIDGET_BUTTON(self.lmenu, VALUE="Selected")


  self.smenu = WIDGET_BUTTON(self.tbar, VALUE="Select", /MENU)

  self.sabutton = WIDGET_BUTTON(self.smenu, VALUE="All")

  self.slbutton = WIDGET_BUTTON(self.smenu, VALUE="Listed")

  self.scbutton = WIDGET_BUTTON(self.smenu, VALUE="Clear")


  spacer = WIDGET_LABEL(self.tbar, SCR_XSIZE=130, VALUE="")


  self.sfield = WIDGET_TEXT(self.tbar, SCR_XSIZE=400, /EDITABLE, /NO_NEWLINE)

  self.sbutton = WIDGET_BUTTON(self.tbar, VALUE="Search")


  self.cbar = WIDGET_BASE(self.base, /ROW, FRAME=0)


  self.ulist_obj = OBJ_NEW('AmesPAHdbIDLSuite_GUI_List_Checkbox', self.cbar, DELEGATE=self.delegate, APPLICATION=self.application, ICONS=self.icons)


  self.obar = WIDGET_BASE(self.cbar, /COL)

  self.sdraw_obj = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Draw_Transitions', self.obar, DELEGATE=self.delegate, APPLICATION=self.application)

  self.ttable_obj = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Table_Transitions', self.obar, DELEGATE=self.delegate, APPLICATION=self.application)


  self.gbar = WIDGET_BASE(self.cbar, /COL)

  self.gdraw_obj = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Draw_Geometry', self.gbar, DELEGATE=self.delegate, APPLICATION=self.application)

  self.ptable_obj = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Table_Properties', self.gbar, DELEGATE=self.delegate, APPLICATION=self.application)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Tab_Browse__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Tab_Browse, $
           INHERITS AmesPAHdbIDLSuite_GUI_Tab, $
           tbar:0L, $
           lmenu:0L, $
           labutton:0L, $
           lsbutton:0L, $
           smenu:0L, $
           slbutton:0L, $
           sabutton:0L, $
           scbutton:0L, $
           sfield:0L, $
           sbutton:0L, $
           cbar:0L, $
           ulist_obj:OBJ_NEW(), $
           obar:0L, $
           sdraw_obj:OBJ_NEW(), $
           ttable_obj:OBJ_NEW(), $
           gbar:0L, $
           gdraw_obj:OBJ_NEW(), $
           ptable_obj:OBJ_NEW(), $
           icons:PTR_NEW(), $
           uids:PTR_NEW()}
END

; END OF amespahdbidlsuite_gui_tab_browse__define.pro
