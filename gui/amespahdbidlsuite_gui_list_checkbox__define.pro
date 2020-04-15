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


FUNCTION AmesPAHdbIDLSuite_GUI_List_Checkbox::EventHandler,Event

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.buttons) THEN RETURN,0

  button = WHERE(*self.buttons EQ Event.id)

  index = WHERE(*self.showing)

  IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ "AMESPAHDBIDLSUITE_GUI_BUTTON_CHECKBOX_BUTTON_EVENT" THEN BEGIN

     RETURN,{id:self.base, top:Event.top, handler:0L, index:index[button]}
  ENDIF ELSE BEGIN

     IF index[0] NE -1 THEN (*self.selection)[index[button]] = Event.checked

     RETURN,0
  ENDELSE
END

FUNCTION EventHandler__AmesPAHdbIDLSuite_GUI_List_Checkbox,Event

  COMPILE_OPT IDL2

  ON_ERROR,2

  WIDGET_CONTROL,Event.handler,GET_UVALUE=obj

  RETURN,CALL_METHOD('EventHandler', obj, Event)
END

PRO AmesPAHdbIDLSuite_GUI_List_Checkbox::SetSelection,state,Showing=Showing

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.selection) THEN RETURN

  indices = KEYWORD_SET(Showing) ? WHERE(*self.showing AND *self.selection NE state, n) : WHERE(*self.selection NE state, n)

  IF n EQ 0 THEN RETURN

  showing = WHERE(*self.showing, nshowing)

  IF nshowing EQ 0 THEN BEGIN

     (*self.selection)[indices] = state

     RETURN
  ENDIF

  selection = WHERE((*self.selection)[showing] NE state, nselection)

  FOR i = 0L, nselection - 1L DO (*self.buttons_obj)[selection[i]]->SetState,state

  (*self.selection)[indices] = state
END

PRO AmesPAHdbIDLSuite_GUI_List_Checkbox::Update,Indices,Selection=Selection

  COMPILE_OPT IDL2

  ON_ERROR,2

  WIDGET_CONTROL,self.base,UPDATE=0,/HOURGLASS

  self.application->SetProgress,0.0,Message='loading: '

  IF PTR_VALID(self.buttons_obj) THEN BEGIN

     WIDGET_CONTROL,self.list,/DESTROY

     OBJ_DESTROY,*self.buttons_obj

     PTR_FREE,self.buttons_obj

     PTR_FREE,self.buttons

     *self.showing *= 0B
  END

  IF N_PARAMS() GT 0 THEN BEGIN

     IF Indices[0] EQ -1 THEN BEGIN

        IF KEYWORD_SET(Selection) THEN BEGIN

           index = WHERE(*self.selection, nbuttons)
        ENDIF ELSE BEGIN

           nbuttons = N_ELEMENTS((*self.icons)[*, 0, 0, 0])

           index = INDGEN(nbuttons, /LONG)
        ENDELSE
     ENDIF ELSE BEGIN

        index = Indices

        nbuttons = N_ELEMENTS(Indices)
     ENDELSE

     WIDGET_CONTROL,self.base,UPDATE=0

     self.list = WIDGET_BASE(self.base, MAP=0, YSIZE=nbuttons*32L)

     IF nbuttons GT 0 THEN BEGIN

        self.buttons = PTR_NEW(LONARR(nbuttons))

        self.buttons_obj = PTR_NEW(OBJARR(nbuttons))

        FOR i = 0L, nbuttons - 1L DO BEGIN

           (*self.buttons_obj)[i] = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Button_Checkbox', self.list, Base=Base, VALUE=REFORM((*self.icons)[index[i], *, *, *]), IsChecked=(*self.selection)[index[i]], YOFFSET=i*32)

           (*self.buttons)[i] = Base

           IF i MOD 16 EQ 0 THEN self.application->SetProgress,FLOAT(i+1)/nbuttons,Message='loading: '
        ENDFOR

        (*self.showing)[index] = 1B
     ENDIF

     WIDGET_CONTROL,self.list,/MAP
  ENDIF

  WIDGET_CONTROL,self.base,/UPDATE

  self.application->SetProgress,1.0
END

PRO AmesPAHdbIDLSuite_GUI_List_Checkbox::Load

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.delegate) THEN RETURN

  WIDGET_CONTROL,/HOURGLASS

  self.application->SetProgress,0.0,Message='loading: '

  IF PTR_VALID(self.buttons_obj) THEN BEGIN

     WIDGET_CONTROL,self.base,MAP=0

     WIDGET_CONTROL,self.list,/DESTROY

     WIDGET_CONTROL,self.base,/MAP

     OBJ_DESTROY,*self.buttons_obj

     PTR_FREE,self.buttons_obj

     PTR_FREE,self.buttons
  ENDIF

  IF PTR_VALID(self.showing) THEN PTR_FREE,self.showing

  IF PTR_VALID(self.selection) THEN PTR_FREE,self.selection

  nbuttons = N_ELEMENTS((*self.icons)[*, 0, 0, 0])

  self.buttons = PTR_NEW(LONARR(nbuttons))

  self.buttons_obj = PTR_NEW(OBJARR(nbuttons))

  self.showing = PTR_NEW(BYTARR(nbuttons) + 1B)

  self.selection = PTR_NEW(BYTARR(nbuttons))

  WIDGET_CONTROL,self.base,UPDATE=0

  self.list = WIDGET_BASE(self.base, MAP=0, YSIZE=nbuttons*32L)

  FOR i = 0L, nbuttons - 1L DO BEGIN

     (*self.buttons_obj)[i] = OBJ_NEW('AmesPAHdbIDLSuite_GUI_Button_Checkbox', self.list, Base=Base, VALUE=REFORM((*self.icons)[i, *, *, *]), YOFFSET=i*32)

     (*self.buttons)[i] = Base

     IF i MOD 16 EQ 0 THEN self.application->SetProgress,FLOAT(i+1)/nbuttons,Message='loading: '
  ENDFOR

  WIDGET_CONTROL,self.base,/UPDATE

  WIDGET_CONTROL,self.list,/MAP,YSIZE=nbuttons*32L

  event = {AmesPAHdbIDLSuite_GUI_Button_Checkbox_Button_Event, id:(*self.buttons)[0], top:0L, handler:0L}

  WIDGET_CONTROL,self.base,SEND_EVENT=event,/NO_COPY

  self.application->SetProgress,1.0
END

PRO  AmesPAHdbIDLSuite_GUI_List_Checkbox::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_GUI_View::Cleanup

  IF PTR_VALID(self.showing) THEN PTR_FREE,self.showing

  IF PTR_VALID(self.selection) THEN PTR_FREE,self.selection

  IF PTR_VALID(self.buttons) THEN PTR_FREE,self.buttons

  IF PTR_VALID(self.buttons_obj) THEN BEGIN

     nbuttons = N_ELEMENTS(self.buttons_obj)

     FOR i = 0L, nbuttons - 1L DO OBJ_DESTROY,(*self.buttons_obj)[i]

     PTR_FREE,self.buttons_obj
  ENDIF
END

FUNCTION AmesPAHdbIDLSuite_GUI_List_Checkbox::Init,parent,_Extra=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_View::Init(_Extra=extra) EQ 0 THEN RETURN,0

  self.base = WIDGET_BASE(parent, SCR_XSIZE=170, SCR_YSIZE=606, /SCROLL, EVENT_FUNC='EventHandler__AmesPAHdbIDLSuite_GUI_List_Checkbox', UVALUE=self)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_List_Checkbox__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_List_Checkbox, $
           INHERITS AmesPAHdbIDLSuite_GUI_View, $
           list:0L, $
           showing:PTR_NEW(), $
           selection:PTR_NEW(), $
           icons:PTR_NEW(), $
           buttons:PTR_NEW(), $
           buttons_obj:PTR_NEW()}
END

; END OF amespahdbidlsuite_gui_list_checkbox__define.pro
