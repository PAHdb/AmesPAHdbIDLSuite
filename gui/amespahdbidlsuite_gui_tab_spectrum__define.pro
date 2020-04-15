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


FUNCTION AmesPAHdbIDLSuite_GUI_Tab_Spectrum::EventHandler,Event

  COMPILE_OPT IDL2, HIDDEN

  ;ON_ERROR,2

  IF NOT PTR_VALID(self.delegate) THEN RETURN,0

  button = WHERE(*self.buttons EQ Event.id)

  IF button[0] EQ -1 THEN RETURN,0

  index = WHERE(*self.selection, nindex)

  transitions = (*(self.delegate))->getTransitionsByUID((*self.uids)[index[button]])

  spectrum = transitions->Convolve()

  WIDGET_CONTROL,self.sdraw,GET_VALUE=wid

  WSET,wid

  DEFSYSV,'!AmesPAHdbIDLSuite_Plot_Managed_Window_ID',wid

  spectrum->Plot

  OBJ_DESTROY,[spectrum, transitions]
END

FUNCTION Eventhandler__AmesPAHdbIDLSuite_GUI_Tab_Spectrum,Event

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  WIDGET_CONTROL,Event.handler,GET_UVALUE=obj

  RETURN,CALL_METHOD('EventHandler', obj, Event)
END

PRO AmesPAHdbIDLSuite_GUI_Tab_Spectrum::Realize

  COMPILE_OPT IDL2

  ON_ERROR,2

  WIDGET_CONTROL,self.sdraw,GET_VALUE=wid

  WSET,wid

  DEFSYSV,'!AmesPAHdbIDLSuite_Plot_Managed_Window_ID',wid

  ERASE

  IF PTR_VALID(self.selection) THEN BEGIN

     WIDGET_CONTROL,/HOURGLASS

     self.application->SetProgress,0.0,Message='loading: '

     IF PTR_VALID(self.buttons) THEN BEGIN

        WIDGET_CONTROL,self.blist,MAP=0

        WIDGET_CONTROL,self.list,/DESTROY

        WIDGET_CONTROL,self.blist,/MAP

        PTR_FREE,self.buttons
     ENDIF

     index = WHERE(*self.selection, nindex)

     IF nindex EQ 0 THEN RETURN

     self.buttons = PTR_NEW(LONARR(nindex))

     WIDGET_CONTROL,self.blist,UPDATE=0

     self.list = WIDGET_BASE(self.blist, MAP=0, YSIZE=nindex*32)

     FOR i = 0, nindex - 1 DO BEGIN

        (*self.buttons)[i] = WIDGET_BUTTON(self.list, VALUE=REFORM((*self.icons)[index[i], *, *, *]), /FLAT, XOFFSET=0, YOFFSET=i*32)

        IF i MOD 16 EQ 0 THEN self.application->SetProgress,FLOAT(i+1)/nindex,Message='loading: '
     ENDFOR

     WIDGET_CONTROL,self.blist,/UPDATE

     WIDGET_CONTROL,self.list,/MAP

     WIDGET_CONTROL,(*self.buttons)[0],SEND_EVENT={id:0L, top:0L, handler:0L},/NO_COPY

     self.application->SetProgress,1.0
  ENDIF
END

PRO AmesPAHdbIDLSuite_GUI_Tab_Spectrum::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.buttons) THEN PTR_FREE,self.buttons

  IF PTR_VALID(self.selection) THEN PTR_FREE,self.selection

  DEFSYSV,'!AmesPAHdbIDLSuite_Plot_Managed_Window_ID',-2
END

FUNCTION AmesPAHdbIDLSuite_GUI_Tab_Spectrum::Init,window_obj,_Extra=extra

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF self->AmesPAHdbIDLSuite_GUI_Tab::Init(window_obj) EQ 0 THEN RETURN,0

  WIDGET_CONTROL,self.base,BASE_SET_TITLE="Spectrum"

  self.contents = WIDGET_BASE(self.base, /ROW)

  self.blist = WIDGET_BASE(self.contents, FRAME=1, SCR_XSIZE=124, SCR_YSIZE=650, /SCROLL, EVENT_FUNC='EventHandler__AmesPAHdbIDLSuite_GUI_Tab_Spectrum', UVALUE=self)

  self.sdraw = WIDGET_DRAW(self.contents, GRAPHICS_LEVEL=1, /BUTTON_EVENTS, /EXPOSE_EVENTS, RETAIN=2, SCR_XSIZE=560, SCR_YSIZE=350)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Tab_Spectrum__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Tab_Spectrum, $
           INHERITS AmesPAHdbIDLSuite_GUI_Tab, $
           contents:0L, $
           list:0L, $
           blist:0L, $
           buttons:PTR_NEW(), $
           sdraw:0L, $
           icons:PTR_NEW(), $
           selection:PTR_NEW(), $
           uids:PTR_NEW()}
END

; END OF amespahdbidlsuite_gui_tab_spectrum__define.pro
