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


PRO AmesPAHdbIDLSuite_GUI_Window::addTab,tab

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  IF PTR_VALID(self.tabs) THEN BEGIN

     *self.tabs = [*self.tabs, tab]

     RETURN
  ENDIF

  self.tabs = PTR_NEW([tab])
END

PRO AmesPAHdbIDLSuite_GUI_Window::CreateMBar

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  self.file = WIDGET_BUTTON(self.mbar, VALUE='File', /MENU)

  self.exit = WIDGET_BUTTON(self.file, VALUE='Exit')
END

PRO AmesPAHdbIDLSuite_GUI_Window::EventHandler,Event

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  IF Event.id EQ self.tbar THEN BEGIN

     (*self.tabs)[Event.tab]->Realize

     RETURN
  ENDIF

  IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_TLB_MOVE' THEN BEGIN

     WIDGET_CONTROL,self.base,XOFFSET=Event.x,YOFFSET=Event.y
  ENDIF  ELSE IF TAG_NAMES(EVENT, /STRUCTURE_NAME) EQ 'WIDGET_TIMER' THEN BEGIN

     WIDGET_CONTROL,self.pbar,GET_VALUE=wid

     WSET,wid

     ERASE,192
  ENDIF ELSE IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' OR Event.id EQ self.exit THEN BEGIN

     OBJ_DESTROY,self
  ENDIF
END

PRO Eventhandler__AmesPAHdbIDLSuite_GUI_Window,Event

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  WIDGET_CONTROL,Event.handler,GET_UVALUE=obj

  CALL_METHOD,'EventHandler', obj, Event
END

PRO AmesPAHdbIDLSuite_GUI_Window::Manage

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  XMANAGER,'AmesPAHdbIDLSuite_GUI_Window',self.base,EVENT_HANDLER='Eventhandler__AmesPAHdbIDLSuite_GUI_Window',/NO_BLOCK
END

PRO AmesPAHdbIDLSuite_GUI_Window::Realize

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  DEVICE,GET_SCREEN_SIZE=screensize

  info = WIDGET_INFO(self.base, /GEOMETRY)

  WIDGET_CONTROL,self.base,XOFFSET=(screensize[0]-info.scr_xsize)/2,YOFFSET=(screensize[1]-info.scr_ysize)/2

  xsize = info.scr_xsize-2*info.xpad

  WIDGET_CONTROL,self.mlabel,SCR_XSIZE=0.8*xsize

  WIDGET_CONTROL,self.pbar,SCR_XSIZE=0.2*xsize,XOFFSET=0.8*xsize,GET_VALUE=wid

  WIDGET_CONTROL,self.base,/REALIZE

  WSET,wid

  ERASE,192

  IF PTR_VALID(self.tabs) THEN BEGIN

     ntabs = N_ELEMENTS(*self.tabs)

     FOR i = 0, ntabs - 1 DO (*self.tabs)[i]->Realize
  ENDIF
END

PRO AmesPAHdbIDLSuite_GUI_Window::SetProgress,progress,Message=Message

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  WIDGET_CONTROL,self.pbar,GET_VALUE=wid

  WSET,wid

  ERASE,192

  info = WIDGET_INFO(self.pbar, /GEOMETRY)

  xoffset = progress * info.scr_xsize

  POLYFILL,[0,0,xoffset,xoffset,0],[0,info.scr_ysize,info.scr_ysize,0,0],/DEVICE,COLOR=30,/LINE_FILL

  IF NOT KEYWORD_SET(Message) THEN Message = ''

  XYOUTS,(info.scr_xsize)/2,info.scr_ysize/2-5,STRING(FORMAT='(A0,I0,"%")',Message,100 * progress),ALIGN=0.5,/DEVICE,COLOR=1,CHARSIZE=1

  IF progress EQ 1.0 THEN WIDGET_CONTROL,self.base,TIMER=0.5
END

PRO AmesPAHdbIDLSuite_GUI_Window::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->AmesPAHdbIDLSuite_GUI_View::Cleanup

  IF WIDGET_INFO(self.base, /VALID_ID) EQ 1 THEN WIDGET_CONTROL,self.base,/DESTROY

  IF PTR_VALID(self.tabs) THEN PTR_FREE,self.tabs
END

FUNCTION AmesPAHdbIDLSuite_GUI_Window::Init

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  IF XREGISTERED('AmesPAHdbIDLSuite_GUI_Window') NE 0 THEN RETURN,0

  IF self->AmesPAHdbIDLSuite_GUI_View::Init() EQ 0 THEN RETURN,0

  self.base = WIDGET_BASE(MBAR=mbar, /TLB_KILL_REQUEST_EVENTS, /TLB_MOVE_EVENTS, /COL, UVALUE=self)

  self.mbar = mbar

  self->CreateMBar

  self.tbar = WIDGET_TAB(self.base)

  self.sbar = WIDGET_BASE(self.base, FRAME=1)

  self.mlabel = WIDGET_LABEL(self.sbar, VALUE=" ", /ALIGN_LEFT)

  info = WIDGET_INFO(self.mlabel, /GEOMETRY)

  self.pbar = WIDGET_DRAW(self.sbar, GRAPHICS_LEVEL=1, RETAIN=2, SCR_YSIZE=info.scr_ysize)

  RETURN,1
END

PRO AmesPAHdbIDLSuite_GUI_Window__DEFINE,class

  COMPILE_OPT IDL2

  ON_ERROR,2

  class = {AmesPAHdbIDLSuite_GUI_Window,$
           INHERITS AmesPAHdbIDLSuite_GUI_View, $
           mbar:0L, $
           file:0L, $
           exit:0L, $
           tbar:0L, $
           tabs:PTR_NEW(), $
           sbar:0L, $
           mlabel:0L, $
           pbar:0L}
END

; END OF amespahdbidlsuite_gui_window__define.pro
