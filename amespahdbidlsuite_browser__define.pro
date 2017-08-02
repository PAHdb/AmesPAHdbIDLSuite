;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Browser
;
; CONTACT:
;
;    UPDATED VERSIONs of the NASA Ames PAH IR Spectroscopic
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Main class to hold the graphical browser interface
;
; CATEGORY:
;    GUI
;
; SUPERCLASSES:
;   NONE 
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;   AMESPAHDBIDLSUITE
;
; OPTIONAL:
;   NONE
;
; CREATION:
;    browser = OBJ_NEW('AmesPAHdbIDLSuite_Browser')
;    OBJ_DESTROY,gui
;
; METHODS
;    PUBLIC:
;      NONE
;
;    PRIVATE:
;      EVENTHANDLER
;      EVENTHANDLER__AMESPAHDBIDLSUITE_BROWSER (OUTSIDE CLASS)
;
; EXAMPLE
;    browser = OBJ_NEW('AmesPAHDBIDLSuite_Browser')
;    OBJ_DESTROY,browser
;
; MODIFICATION HISTORY
;
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

PRO AmesPAHdbIDLSuite_Browser::UpdateGeometryWindow

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  geometry = self.pahdb->GetGeometryByUID((*self.uids)[self.current])

  geometry->Diagonalize

  IF OBJ_VALID(self.grModel) THEN OBJ_DESTROY,self.grModel
        
  self.grModel = geometry->Structure(/OBJ)

  GET_BOUNDS,self.grModel,x,y,z

  max = 1.1 * MAX(ABS([x, y, z]))

  grAmbient = OBJ_NEW('IDLgrLight', COLOR=[255,255,255], INTENSITY=0.95)

  self.grModel->Add,grAmbient

  grDirectional = OBJ_NEW('IDLgrLight', TYPE=2, COLOR=[255,255,255], DIRECTION=[0,0,0], LOCATION=[0,1,0])

  self.grModel->Add,grDirectional

  self.grView->SetProperty,VIEWPLANE_RECT=[-max, -max, 2 * max, 2 * max], ZCLIP=[max, -max], EYE=2.5*max, COLOR=[192, 192, 192]

  self.grView->Add,self.grModel

  self.grWindow->Draw,self.grView

  OBJ_DESTROY,geometry
END

PRO Eventhandler__AmesPAHdbIDLSuite_Browser,Event

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  CALL_METHOD,'Eventhandler', !AMESPAHDBIDLSUITE_BROWSER_OBJ, Event
END

PRO AmesPAHdbIDLSuite_Browser::Eventhandler,Event

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  IF TAG_NAMES(Event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN

     OBJ_DESTROY,self

     RETURN
  ENDIF

  WIDGET_CONTROL,Event.id,GET_UVALUE=uval

  CASE uval OF

     'text' : BEGIN

        IF NOT OBJ_VALID(self.pahdb) THEN RETURN

        WIDGET_CONTROL,self.text,GET_VALUE=str,/HOURGLASS

        *self.uids = self.pahdb->Search(str[0])

        species = self.pahdb->GetSpeciesByUID(*self.uids)

        WIDGET_CONTROL,self.list,SET_VALUE=(species->Get()).data.formula

        OBJ_DESTROY,species        
     END

     'search' : BEGIN

        IF NOT OBJ_VALID(self.pahdb) THEN RETURN

        WIDGET_CONTROL,self.text,GET_VALUE=str,/HOURGLASS

        *self.uids = self.pahdb->Search(str[0], Count)

        IF Count EQ 0 THEN BEGIN

           WIDGET_CONTROL,self.list,SET_VALUE=['']

           ret = DIALOG_MESSAGE('No species found. Check your console for possible errors in your search syntax.', DIALOG_PARENT=self.base)

           RETURN

        ENDIF

        species = self.pahdb->GetSpeciesByUID(*self.uids)

        WIDGET_CONTROL,self.list,SET_VALUE=(species->get()).data.formula

        OBJ_DESTROY,species
     END

     'select' : BEGIN

        print,(*self.uids)[self.current]
     END
     
     'list'   : BEGIN

        IF NOT OBJ_VALID(self.pahdb) THEN RETURN

        WIDGET_CONTROL,/HOURGLASS

        self.current = Event.index

        spectrum = self.pahdb->GetTransitionsByUID((*self.uids)[self.current])

        spectrum->Plot

        *self.store = TVRD(/TRUE)

        self->UpdateGeometryWindow

        transitions = spectrum->get()

        n = N_ELEMENTS(transitions.data)

        str = STRARR(n+3)

        str[0] = STRING(FORMAT='(A7,2X,A8,2X,A8,2X,A4)', 'wave', 'freq.', 'I', 'sym')

        str[1] = STRING(FORMAT='(A7,2X,A8,2X,A8,2X,A4)', '[um]', '[cm^-1]', '[km/mol]','')

        str[2] = ''

        FOR i = 0, n - 1 DO str[i+3] = STRING(FORMAT='(G7.4,2X,G8.4,X,G9.4,2X,A4)', 1D4/transitions.data[i].frequency, transitions.data[i].frequency, transitions.data[i].intensity, transitions.data[i].symmetry)

        WIDGET_CONTROL,self.transitions,SET_VALUE=str

        species = self.pahdb->GetSpeciesByUID((*self.uids)[self.current])

        Str = ' '

        species->Print,Str=Str

        WIDGET_CONTROL,self.information,SET_VALUE=Str

        OBJ_DESTROY,[spectrum, species]
     END

     'spectrum'   : BEGIN

        IF NOT OBJ_VALID(self.pahdb) OR SIZE(*self.store, /TYPE) EQ 0 THEN RETURN

        IF Event.Press EQ 1 THEN *self.xy = [Event.x, Event.y]

        IF SIZE(*self.xy, /N_DIMENSIONS) NE 0 THEN BEGIN

           TV,*self.store,/TRUE

           PLOTS,(*self.xy)[0],(*self.xy)[1],/DEVICE

           PLOTS,[Event.x, Event.x, (*self.xy)[0], (*self.xy)[0]],[(*self.xy)[1], Event.y, Event.y, (*self.xy)[1]],/CONTINUE,/DEVICE,COLOR='00ff00'x
        ENDIF

        IF Event.Release EQ 1 THEN BEGIN

           spectrum = self.pahdb->GetTransitionsByUID((*self.uids)[self.current])

           coords = CONVERT_COORD([(*self.xy)[0], Event.x], [(*self.xy)[1], Event.y], /DEVICE, /TO_DATA)

           coords[1] *= 10D^(!AmesPAHdbIDLSuite_Plot_P.YORDER)
           
           IF coords[0,0] LT coords[0,1] THEN BEGIN

              tmp = coords[0,1]

              coords[0,1] = coords[0,0]

              coords[0,0] = tmp
           ENDIF

           IF coords[1,0] GT coords[1,1] THEN BEGIN

              tmp = coords[1,1]

              coords[1,1] = coords[1,0]

              coords[1,0] = tmp
           ENDIF

           IF coords[1,0] LT 0 THEN coords[1,0] = 0

           spectrum->Plot,XRANGE=coords[0,0:1],YRANGE=coords[1,0:1],/XSTYLE,/YSTYLE

           OBJ_DESTROY,spectrum

           *self.xy = 0

           *self.store = TVRD(/TRUE)
        ENDIF

        IF Event.Press EQ 4 THEN BEGIN

           spectrum = self.pahdb->GetTransitionsByUID((*self.uids)[self.current])

           spectrum->Plot

           OBJ_DESTROY,spectrum

           *self.store = TVRD(/TRUE)
        ENDIF
     END

     'molecule'   : BEGIN

        IF NOT OBJ_VALID(self.grModel) THEN RETURN

        updated = self.grTrackball->Update(Event, TRANSFORM=trackXform)

        IF (updated) THEN BEGIN
           
           self.grModel->GetProperty,TRANSFORM=modelXform
           
           self.grModel->SetProperty,TRANSFORM=modelXform # trackXform
           
           self.grWindow->Draw,self.grView         
       ENDIF
     END

     'open': BEGIN

        file = DIALOG_PICKFILE(/READ, FILTER=['*.xml'])

        IF file EQ '' THEN RETURN
        
        IF OBJ_VALID(self.pahdb) THEN OBJ_DESTROY,self.pahdb

        WIDGET_CONTROL,/HOURGLASS

        self.pahdb = OBJ_NEW('AmesPAHdbIDLSuite', Filename=file)

        IF NOT OBJ_VALID(self.pahdb) THEN RETURN

        species = self.pahdb->GetSpeciesByUID(-1)

        *self.uids = (species->get()).data.uid

        WIDGET_CONTROL,self.list,SET_VALUE=(species->get()).data.formula

        WIDGET_CONTROL,self.label,SET_VALUE=self.pahdb->GetVersion(/Str)

        OBJ_DESTROY,species
     END

     'exit': BEGIN

        OBJ_DESTROY,self
     END

     ELSE : MESSAGE,'EVENT '+uval+' NOT HANDLED',/INFORMATIONAL
  ENDCASE
END

PRO AmesPAHdbIDLSuite_Browser::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF WIDGET_INFO(self.base, /VALID_ID) EQ 1 THEN WIDGET_CONTROL,self.base,/DESTROY

  IF OBJ_VALID(self.pahdb) THEN OBJ_DESTROY,self.pahdb

  IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

  IF OBJ_VALID(self.grView) THEN OBJ_DESTROY,self.grView

  IF OBJ_VALID(self.grTrackball) THEN OBJ_DESTROY,self.grTrackball

  IF PTR_VALID(self.xy) THEN PTR_FREE,self.xy

  IF PTR_VALID(self.store) THEN PTR_FREE,self.store
END

FUNCTION AmesPAHdbIDLSuite_Browser::Init

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  self.base = WIDGET_BASE(TITLE='NASA Ames PAH IR Spectroscopic Database Browser', MBAR=mbar, /TLB_KILL_REQUEST_EVENTS, /COLUMN)

  file = WIDGET_BUTTON(mbar, VALUE='File', /MENU)

  open = WIDGET_BUTTON(file, VALUE='Open', UVALUE='open')

  exit = WIDGET_BUTTON(file, VALUE='Exit', UVALUE='exit')

  row1 = WIDGET_BASE(self.base, /ROW)

  self.text = WIDGET_TEXT(row1, UVALUE='text', XSIZE=78 , /EDITABLE, /NO_NEWLINE)

  button = WIDGET_BUTTON(row1, VALUE="Search", UVALUE='search', XSIZE=100)

  self.label = WIDGET_LABEL(row1, /ALIGN_RIGHT, XSIZE=350)

  row2 = WIDGET_BASE(self.base, /ROW)

  self.list = WIDGET_LIST(row2, XSIZE=11, UVALUE='list')

  spectrum = WIDGET_DRAW(row2, UVALUE='spectrum', XSIZE=500, YSIZE=500, /GRAPHICS_LEVEL, /BUTTON_EVENTS, /MOTION_EVENTS, /EXPOSE_EVENTS, RETAIN=2)

  self.uids = PTR_NEW(/ALLOCATE)

  self.xy = PTR_NEW(/ALLOCATE)

  self.store = PTR_NEW(/ALLOCATE)

  col21 = WIDGET_BASE(row2, /COL)

  molecule = WIDGET_DRAW(col21, UVALUE='molecule', XSIZE=200, YSIZE=200, GRAPHICS_LEVEL=2, /BUTTON_EVENTS, /MOTION_EVENTS, /EXPOSE_EVENTS, RETAIN=2)

  self.information = WIDGET_TEXT(col21, UVALUE='information', SCR_YSIZE=300, /SCROLL)

  self.transitions = WIDGET_TEXT(row2, UVALUE='transitions', XSIZE=33, /SCROLL)

  self.pahdb = OBJ_NEW('AmesPAHdbIDLSuite')

  IF OBJ_VALID(self.pahdb) THEN BEGIN

     species = self.pahdb->GetSpeciesByUID(-1)

     *self.uids = (species->get()).data.uid

     WIDGET_CONTROL,self.label,SET_VALUE=self.pahdb->GetVersion(/Str)

     WIDGET_CONTROL,self.list,SET_VALUE=(species->get()).data.formula

     OBJ_DESTROY,species
  ENDIF

  WIDGET_CONTROL,self.base,/REALIZE

  WIDGET_CONTROL,molecule,GET_VALUE=value

  self.grWindow = value

  self.grView = OBJ_NEW('IDLgrView', PROJECTION=2)

  self.grTrackball = OBJ_NEW('Trackball', [100, 100], 100)

  DEFSYSV,'!AMESPAHDBIDLSUITE_BROWSER_OBJ',self

  XMANAGER,'AmesPAHdbIDLSuite_Browser::Init',self.base,EVENT_HANDLER='Eventhandler__AmesPAHdbIDLSuite_Browser',/NO_BLOCK

  RETURN,1
END

PRO AmesPAHdbIDLSuite_Browser__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Browser,$
          base:0, $
          list:0, $
          text:0, $
          transitions:0, $
          information:0, $
          label:0, $
          grWindow:OBJ_NEW(), $
          grView:OBJ_NEW(), $
          grModel:OBJ_NEW(), $
          grTrackball:OBJ_NEW(), $
          pahdb:OBJ_NEW(), $
          uids:PTR_NEW(), $
          current:0L, $
          xy:PTR_NEW(), $
          store:PTR_NEW()}
END

; END OF amespahdbidlsuite_browser__define.pro
