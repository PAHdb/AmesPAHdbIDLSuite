;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Plot
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Main class to handle plotting
;
; CATEGORY:
;    PLOT
;
; SUPERCLASSES:
;   NONE 
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;
; OPTIONAL:
;   NONE
;
; CREATION:
;    data = OBJ_NEW('AmesPAHdbIDLSuite_Plot')
;
; METHODS
;    PUBLIC:
;      SETUP (PROCEDURE)
;      RESTORE (PROCEDURE)
;      PLOT (PROCEDURE)
;      OPLOT (PROCEDURE)
;      OPLOTERR (PROCEDURE)
;      LEGEND (PROCEDURE)
;
; PRIVATE:
;
; EXAMPLE
;    plot = OBJ_NEW('AmesPAHdbIDLSuite_Plot')
;    OBJ_DESTROY,plot
;
; MODIFICATION HISTORY
;
;   05-03-2015
;   Use Courier-font for LEGEND. Christiaan Boersma.
;   04-08-2015
;   Silenced LOADCT. Christiaan Boersma.
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

;; PLOT

PRO AmesPAHdbIDLSuite_Plot::Plot,x,y,Yrange=yrange,Stick=Stick,Fill=Fill,NoData=NoData,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  IF NOT KEYWORD_SET(Yrange) THEN BEGIN

     ymax = MAX(ABS(y), imax)

     sign = y[imax] GT 0 ? 1 : -1
     
     !AmesPAHdbIDLSuite_Plot_P.YORDER = FLOOR(ALOG10(sign * ymax))
  ENDIF ELSE BEGIN

     ymax = MAX(ABS(Yrange), imax)
     
     sign = Yrange[imax] GT 0 ? 1 : -1
 
     !AmesPAHdbIDLSuite_Plot_P.YORDER = FLOOR(ALOG10(sign * Yrange[imax]))

     yrange *= 10D^(-1 * !AmesPAHdbIDLSuite_Plot_P.YORDER)
  ENDELSE
    
  _y = 10D^(-!AmesPAHdbIDLSuite_Plot_P.YORDER) * y
  
  PLOT,x,_y,/NODATA,YRANGE=yrange,_EXTRA=EXTRA

  !AmesPAHdbIDLSuite_Plot_P.CLIP = !P.CLIP
  
  IF NOT KEYWORD_SET(NoData) THEN BEGIN
  
     IF KEYWORD_SET(Stick) THEN BEGIN

        nx = N_ELEMENTS(x)
     
        FOR i = 0L, nx - 1 DO OPLOT,x[i]*[1,1],[0,_y[i]],YRANGE=yrange,_EXTRA=EXTRA
     ENDIF ELSE IF KEYWORD_SET(Fill) THEN POLYFILL,[x, [x0]],[_y, _y[0]],/DATA,_EXTRA=EXTRA $
     ELSE OPLOT,x,_y,_EXTRA=EXTRA
  ENDIF

  IF NOT KEYWORD_SET(Oplot) AND !AmesPAHdbIDLSuite_Plot_P.YORDER NE 0 THEN BEGIN

     xy = CONVERT_COORD([!X.WINDOW[0], !Y.WINDOW[1]], /NORMAL, /TO_DEVICE)
     
     XYOUTS,xy[0]+3*!D.X_CH_SIZE,xy[1]-3*!D.Y_CH_SIZE,STRING(FORMAT='("(x10!U",I-0,"!N)")', !AmesPAHdbIDLSuite_Plot_P.YORDER),/DEVICE,CHARSIZE=1.5
  ENDIF
END

PRO AmesPAHdbIDLSuite_Plot::Oplot,x,y,Stick=Stick,Fill=Fill,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  !P.CLIP = !AmesPAHdbIDLSuite_Plot_P.CLIP
 
  _y = 10D^(-!AmesPAHdbIDLSuite_Plot_P.YORDER) * y
   
  IF KEYWORD_SET(Stick) THEN BEGIN

     nx = N_ELEMENTS(x)
     
     FOR i = 0L, nx - 1 DO OPLOT,x[i]*[1,1],[0,_y[i]],_EXTRA=EXTRA
  ENDIF ELSE IF KEYWORD_SET(Fill) THEN POLYFILL,[x,x[0]],[_y,_y[0]],/DATA,_EXTRA=EXTRA $
  ELSE OPLOT,x,_y,_EXTRA=EXTRA  
END

PRO AmesPAHdbIDLSuite_Plot::OplotError,x,y,ystdev,xstdev,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  !P.CLIP = !AmesPAHdbIDLSuite_Plot_P.CLIP
  
  nx = N_ELEMENTS(x)
  
  w = 0.01 * (!X.WINDOW[1] - !X.WINDOW[0]) * !D.X_SIZE

  _y = 10D^(-!AmesPAHdbIDLSuite_Plot_P.YORDER) * y

  _ystdev = 10D^(-!AmesPAHdbIDLSuite_Plot_P.YORDER) * ystdev
  
  FOR i = 0, nx - 1 DO BEGIN

     IF ystdev[i] EQ 0 THEN CONTINUE
     
     xy0 = CONVERT_COORD(x[i], _y[i]-_ystdev[i], /DATA, /TO_DEVICE)

     xy1 = CONVERT_COORD(x[i], _y[i]+_ystdev[i], /DATA, /TO_DEVICE)

     PLOTS,[xy0[0]+[-w, w, 0],xy1[0]+[0, -w, w]],[REPLICATE(xy0[1], 3),REPLICATE(xy1[1], 3)],/DEVICE,_EXTRA=EXTRA
  ENDFOR

  IF N_PARAMS() GT 3 THEN BEGIN
  
     h = 0.01 * (!Y.WINDOW[1] - !Y.WINDOW[0]) * !D.Y_SIZE

     FOR i = 0, nx - 1 DO BEGIN

        IF xstdev[i] EQ 0 THEN CONTINUE
     
        xy0 = CONVERT_COORD(x[i]-xstdev[i], _y[i], /DATA, /TO_DEVICE)

        xy1 = CONVERT_COORD(x[i]+xstdev[i], _y[i], /DATA, /TO_DEVICE)

        PLOTS,[REPLICATE(xy0[0], 3),REPLICATE(xy1[0], 3)],[xy0[1]+[-h,h,0],xy1[1]+[0,-h,h]],/DEVICE,_EXTRA=EXTRA 
     ENDFOR
  ENDIF
END

;; LEGEND

PRO AmesPAHdbIDLSuite_Plot::Legend,Str,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  maxlen = MAX(STRLEN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX)))

  IF !P.FONT EQ 0 THEN maxlen *= 1.4
  
  xy = CONVERT_COORD([!X.WINDOW[1], !Y.WINDOW[1]], /NORMAL, /TO_DEVICE)

  XYOUTS,xy[0]-maxlen*!D.X_CH_SIZE,xy[1]-3*!D.Y_CH_SIZE,"!11"+Str+"!3",CHARSIZE=1.25,/DEVICE
END

;; PLOT SETUP/RESTORE

PRO AmesPAHdbIDLSuite_Plot::Setup,Oplot=Oplot,WindowTitle=WindowTitle,XSIZE=xsize,YSIZE=ysize,_EXTRA=EXTRA

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.p) THEN PTR_FREE,self.p

  self.p = PTR_NEW(!P)

  !P.CHARSIZE = 2

  IF !D.NAME EQ 'X' THEN !P.FONT = 1 $
  ELSE IF !D.NAME EQ 'PS' THEN !P.FONT = 0

  IF !D.NAME EQ 'X' AND !D.WINDOW NE 32 THEN BEGIN

     DEVICE,GET_DECOMPOSED=decomposed

     self.decomposed = decomposed
     
     DEVICE,DECOMPOSE=0

     IF NOT KEYWORD_SET(Oplot) THEN BEGIN

        IF NOT KEYWORD_SET(XSIZE) THEN xsize = 400

        IF NOT KEYWORD_SET(YSIZE) THEN ysize = xsize
     
        IF !D.WINDOW NE -1 THEN BEGIN

           DEVICE,GET_WINDOW_POSITION=window_position

           window_position[1] += 21

           win = !D.WINDOW
        ENDIF ELSE BEGIN

           DEVICE,GET_SCREEN_SIZE=screen_size
        
           window_position = (screen_size - [xsize, ysize]) / 2

           win = 0
        ENDELSE
        
        WINDOW,win,TITLE=OBJ_CLASS(self),XSIZE=XSIZE,YSIZE=YSIZE,XPOS=window_position[0],YPOS=window_position[1]
     ENDIF
  ENDIF

  TVLCT,red,green,blue,/GET

  IF PTR_VALID(self.red) THEN PTR_FREE,self.red

  self.red = PTR_NEW(red)

  IF PTR_VALID(self.green) THEN PTR_FREE,self.green

  self.green = PTR_NEW(green)

  IF PTR_VALID(self.blue) THEN PTR_FREE,self.blue

  self.blue = PTR_NEW(blue)
  
  LOADCT,0,/SILENT

  red = 255 * [0,1,1,0,0,0.8,0,1,1.0,0.3,0.7,1.0,0.7,0.3,0.5,0.18,0.5,1,0]

  green = 255 * [0,1,0,0.5,0,0.8,1,0,0.7,1.0,0.3,0.3,1.0,0.7,0.5,0.55,0,0.27,0]

  blue = 255 * [0,1,0,0,1,0,1,1,0.3,0.7,1.0,0.7,0.3,1.0,0.5,0.34,0,0,0.8]

  TVLCT,red,green,blue

  !P.BACKGROUND = 1

  !P.COLOR = 0
END

PRO AmesPAHdbIDLSuite_Plot::Restore

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF !D.NAME EQ 'X' THEN DEVICE,DECOMPOSE=self.decomposed
  
  IF PTR_VALID(self.p) THEN !P = *self.p

  IF PTR_VALID(self.red) AND $
     PTR_VALID(self.green) AND $
     PTR_VALID(self.blue) THEN TVLCT,*self.red,*self.green,*self.blue
END

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite_Plot::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.p) THEN PTR_FREE,self.p

  IF PTR_VALID(self.red) THEN PTR_FREE,self.red

  IF PTR_VALID(self.green) THEN PTR_FREE,self.green

  IF PTR_VALID(self.blue) THEN PTR_FREE,self.blue
END

FUNCTION AmesPAHdbIDLSuite_Plot::Init

  COMPILE_OPT IDL2

  ON_ERROR,2

  DEFSYSV, '!AmesPAHdbIDLSuite_Plot_P', EXISTS=exists

  IF exists EQ 0 THEN DEFSYSV, '!AmesPAHdbIDLSuite_Plot_P', $
                               {YORDER:0D, $
                                CLIP:INTARR(6)}
  RETURN,1
END

PRO AmesPAHdbIDLSuite_Plot__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  void = {AmesPAHdbIDLSuite_Plot, $
          decomposed:0, $
          p:PTR_NEW(), $
          red:PTR_NEW(), $
          green:PTR_NEW(), $
          blue:PTR_NEW()}
END

; END OF amespahdbidlsuite_plot__define.pro
