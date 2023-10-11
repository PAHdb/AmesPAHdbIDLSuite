; docformat = 'rst'

;+
;
; Class used to hold the PAH geometry data
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Create and destroy an
;   AmesPAHdbIDLSuite_Geometry-instance::
;
;     IDL> geo = OBJ_NEW('AmesPAHdbIDLSuite_Geometry')
;     IDL> geo->Set,data
;     IDL> geo->Plot
;     IDL> OBJ_DESTROY,geo
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
;     05-02-2023
;     Accommodate UIDs >9999 in PLOT. Christiaan Boersma.
;     09-20-2022
;     Use CPK codes for coloring atoms. Christiaan Boersma.
;     06-22-2022
;     Some simplifications STRUCTURE. Christiaan Boersma.
;     06-11-2022
;     Fix INERTIA computations. Christiaan Boersma.
;     10-03-2021
;     Added DESCRIPTION to avoid crash on calling HELP. Christiaan
;     Boersma.
;     29-02-2021
;     Saving PNG-files now works for species with UIDs larger than 999.
;     10-10-2017
;     Refactored and now using LA_TRIQL and LA_TRIRED instead of TRIQL
;     and TRIRED in DIAGONALIZE. Christiaan Boersma.
;     09-18-2017
;     Updated GETUIDSCOMPLETECHARGESET to also check equality in number
;     of rings in and fixed typo in RINGS in.
;     04-08-2015
;     Fixed memory leak in IDL_Container; moved to IDLgrImage
;     instead. Christiaan Boersma
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;-
;+
;  Output geometry description.
;
;  :Params:
;     Str: out, optional, type="string array"
;       Ouput to Str
;
; :Categories:
;   INFORMATIVE
;-
PRO AmesPAHdbIDLSuite_Geometry::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  Str = ""

  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,""
END

;+
;  Plot the chemical structure.
;
;  :Params:
;    UID: in, optional, type=long
;      UID of the PAH to plot
;
; :Returns:
;   byte array
;
;  :Keywords:
;
;    NoErase: in, optional, type=int
;      Whether to erase the output before plotting
;    Resolution: in, optional, type="integer array"
;      Resolution in pixels of the output
;    Save: in, optional, type=int
;      Wether to save the plot to file (PNG)
;    Position: in, optional, type="float array"
;      Position in normalized coordinates to place the plot
;    Scale: in, optional, type=float
;      Scaling factor for drawing atoms
;    Thick: in, optional, type=float
;      Thickness for drawing bonds
;    RGB: in, optional, type=int
;      Whether to use decomposed colors
;    Angle: in, optional, type=flot
;      Rotation angle along the z-axis
;
; :Categories:
;   PLOTTING
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Plot,UID,NoErase=NoErase,Resolution=Resolution,Save=Save,Position=Position,Scale=Scale,Thick=Thick,RGB=RGB,Angle=Angle

  COMPILE_OPT IDL2

  ON_ERROR,2

  _p = !P

  _d = !D.NAME

  IF _d EQ 'X' AND !D.WINDOW NE 35 THEN WINDOW,0,TITLE=OBJ_CLASS(self),XSIZE=400,YSIZE=400

  TVLCT,_r,_g,_b,/GET

  LOADCT,0

  r = 255 * [0,1,1,0,0,1,0,1,1.0,0.3,0.7,1.0,0.7,0.3,0.5,0.18,0.5,1,0]

  g = 255 * [0,1,0,1,0,1,1,0,0.7,1.0,0.3,0.3,1.0,0.7,0.5,0.55,0,0.27,0]

  b = 255 * [0,1,0,0,1,0,1,1,0.3,0.7,1.0,0.7,0.3,1.0,0.5,0.34,0,0,0.8]

  TVLCT,r,g,b

  !P.BACKGROUND = 1

  !P.COLOR = 0

  IF N_PARAMS() EQ 0 THEN BEGIN

     IF self.current EQ self.nuids THEN self.current = 0

     UID = (*self.uids)[self.current++]
  ENDIF ELSE IF WHERE(*self.uids EQ UID) EQ -1 THEN UID = (*self.uids)[0]

  IF NOT KEYWORD_SET(Scale) THEN Scale = 1.0

  IF NOT KEYWORD_SET(Thick) THEN Thick = 4.0

  IF KEYWORD_SET(Resolution) THEN BEGIN

     SET_PLOT,'Z'

     DEVICE,SET_RESOLUTION=Resolution
  ENDIF

  atom_names =   ['H', 'C', 'N', 'O', 'Mg', 'Si', 'Fe']

  atom_numbers = [ 1,   6,   7,   8,   12,   14,   26 ]

  atom_colors =  [14,   0,   4,   2,    3 ,  11,    8 ]

  atom_symsize = [ 1,   2,   3,   3,  3.5,    4,    4 ] * Scale

  natom_names = N_ELEMENTS(atom_names)

  IF KEYWORD_SET(RGB) THEN BEGIN

     TVLCT,red,green,blue,/GET

     atom_colors = LONG(REFORM([TRANSPOSE(red[atom_colors]), TRANSPOSE(green[atom_colors]), TRANSPOSE(blue[atom_colors]), TRANSPOSE(BYTARR(natom_names))], natom_names * 4), 0, natom_names)
  ENDIF

  select = WHERE((*self.data).uid EQ UID[0], nselect)

  plotx = (*self.data)[select].x

  ploty = (*self.data)[select].y

  plotz = (*self.data)[select].z

  IF KEYWORD_SET(Angle) THEN BEGIN

     T3D,/RESET

     T3D,ROTATE=[0, 0, Angle],MATRIX=rotmat

     FOR i = 0, nselect - 1 DO BEGIN

        coords =  [plotx[i], ploty[i], plotz[i], 1.0] # rotmat

        plotx[i] = coords[0]

        ploty[i] = coords[1]

        plotz[i] = coords[2]
     ENDFOR
  ENDIF

  max = MAX(ABS([plotx, ploty]))

  range = 1.05 * [-max, max]

  PLOT,plotx,ploty,XRANGE=range,YRANGE=range,XSTYLE=7,YSTYLE=7,XMARGIN=[0,0],ymargin=[0,0],/NODATA,NOERASE=Noerase,POSITION=Position

  numn = INTARR(nselect)

  nlist = MAKE_ARRAY(nselect, 6, VALUE=-1)

  FOR i = 0, nselect - 1 DO BEGIN

     dd = SQRT((plotx - plotx[i])^2 + (ploty - ploty[i])^2 + (plotz - plotz[i])^2)

     sel = WHERE(dd GT 0D AND dd LT 1.6D, nsel)

     numn[i] = nsel

     nlist[i, 0:nsel-1] = sel
  ENDFOR

  FOR i = 0, nselect - 1 DO BEGIN

     FOR j = 0, numn[i] - 1 DO BEGIN

        OPLOT,[plotx[i], plotx[nlist[i, j]]],[ploty[i], ploty[nlist[i, j]]]

        IF --numn[nlist[i, j]] GT 0 THEN BEGIN

           nlist[nlist[i, j], WHERE(nlist[nlist[i, j], *] EQ i)] = -1

           nlist[nlist[i, j], *] = nlist[nlist[i, j], REVERSE(SORT(nlist[nlist[i, j], *]))]
        ENDIF
     ENDFOR
  ENDFOR

  PLOTSYM,0,/FILL

  FOR i = 0, natom_names - 1 DO BEGIN

     ii = WHERE((*self.data)[select].type EQ atom_numbers[i], count)

     IF count GT 0 THEN OPLOT,plotx[ii],ploty[ii],PSYM=8,SYMSIZE=atom_symsize[i],COLOR=atom_colors[i, *]
  ENDFOR

  img = 1

  IF KEYWORD_SET(Resolution) THEN BEGIN

     img = TVRD(TRUE=true)

     IF KEYWORD_SET(Save) THEN BEGIN

        TVLCT,r,g,b,/GET

        dim = SIZE(img, /DIM)

        img_byte = BYTSCL(img)

        img_3D = BYTARR(3, dim[0], dim[1])

        img_3D[0,*,*] = r[img_byte]

        img_3D[1,*,*] = g[img_byte]

        img_3D[2,*,*] = b[img_byte]

        WRITE_PNG,STRING(FORMAT='(A0,"_UID",I05,".png")', ID),OBJ_CLASS(self),img_3D
     ENDIF

     SET_PLOT,_d

     TV,img,TRUE=true
  ENDIF

  TVLCT,_r,_g,_b

  !P = _p

  RETURN,img
END

;+
;  Render chemical structure.
;
;  :Params:
;    UID: in, optional, type=long
;      UID of the PAH to plot
;
; :Returns:
;   byte array or IDLgrModel
;
;  :Keywords:
;
;    Background: in, optional, type="byte array"
;      Background color
;    Save: in, optional, type=int
;      Wether to save the plot to file (PNG)
;    Resolution: in, optional, type="integer array"
;      Resolution in pixels of the output
;    View: in, optional, type=int
;      Whether to open an interactive display to
;      view the structure
;    Axis: in, optional, type="int array"
;      Axis of rotation
;    Angle: in, optional, type=flot
;      Rotation angle along the defined axis
;    Obj: in, optional, type=object
;      Return the rendering as an IDLgrModel
;    Transparent: in, optional, type="byte array"
;      Transparent color
;    Frame: in, optional, type=float
;      Render as wire frame
;
; :Categories:
;   PLOTTING
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Structure,UID,Background=Background,Save=Save,Resolution=Resolution,View=View,Axis=Axis,Angle=Angle,Obj=Obj,Transparent=Transparent,Frame=Frame

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() EQ 0 THEN BEGIN

     IF self.current EQ self.nuids THEN self.current = 0

     UID = (*self.uids)[self.current++]
  ENDIF ELSE IF WHERE(*self.uids EQ UID) EQ -1 THEN UID = (*self.uids)[0]

  atom_names =   ['H', 'C', 'N', 'O', 'Mg', 'Si', 'Fe']

  atom_numbers = [ 1,   6,   7,   8,   12,   14,   26 ]

  atom_colors =  [[200,200,200], [27,27,27], [48,80,248], [255,13,13], [138,255,0], [240,200,160], [224,102,51]]

  atom_symsize = [0.2, 0.4, 0.6, 0.6, 0.7, 0.8, 0.8]

  natom_names = N_ELEMENTS(atom_names)

  select = WHERE((*self.data).uid EQ UID[0], nselect)

  plotx = (*self.data)[select].x

  ploty = (*self.data)[select].y

  plotz = (*self.data)[select].z

  max = 1.2 * MAX(ABS([plotx, ploty, plotz]))

  grModel = OBJ_NEW('IDLgrModel')

;;;

  numn = INTARR(nselect)

  nlist = MAKE_ARRAY(nselect, 6, VALUE=-1)

  FOR i = 0, nselect - 1 DO BEGIN

     dd = SQRT((plotx - plotx[i])^2 + (ploty - ploty[i])^2 + (plotz - plotz[i])^2)

     sel = WHERE(dd GT 0D AND dd LT 1.6D, nsel)

     numn[i] = nsel

     IF nsel EQ 0 THEN CONTINUE

       IF nsel GT 6 THEN BEGIN
         idx = SORT(dd[sel])
         sel = sel[idx[0:5]]
         nsel = 6
       ENDIF

     nlist[i, 0:nsel-1] = sel
  ENDFOR

  FOR i = 0, nselect - 1 DO BEGIN

     FOR j = 0, numn[i] - 1 DO BEGIN

        IF --numn[nlist[i, j]] GT 0 THEN BEGIN

           nlist[nlist[i, j], WHERE(nlist[nlist[i, j], *] EQ i)] = -1

           nlist[nlist[i, j], *] = nlist[nlist[i, j], REVERSE(SORT(nlist[nlist[i, j], *]))]
        ENDIF

        if numn[nlist[i, j]] LT 0 THEN CONTINUE

        vec = [plotx[i] - plotx[nlist[i, j]], ploty[i] - ploty[nlist[i, j]], plotz[i] - plotz[nlist[i, j]]]

        norm = NORM(vec)

        rotAxis = [vec[1], -vec[0], 0D]

        rotAngle = 180D * ACOS(-vec[2] / norm) / !DPI

        IF (*self.data)[select[i]].type EQ 1 OR (*self.data)[select[nlist[i, j]]].type EQ 1 THEN bcolor = atom_colors[*, 0] $
        ELSE bcolor = [0, 0, 0]

        IF rotAngle GT 179.99 THEN norm = -norm

        grBondModel = OBJ_NEW('IDLgrModel')

        MESH_OBJ,3,vertex,poly,REPLICATE(0.075,25,25),P3=0,P4=norm

        grBond = OBJ_NEW('IDLgrPolygon', DATA=vertex, POLYGONS=poly, /SHADING, COLOR=bcolor, ALPHA_CHANNEL=1.0, SHININESS=24)

        grBondModel->Add,grBond

        grBondModel->Rotate,rotAxis,rotAngle

        grBondModel->Translate,plotx[i],ploty[i],plotz[i]

        grModel->Add,grBondModel
     ENDFOR
  ENDFOR

  IF NOT KEYWORD_SET(Frame) THEN BEGIN

     FOR i = 0, natom_names - 1 DO BEGIN

        ii = WHERE((*self.data)[select].type EQ atom_numbers[i], count)

        IF count GT 0 THEN BEGIN

           MESH_OBJ,4,vertex,poly,REPLICATE(atom_symsize[i],25,25),/CLOSED

           FOR iii = 0, count - 1 DO BEGIN

              T3D,/RESET

              T3D,TRANSLATE=[plotx[ii[iii]], ploty[ii[iii]], plotz[ii[iii]]]

              grAtom = OBJ_NEW('IDLgrPolygon', DATA=VERT_T3D(vertex), POLYGONS=poly, SHADING=1, COLOR=atom_colors[*, i], ALPHA_CHANNEL=1.0, SHININESS=24)

              grModel->Add,grAtom
           ENDFOR
        ENDIF
     ENDFOR
  ENDIF

  IF KEYWORD_SET(Obj) THEN RETURN,grModel

  IF NOT KEYWORD_SET(Resolution) THEN Resolution = [512, 512]

  IF NOT KEYWORD_SET(Background) THEN Background = [60, 60, 60]

  grBuffer = OBJ_NEW('IDLgrBuffer', DIMENSIONS=Resolution)

  grView = OBJ_NEW('IDLgrView', PROJECTION=2, COLOR=Background, VIEWPLANE_RECT=[-max, -max, 2 * max, 2 * max], ZCLIP=[max, -max], EYE=2.5*max)

  grAmbient = OBJ_NEW('IDLgrLight', COLOR=[255,255,255], INTENSITY=0.95)

  grModel->Add,grAmbient

  grDirectional = OBJ_NEW('IDLgrLight', TYPE=2, COLOR=[255,255,255], DIRECTION=[0,0,0], LOCATION=[0,1,0])

  grModel->Add,grDirectional

  IF KEYWORD_SET(Axis) AND KEYWORD_SET(Angle) THEN grModel->Rotate,Axis,Angle

  grView->Add,grModel

  grBuffer->Draw,grView

  IF KEYWORD_SET(View) THEN XOBJVIEW,grModel,BACKGROUND=Background,XSIZE=800,YSIZE=800,/MODAL

  grImage = grBuffer->Read()

  grImage->GetProperty,DATA=image

  IF KEYWORD_SET(Save) THEN BEGIN

     IF KEYWORD_SET(Transparent) THEN BEGIN

        r = image[0, *, *]

        g = image[1, *, *]

        b = image[2, *, *]

        a = BYTARR([1, Resolution]) + 255B

        trans = WHERE(r EQ Background[0] AND g EQ Background[1] AND b EQ Background[2], ntrans)

        IF NTRANS GT 0 THEN BEGIN

           a[trans] = 0B

           image = [image, a]
        ENDIF
     ENDIF

     WRITE_PNG,STRING(FORMAT='(I04,".png")', UID),image
  ENDIF

  OBJ_DESTROY,[grBuffer, grView, grImage]

  RETURN,image
END

;+
; Calculates the total mass for each PAH.
;
; :Returns:
;   struct array
;
; :Categories:
;   CALCULATE
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Mass

  COMPILE_OPT IDL2

  ON_ERROR,2

  atomic_masses = [0.0D, 1.007940, 4.002602, 6.941000, 9.012182, 10.811000, 12.011000, 14.006740, 15.999400, 18.998404, 20.179701, 22.989767, 24.305000, 26.981539, 28.085501, 30.973763, 32.066002, 35.452702, 39.948002, 39.098301, 40.077999, 44.955910, 47.880001, 50.941502, 51.996101, 54.938049, 55.847000, 58.933201, 58.693401, 63.546001, 65.389999, 69.723000, 72.610001, 74.921593, 78.959999, 79.903999, 83.800003, 85.467796, 87.620003, 88.905853, 91.223999, 92.906380, 95.940002, 98.000000, 101.070000, 102.905502, 106.419998, 107.868202, 112.411003, 114.820000, 118.709999, 121.757004, 127.599998, 126.904472, 131.289993, 132.905426, 137.326996, 138.905502, 140.115005, 140.907654, 144.240005, 145.000000, 150.360001, 151.964996, 157.250000, 158.925339, 162.500000, 164.930313, 167.259995, 168.934204, 173.039993, 174.966995, 178.490005, 180.947906, 183.850006, 186.207001, 190.199997, 192.220001, 195.080002, 196.966537, 200.589996, 204.383301, 207.199997, 208.980377, 209.000000, 210.000000, 222.000000, 223.000000, 226.024994, 227.028000, 232.038101, 231.035904, 238.028900, 237.048004, 244.000000, 243.000000, 247.000000, 247.000000, 251.000000, 252.000000, 257.000000, 258.000000, 259.000000, 262.000000, 261.000000, 262.000000, 263.000000, 262.000000, 265.000000, 266.000000]

  mass = REPLICATE({AmesPAHdb_Geometry_Mass_S, uid:0, mass:0D}, self.nuids)

  mass.uid = *self.uids

  FOR i = 0, self.nuids - 1 DO BEGIN

     select = WHERE((*self.data).uid EQ (*self.uids)[i])

     mass[i].mass = TOTAL(atomic_masses[(*self.data)[select].type])
  ENDFOR

  RETURN,mass
END

;+
; Calculates the moment of inertia for each PAH.
;
; :Returns:
;   struct array
;
; :Categories:
;   CALCULATE
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Inertia

  COMPILE_OPT IDL2

  ON_ERROR,2

  atomic_masses = [0.0D, 1.007940, 4.002602, 6.941000, 9.012182, 10.811000, 12.011000, 14.006740, 15.999400, 18.998404, 20.179701, 22.989767, 24.305000, 26.981539, 28.085501, 30.973763, 32.066002, 35.452702, 39.948002, 39.098301, 40.077999, 44.955910, 47.880001, 50.941502, 51.996101, 54.938049, 55.847000, 58.933201, 58.693401, 63.546001, 65.389999, 69.723000, 72.610001, 74.921593, 78.959999, 79.903999, 83.800003, 85.467796, 87.620003, 88.905853, 91.223999, 92.906380, 95.940002, 98.000000, 101.070000, 102.905502, 106.419998, 107.868202, 112.411003, 114.820000, 118.709999, 121.757004, 127.599998, 126.904472, 131.289993, 132.905426, 137.326996, 138.905502, 140.115005, 140.907654, 144.240005, 145.000000, 150.360001, 151.964996, 157.250000, 158.925339, 162.500000, 164.930313, 167.259995, 168.934204, 173.039993, 174.966995, 178.490005, 180.947906, 183.850006, 186.207001, 190.199997, 192.220001, 195.080002, 196.966537, 200.589996, 204.383301, 207.199997, 208.980377, 209.000000, 210.000000, 222.000000, 223.000000, 226.024994, 227.028000, 232.038101, 231.035904, 238.028900, 237.048004, 244.000000, 243.000000, 247.000000, 247.000000, 251.000000, 252.000000, 257.000000, 258.000000, 259.000000, 262.000000, 261.000000, 262.000000, 263.000000, 262.000000, 265.000000, 266.000000]

  inertia = REPLICATE({AmesPAHdb_Geometry_Inertia_S, uid:0, inertia:DBLARR(3,3)}, self.nuids)

  inertia.uid = *self.uids

  FOR idx = 0, self.nuids - 1 DO BEGIN

     select = WHERE((*self.data).uid EQ (*self.uids)[idx], nselect)

     IF nselect EQ 0 THEN RETURN,-1

     I11 = TOTAL(atomic_masses[(*self.data)[select].type] * ((*self.data)[select].y^2 + ((*self.data)[select].z^2)))

     I22 = TOTAL(atomic_masses[(*self.data)[select].type] * ((*self.data)[select].x^2 + ((*self.data)[select].z^2)))

     I33 = TOTAL(atomic_masses[(*self.data)[select].type] * ((*self.data)[select].x^2 + ((*self.data)[select].y^2)))

     I12 = -TOTAL(atomic_masses[(*self.data)[select].type] * (*self.data)[select].x * (*self.data)[select].y)

     I13 = -TOTAL(atomic_masses[(*self.data)[select].type] * (*self.data)[select].x * (*self.data)[select].z)

     I23 = -TOTAL(atomic_masses[(*self.data)[select].type] * (*self.data)[select].y * (*self.data)[select].z)

     inertia[idx].inertia = [[I11, I12, I13], [I12, I22, I23], [I13, I23, I33]]
  ENDFOR

  RETURN,inertia
END

;+
; Calculates the number of rings per type for each PAH.
;
; :Returns:
;   struct array
;
; :Categories:
;   CALCULATE
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Rings

  COMPILE_OPT IDL2

  ON_ERROR,2

  numring = REPLICATE({AmesPAHdb_Geometry_Rings_S, uid:0, three:0, four:0, five:0, six:0, seven:0, eight:0}, self.nuids)

  FOR idx = 0, self.nuids - 1 DO BEGIN

     select = WHERE((*self.data).uid EQ (*self.uids)[idx], size)

     x = (*self.data)[select].x

     y = (*self.data)[select].y

     z = (*self.data)[select].z

     numn = INTARR(size)

     nlist = INTARR(size, 6)

     FOR i = 0, size - 1 DO BEGIN

        dd = SQRT((x - x[i])^2 + (y - y[i])^2 + (z - z[i])^2)

        sel = WHERE(dd GT 0D AND dd LT 1.7D, nsel)

        numn[i] = nsel

        nlist[i, 0:nsel-1] = sel
     ENDFOR

     iring = INTARR(9)

     FOR i = 0, size - 1 DO BEGIN

        iring[0] = i

        FOR j = 0, numn[i] - 1 DO BEGIN

           i2 = nlist[i, j]

           IF i2 LT i THEN CONTINUE

           iring[1] = i2

           FOR k = 0, numn[i2] - 1 DO BEGIN

              i3 = nlist[i2, k]

              IF i3 LE i THEN CONTINUE

              iring[2] = i3

              IF i3 NE i THEN BEGIN

                 FOR l = 0, numn[i3] - 1 DO BEGIN

                    i4 = nlist[i3, l]

                    IF i4 LE i THEN CONTINUE

                    iring[3] = i4

                    IF i4 EQ i2 THEN CONTINUE

                    IF i4 NE i OR iring[1] LT iring[2] THEN BEGIN

                       FOR m = 0, numn[i4] - 1 DO BEGIN

                          i5 = nlist[i4, m]

                          IF i5 LT i THEN CONTINUE

                          iring[4] = i5

                          IF i5 EQ i2 OR i5 EQ i3 THEN CONTINUE

                          IF i5 NE i OR iring[1] LT iring[3] THEN BEGIN

                             FOR n = 0, numn[i5] - 1 DO BEGIN

                                i6 = nlist[i5, n]

                                IF i6 LT i THEN CONTINUE

                                iring[5] = i6

                                IF i6 EQ i2 OR i6 EQ i3 OR i6 EQ i4 THEN CONTINUE

                                IF i6 NE i OR iring[1] LT iring[4] THEN BEGIN

                                   FOR o = 0, numn[i6] - 1 DO BEGIN

                                      i7 = nlist[i6, o]

                                      IF i7 LT i THEN CONTINUE

                                      iring[6] = i7

                                      IF i7 EQ i2 OR i7 EQ i3 OR i7 EQ i4 OR i7 EQ i5 THEN CONTINUE

                                      IF i7 NE i OR iring[1] LT iring[5] THEN BEGIN

                                         FOR p = 0, numn[i7] - 1 DO BEGIN

                                            i8 = nlist[i7, p]

                                            IF i8 LT i THEN CONTINUE

                                            iring[7] = i8

                                            IF i8 EQ i2 OR i8 EQ i3 OR i8 EQ i4 OR i8 EQ i5 OR i8 EQ i6 THEN CONTINUE

                                            IF i8 NE i OR iring[1] LT iring[6] THEN BEGIN

                                               FOR q = 0, numn[i8] - 1 DO BEGIN

                                                  i9 = nlist[i8, q]

                                                  IF i9 LT i THEN CONTINUE

                                                  iring[8] = i9

                                                  IF i9 EQ i2 OR i9 EQ i3 OR i9 EQ i4 OR i9 EQ i5 OR i9 EQ i6 OR i9 EQ i7 THEN CONTINUE

                                                  IF i9 NE i OR iring[1] LT iring[7] THEN CONTINUE $
                                                  ELSE numring[idx].eight += 1 ; 8-ring
                                               ENDFOR
                                            ENDIF ELSE numring[idx].seven += 1 ; 7-ring
                                         ENDFOR
                                      ENDIF ELSE numring[idx].six += 1 ; 6-ring
                                   ENDFOR
                                ENDIF ELSE numring[idx].five += 1 ; 5-ring
                             ENDFOR
                          ENDIF ELSE numring[idx].four += 1 ; 4-ring
                       ENDFOR
                    ENDIF ELSE numring[idx].three += 1 ; 3-ring
                 ENDFOR
              ENDIF
           ENDFOR
        ENDFOR
     ENDFOR
  ENDFOR

  numring.uid = *self.uids

  RETURN,numring
END

;+
; Calculates the total area of each PAH.
;
; :Returns:
;   struct array
;
; :Categories:
;   CALCULATE
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Area

  COMPILE_OPT IDL2

  ON_ERROR,2

  rings = self->Rings()

  area = REPLICATE({AmesPAHdb_Geometry_Area_S, uid:0, area:0D}, self.nuids)

  area.uid = *self.uids

  area.area = TOTAL([[rings.three], [rings.four], [rings.five], [rings.six], [rings.seven], [rings.eight]] * TRANSPOSE(REBIN([0.848, 1.96D, 3.37D, 5.09D, 7.12D, 9.46D], 6, self.nuids)), 2)

  RETURN,area
END

;+
; Diagonalizes the moment of inertia of each PAH and aligns its
; structure it with the x-y plane.
;
; :Keywords:
;   Full: in, optional, type=int
;     Whether to include off-diagnonal terms
;   Equal: in, optional, type=int
;     Whether to consider all atoms having equal masses
;
; :Categories:
;   DISPLAY
;-
PRO AmesPAHdbIDLSuite_Geometry::Diagonalize,Full=Full,Equal=Equal

    COMPILE_OPT IDL2

    ON_ERROR,2

    atomic_masses = [0.0D, 1.007940, 4.002602, 6.941000, 9.012182, 10.811000, 12.011000, 14.006740, 15.999400, 18.998404, 20.179701, 22.989767, 24.305000, 26.981539, 28.085501, 30.973763, 32.066002, 35.452702, 39.948002, 39.098301, 40.077999, 44.955910, 47.880001, 50.941502, 51.996101, 54.938049, 55.847000, 58.933201, 58.693401, 63.546001, 65.389999, 69.723000, 72.610001, 74.921593, 78.959999, 79.903999, 83.800003, 85.467796, 87.620003, 88.905853, 91.223999, 92.906380, 95.940002, 98.000000, 101.070000, 102.905502, 106.419998, 107.868202, 112.411003, 114.820000, 118.709999, 121.757004, 127.599998, 126.904472, 131.289993, 132.905426, 137.326996, 138.905502, 140.115005, 140.907654, 144.240005, 145.000000, 150.360001, 151.964996, 157.250000, 158.925339, 162.500000, 164.930313, 167.259995, 168.934204, 173.039993, 174.966995, 178.490005, 180.947906, 183.850006, 186.207001, 190.199997, 192.220001, 195.080002, 196.966537, 200.589996, 204.383301, 207.199997, 208.980377, 209.000000, 210.000000, 222.000000, 223.000000, 226.024994, 227.028000, 232.038101, 231.035904, 238.028900, 237.048004, 244.000000, 243.000000, 247.000000, 247.000000, 251.000000, 252.000000, 257.000000, 258.000000, 259.000000, 262.000000, 261.000000, 262.000000, 263.000000, 262.000000, 265.000000, 266.000000]

    IF KEYWORD_SET(Equal) THEN atomic_masses = 1D $
    ELSE atomic_masses[[12, 26]] = 0D

    FOR j = 0, self.nuids - 1 DO BEGIN

       select = WHERE((*self.data).uid EQ (*self.uids)[j], nselect)

       IF nselect LE 0 THEN CONTINUE

       coordinates = [[(*self.data)[select].x], $
                      [(*self.data)[select].y], $
                      [(*self.data)[select].z]]

       m = REBIN(atomic_masses[(*self.data)[select].type], nselect, 3)

       ; Translate molecule to center-of-mass
       coordinates -= TRANSPOSE(REBIN(TOTAL(coordinates * m, 1) / TOTAL(m, 1), 3, nselect))

       ; Create the moment of inertia tensor
       ;I = DIAG_MATRIX([TOTAL(m[*,0] * (coordinates[*,1]^2 + coordinates[*,2]^2)), $
       ;                 TOTAL(m[*,1] * (coordinates[*,0]^2 + coordinates[*,2]^2)), $
       ;                 TOTAL(m[*,2] * (coordinates[*,0]^2 + coordinates[*,1]^2))])

       I = DIAG_MATRIX([TOTAL(m[*,0] * coordinates[*,0]^2), $
                        TOTAL(m[*,1] * coordinates[*,1]^2), $
                        TOTAL(m[*,2] * coordinates[*,2]^2)])

       IF KEYWORD_SET(Full) THEN BEGIN

          I += DIAG_MATRIX(-[TOTAL(m[*,0] * coordinates[*,0] * coordinates[*,1]), $
                             TOTAL(m[*,2] * coordinates[*,1] * coordinates[*,2])], 1)

          I += DIAG_MATRIX(DIAG_MATRIX(I, 1), -1)

          I[0,2] = -TOTAL(m[*,1] * coordinates[*,0] * coordinates[*,2])

          I[2,0] = I[0,2]
       ENDIF

       ; Diagonalize the matrix
       LA_TRIRED,I,D,E

       LA_TRIQL,D,E,I

       ; Project the molecule
       FOR l = 0, nselect - 1 DO coordinates[l, *] = coordinates[l, *] # I

       ; Sort the Eigenvectors from smallest to largest Eigenvalue
       srt = REVERSE(SORT(D))

       coordinates = coordinates[*, srt]

       (*self.data)[select].x = coordinates[*, 0]
       (*self.data)[select].y = coordinates[*, 1]
       (*self.data)[select].z = coordinates[*, 2]
    ENDFOR
END

;+
; Retrieves the AmesPAHdbIDLSuite_Geometry representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  RETURN,{type:OBJ_CLASS(self)+'_S', $
          data:*self.data, $
          uids:*self.uids}
END

;+
; Populates the AmesPAHdbIDLSuite_Geometry-instance.
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Data: in, optional, type=struct
;     Data structure
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Geometry::Set,Struct,Data=Data,Uids=Uids

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Data) THEN BEGIN

              IF PTR_VALID(self.data) THEN PTR_FREE,self.data

              self.data = PTR_NEW(Struct.data)
           ENDIF

           IF NOT KEYWORD_SET(Uids) THEN BEGIN

              IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

              self.uids = PTR_NEW(Struct.uids)

              self.nuids = N_ELEMENTS(*self.uids)
           ENDIF
        ENDIF
     ENDIF
  ENDIF

  IF KEYWORD_SET(Data) THEN BEGIN

     IF PTR_VALID(self.data) THEN PTR_FREE,self.data

     self.data = PTR_NEW(Data)
  ENDIF

  IF KEYWORD_SET(Uids) THEN BEGIN

     IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

     self.uids = PTR_NEW(Uids)

     self.nuids = N_ELEMENTS(*self.uids)
  ENDIF

  self.current = 0

  self.state = 1
END

;+
; Clean-up an AmesPAHdbIDLSuite_Geometry-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Geometry::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids
END

;+
; Create an AmesPAHdbIDLSuite_Geometry-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Geometry-instance
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Data: in, optional, type=struct
;     Data structure
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Geometry::Init,Struct,Data=Data,Uids=Uids

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Data=Data,Uids=Uids $
  ELSE self->Set,Data=Data,Uids=Uids

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Geometry Class
;
; :Fields:
;   current: type=long
;    Last UID used
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Geometry__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Geometry, $
          INHERITS AmesPAHdbIDLSuite_Data, $
          ;state:0, $
          ;data:PTR_NEW(), $
          ;uids:PTR_NEW(), $
          ;nuids:0L, $
          current:0L}
END

; END OF amespahgeometry__define.pro
