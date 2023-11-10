; docformat = 'rst'

;+
;
; Main class to hold and select spectroscopic data.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;
;   Creating, setting and destroying and
; AmesPAHdbIDLSuite_Data-instance::
;
;    IDL> data = OBJ_NEW('AmesPAHdbIDLSuite_Data')
;    IDL> data->Set,data
;    IDL> OBJ_DESTROY,data
;
; :Properties:
;   Data: in, optional, type="Data-struct"
;     Variable data structure
;   Model: in, optional, type="Model-struct"
;     Model-tracking structure
;   PAHdb: in, optional, type=pointer
;     Pointer to a parsed database
;   Type: in, optional, type=string
;     Type of Data
;   UIDs: in, optional, type="long array (1D)"
;     UIDs in Data
;   Units: in, optional, type="Unit-struct"
;     Unit-tracking structure
;   Version: in, optional, type=string
;     Version of Data
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
;     11-09-2023
;     Add missing case for switch in HASHCODE method. Christiaan Boersma.
;     05-16-2022
;     Speed up INTERSECT and DIFFERENCE using HISTOGRAM.
;     Christiaan Boersma.
;     05-12-2022
;     Use HISTOGRAM in NORMALIZE. Add HASHCODE method. Christiaan
;     Boersma.
;     05-20-2015
;     Added COMPLEMENT and ensured uids is an array in INTERSECT.
;     Christiaan Boersma
;     04-07-2015
;     Added _OVERLOADFOREACH, _OVERLOADPLUS, _OVERLOADBRACKETSRIGHTSIDE,
;     _OVERLOADSIZE, _OVERLOADPRINT, _OVERLOADIMPLIEDPRINT,
;     _OVERLOADHELP. Christiaan Boersma
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;
; :Private:
;-

;+
; Provides Description of Data
;
; :Params:
;   Str: out, optional, type=string
;     Description of Data
;
; :Categories:
;   INFORMATIVE
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Data::Description,Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  Str = STRING(FORMAT='(A-12,":",X,A-0)', "database", self.type)

  Str = [Str, STRING(FORMAT='(A-12,":",X,A-0)', "version", self.version)]

  Str = [Str, (*self.model).description]

  Str = STRJOIN(Str, "!C")

  IF N_PARAMS() GT 0 THEN RETURN

  PRINT,STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

;+
; Normalizes the intensity Data
;
; :Keywords:
;   FromAll: in, optional, type=int
;     Whether to consider All intensities
;   Max: out, optional, type=double
;     Maximum intensity
;
; :Categories:
;   DATA
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Data::Normalize,FromAll=FromAll,Max=Max

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF KEYWORD_SET(FromAll) THEN BEGIN

     Max = MAX((*self.data).intensity)

     (*self.data).intensity /= Max
  ENDIF ELSE BEGIN

     Max = DBLARR(self.nuids, /NOZERO)

     h = HISTOGRAM((*self.data).uid, MIN=0, REVERSE_INDICES=ri)

     FOR i = 0, self.nuids - 1 DO BEGIN

        select = ri[ri[(*self.uids)[i]]:ri[(*self.uids)[i]+1]-1]

        Max[i] = MAX((*self.data)[select].intensity)

        (*self.data)[select].intensity /= Max[i]
     ENDFOR
  ENDELSE
END

;+
; Updates Data to the Intersection with UIDs
;
; :Params:
;   UIDs: in, required, type="long or long array"
;     UIDs to consider for Difference
;   Count: out, optional, type=long
;     Number of UIDs
;
; :Categories:
;   SET OPERATIONS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Data::Intersect,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = 0L

  mina = MIN(*self.uids, MAX=maxa) 

  minb = MIN(UIDs, MAX=maxb)

  minab = mina > minb

  maxab = maxa < maxb

  IF ((maxa LT minab) AND (minb GT maxab)) OR $
     ((maxb LT minab) AND (mina GT maxab)) THEN BEGIN

    PRINT
    PRINT,"========================================================="
    PRINT,"                  NO INTERSECTION FOUND                  "
    PRINT,"========================================================="
    PRINT
    self.state = 0
    RETURN
  ENDIF

  r = Where((HISTOGRAM(*self.uids, MIN=minab, MAX=maxab) NE 0) AND  $
            (HISTOGRAM(UIDs, MIN=minab, MAX=maxab) NE 0), n)

  IF n EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                 NO INTERSECTION FOUND                   "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"           INTERSECTION FOUND: "+STRING(FORMAT='(I-0)', n)
  PRINT,"========================================================="
  PRINT

  *self.uids = r + minab

  self.nuids = n

  nbuf = 4096L

  select = LONARR(nbuf, /NOZERO)

  h = HISTOGRAM((*self.data).uid, MIN=0, REVERSE_INDICES=ri)

  FOR i = 0L, self.nuids - 1L DO BEGIN

    n = h[(*self.uids)[i]]

    IF n GT 0 THEN BEGIN

      s = ri[ri[(*self.uids)[i]]:ri[(*self.uids)[i]+1]-1]

      IF Count + n GT nbuf THEN BEGIN

        nbuf *= 2L

        new = LONARR(nbuf, /NOZERO)

        new[0:Count-1L] = select[0:Count-1L]

        select = new
      ENDIF

      select[Count:Count+n-1L] = s

      Count += n
    ENDIF
  ENDFOR

  *self.data = (*self.data)[select[0:Count-1L]]
END

;+
; Updates Data to the difference with UIDs
;
; :Params:
;   UIDs: in, required, type="long or long array"
;     UIDs to consider for Difference
;   Count: out, optional, type=long
;     Number of UIDs
;
; :Categories:
;   SET OPERATIONS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Data::Difference,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = 0L

  mina = MIN(*self.uids, MAX=maxa)

  minb = MIN(UIDs, MAX=maxb)

  IF (minb GT maxa) OR $
     (maxb LT mina) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                  NO DIFFERENCE FOUND                    "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  r = WHERE((HISTOGRAM(*self.uids, MIN=mina, MAX=maxa) NE 0) AND $
            (HISTOGRAM(UIDs, MIN=mina, MAX=maxa) EQ 0), n)

  IF n EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                  NO DIFFERENCE FOUND                    "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF

  PRINT
  PRINT,"========================================================="
  PRINT,"          DIFFERENCE FOUND: "+STRING(FORMAT='(I-0)', n)
  PRINT,"========================================================="
  PRINT

  *self.uids = r + mina

  self.nuids = n

  nbuf = 4096L

  select = LONARR(nbuf, /NOZERO)

  h = HISTOGRAM((*self.data).uid, MIN=0, REVERSE_INDICES=ri)

  FOR i = 0L, self.nuids - 1L DO BEGIN

    n = h[(*self.uids)[i]]

    IF n GT 0 THEN BEGIN

      s = ri[ri[(*self.uids)[i]]:ri[(*self.uids)[i]+1]-1]

      IF Count + n GT nbuf THEN BEGIN

        nbuf *= 2L

        new = LONARR(nbuf, /NOZERO)

        new[0:Count-1L] = select[0:Count-1L]

        select = new
      ENDIF

      select[Count:Count+n-1L] = s

      Count += n
    ENDIF
  ENDFOR

  *self.data = (*self.data)[select[0:Count-1L]]
END

;+
; Retrieves the Data UIDs.
;
; :Returns:
;   long
;
; :Params:
;   Count: out, optional, type=long
;     Number of UIDs
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::GetUIDS,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = self.nuids

  RETURN,*self.uids
END

;+
; Retrieves the Class representation in a structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  RETURN,{type:OBJ_CLASS(self)+'_S', $
          database:self.type, $
          version:self.version, $
          data:*self.data, $
          uids:*self.uids, $
          nuids:self.nuids, $
          model:*self.model, $
          units:self.units}
END

;+
; Populates the instance.
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   Version: in, optional, type=string
;    Versioning information
;   Data: in, optional, type=struct
;     Data structure
;   PAHdb: in, optional, type=pointer
;     Pointer to parsed database file
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;   Model: in, optional, type=struct
;     Model-describing structure
;   Units: in, optional, type=struct
;     Unit-describing structure
;
; :Categories:
;   SET/GET
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Data::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  COMPILE_OPT IDL2, HIDDEN

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN BEGIN

     tag = WHERE(TAG_NAMES(Struct) EQ 'TYPE', ntype)

     IF ntype EQ 1 THEN BEGIN

        IF Struct.(tag) EQ OBJ_CLASS(self)+'_S' THEN BEGIN

           IF NOT KEYWORD_SET(Type) THEN self.type = Struct.database

           IF NOT KEYWORD_SET(Version) THEN self.version = Struct.version

           IF NOT KEYWORD_SET(Data) THEN BEGIN

              IF PTR_VALID(self.data) THEN PTR_FREE,self.data

              self.data = PTR_NEW(Struct.data)
           ENDIF

           IF NOT KEYWORD_SET(Uids) THEN BEGIN

              IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

              self.uids = PTR_NEW(Struct.uids)

              self.nuids = N_ELEMENTS(*self.uids)
           ENDIF

           IF NOT KEYWORD_SET(Model) THEN BEGIN

              IF PTR_VALID(self.model) THEN PTR_FREE,self.model

              self.model = PTR_NEW(Struct.model)
           ENDIF

           IF NOT KEYWORD_SET(Units) THEN self.units = Struct.units
        ENDIF
     ENDIF
  ENDIF

  IF KEYWORD_SET(Type) THEN self.type = Type

  IF KEYWORD_SET(Version) THEN self.version = Version

  IF KEYWORD_SET(Data) THEN BEGIN

     IF PTR_VALID(self.data) THEN PTR_FREE,self.data

     self.data = PTR_NEW(Data)
  ENDIF

  IF KEYWORD_SET(PAHdb) THEN self.database = PAHdb

  IF KEYWORD_SET(Uids) THEN BEGIN

     IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

     self.uids = PTR_NEW(Uids)

     self.nuids = N_ELEMENTS(*self.uids)
  ENDIF

  IF KEYWORD_SET(Model) THEN BEGIN

     IF PTR_VALID(self.model) THEN PTR_FREE,self.model

     self.model = PTR_NEW(Model)
  ENDIF

  IF KEYWORD_SET(Units) THEN self.units = Units

  IF PTR_VALID(self.database) THEN BEGIN

     IF STRCMP((*self.database).type, self.type) EQ 0 THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"             DATABASE MISMATCH: "+(*self.database).type+" != "+self.type
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF

     IF STRCMP((*self.database).version, self.version) EQ 0 THEN BEGIN
        PRINT
        PRINT,"========================================================="
        PRINT,"              VERSION MISMATCH: "+(*self.database).version+" != "+self.version
        PRINT,"========================================================="
        PRINT
        self.state = 0
        RETURN
     ENDIF
  ENDIF

  self.state = 1
END

;+
; Compute hash code for object or struct.
;
; :Returns:
;   long
;
; :Params:
;   In: in, required, type=object/struct
;     Object/struct to compute hash code for.
;
; :Categories:
;   HELPER
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::HashCode,In

  COMPILE_OPT IDL2, STATIC

  ON_ERROR,2

  ntags = N_TAGS(In)

  Code = []

  FOR i = 0L, ntags - 1L DO BEGIN

    SWITCH SIZE(In.(i), /TYPE) OF
      0: BREAK
      1:
      2:
      3:
      4:
      5:
      6:
      7: BEGIN
        Code = [Code, In.(i).HASHCODE()]
        BREAK
      END
      8: BEGIN
        Code = [Code, self.HashCode(In.(i))]
        BREAK
      END
      9: BEGIN
        Code = [Code, In.(i).HASHCODE()]
        BREAK
      END
      10: BEGIN
        IF PTR_VALID(In.(i)) THEN BEGIN
          t = SIZE(*In.(i), /TYPE)
          IF t EQ 8 OR t EQ 10 OR t EQ 11 THEN Code = [Code, self.HashCode(*In.(i))] $
          ELSE Code = [Code, (*In.(i)).HASHCODE()]
        ENDIF
        BREAK
      END
      11: BEGIN
        IF OBJ_VALID(In.(i)) THEN Code = [Code, self.HashCode(In.(i))]
        BREAK
      END
      12:
      13:
      14:
      15: BEGIN
        Code = [Code, In.(i).HASHCODE()]
        BREAK
      END
    ENDSWITCH
  ENDFOR

  RETURN, Code.HASHCODE()
END

;+
; Overload the array subscript access operator.
;
; :Returns:
;   Object element
;
; :Params:
;   isRange: in, required, type=int
;     Whether index is an array
;   index: in, required, type="long or long array (1D)"
;     Index
;
; :Categories:
;   OVERLOAD
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::_overloadBracketsRightSide,isRange,index

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.data) THEN BEGIN

     select = WHERE((*self.data).uid EQ (*self.uids)[index], nselect)

     IF nselect EQ 0 THEN RETURN,-1

     RETURN,(*self.data)[select]
  ENDIF

  RETURN,-1
END

;+
; Overload FOREACH operator.
;
; :Returns:
;   1 on success or 0 on failure
;
; :Params:
;   value: out, required, type=object
;     Object element
;   index: out, required, type=long
;     Index associated with Object element
;
; :Categories:
;   OVERLOAD
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::_overloadForeach,value,index

  COMPILE_OPT IDL2

  ON_ERROR,2

  index = N_ELEMENTS(index) EQ 0L ? 0L : (index + 1L)

  status = index LT self.nuids

  IF status THEN value = self->_overloadBracketsRightSide(isRange, index)

  RETURN,status
END

;+
; Overload the PLUS operator.
;
; :Returns:
;   AmesPAHdbIDLSuite_Data- or derived-instance
;
; :Params:
;   left: in, required, type=object
;     left argument
;   right: in, required, type=object
;     right argument
;
; :Categories:
;   OVERLOAD
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::_overloadPlus,left,right

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF OBJ_CLASS(left) NE OBJ_CLASS(RIGHT) THEN MESSAGE,'TWO <' + OBJ_CLASS(left) + '> OBJECT INSTANCES REQUIRED'

  left_s = left->Get()

  right_s = right->Get()

  left_min = MIN(left_s.uids, MAX=left_max)

  right_min = MIN(right_s.uids, MAX=right_max)

  IF (right_min GT left_max) OR $
     (right_max LT left_min) THEN RETURN, OBJ_NEW(OBJ_CLASS(left), $
                                                  right_s, $
                                                  Data=[right_s.data, left_s.data], $
                                                  UIDs=[right_s.uids, left_s.uids])

  uids = WHERE((HISTOGRAM(left_s.uids, MIN=left_min, MAX=left_max) NE 0) AND $
               (HISTOGRAM(right_s.uids, MIN=left_min, MAX=left_max) EQ 0), nuids) $
         + left_min

  IF nuids EQ 0 THEN RETURN, OBJ_NEW(OBJ_CLASS(right), $
                                     right_s)

  ndata = N_ELEMENTS(right_s.data)

  idx = ULINDGEN(ndata, nuids)

  select = WHERE(right_s.data[idx MOD ndata].uid EQ uids[idx / ndata]) MOD ndata

  RETURN,OBJ_NEW(OBJ_CLASS(left), $
                 right_s, $
                 Data=[right_s.data, left_s.data[select]], $
                 UIDs=[right_s.uids, uids])
END

;+
; Overload the Size information.
;
; :Returns:
;   long or long array
;
; :Categories:
;   OVERLOAD
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::_overloadSize

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,SIZE(*self.uids, /DIMENSIONS)
END

;+
; Overload the PRINT information.
;
; :Returns:
;   string or string array
;
; :Categories:
;   OVERLOAD
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::_overloadPrint

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->Description,Str

  RETURN, STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

;+
; Overload the Implied PRINT information.
;
; :Returns:
;   string or string array
;
; :Params:
;   arg: in, required, type=string
;     Variable name
;
; :Categories:
;   OVERLOAD
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::_overloadImpliedPrint,arg

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN, self->AmesPAHdbIDLSuite_Data::_overloadPrint()
END

;+
; Overload the HELP information.
;
; :Returns:
;   string or string array
;
; :Params:
;   arg: in, required, type=string
;     Variable name
;
; :Categories:
;   OVERLOAD
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Data::_overloadHelp,arg

  COMPILE_OPT IDL2

  ON_ERROR,2

  Str = '<' + arg + '>' + STRING(10B) + $
        STRING(FORMAT='(A-12,":",X,I-0)', "nuids", self.nuids) + STRING(10B) + $
        self->AmesPAHdbIDLSuite_Data::_overloadPrint()

  RETURN,Str
END

;+
; Clean-up an AmesPAHdbIDLSuite_Data-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Data::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

  IF PTR_VALID(self.model) THEN PTR_FREE,self.model
END

;+
; Create an AmesPAHdbIDLSuite_Data-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Data-instance
;
; :Params:
;   Struct: in, optional, type=struct
;     Data structure
;
; :Keywords:
;   Type: in, optional, type=string
;     Type of Data
;   Version: in, optional, type=string
;    Versioning information
;   Data: in, optional, type=struct
;     Data structure
;   PAHdb: in, optional, type=pointer
;     Pointer to parsed database file
;   Uids: in, optional, type="long array (1D)"
;     UIDs in Data
;   Model: in, optional, type=struct
;     Model-describing structure
;   Units: in, optional, type=struct
;     Unit-describing structure
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Data::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units $
  ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  RETURN,self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Data Class
;
; :Fields:
;   state: type=long
;     Internal state
;   type: type=string
;     Type of Data
;   version: type=sting
;     Versioning information
;   data: type=pointer
;     Data pointer
;   database: type=pointer
;     Database pointer
;   uids: type=pointer
;     Pointer to the UIDs of Data
;   nuids: type=long
;     Number of UIDs
;   model: type=pointer
;     Pointer to the model describing structure
;   units: type="AmesPAHdb_Data_Units_S"
;     Units describing structure
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Data__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Data, $
          INHERITS IDL_Object, $
          state:0L, $
          type:'', $
          version:'', $
          data:PTR_NEW(), $
          database:PTR_NEW(), $
          uids:PTR_NEW(), $
          nuids:0L, $
          model:PTR_NEW(), $
          units:{AmesPAHdb_Data_Units_S, $
                 abscissa:{AmesPAHdb_Unit_S, $
                           unit:0, $
                           str:''}, $
                 ordinate:{AmesPAHdb_Unit_S, $
                           unit:0, $
                           str:''}}}
END

; END OF amespahdata__define.pro
