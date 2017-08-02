;+
; CLASS_NAME:
;    AmesPAHdbIDLSuite_Data
;
; CONTACT:
;
;    Updated versions of the NASA Ames PAH IR Spectroscopic
;    Database and more information can be found at:
;    http://www.astrochem.org/pahdb
;
; DESCRIPTION:
;    Main class to hold and select spectroscopic data
;
; CATEGORY:
;    DATA
;
; SUPERCLASSES:
;   NONE 
;
; SUBCLASSES:
;    NONE
;
; REQUIRED CLASSES:
;    NONE
;
; OPTIONAL:
;    NONE
;
; CREATION:
;    data = OBJ_NEW('AmesPAHdbIDLSuite_Data')
;
; METHODS
;    PUBLIC:
;    DESCRIPTION (PROCEDURE)
;    NORMALIZE (PROCEDURE)
;    GET (FUNCTION)
;    SET (PROCEDURE)
;    GETUIDS (FUNCTION)
;    INTERSECT (PROCEDURE)
;    DIFFERENCE (PROCEDURE)
;
; PRIVATE:
;
; OVERLOADED OPERATORS
;    _OVERLOADFOREACH
;    _OVERLOADPLUS
;    _OVERLOADBRACKETSRIGHTSIDE
;    _OVERLOADSIZE
;    _OVERLOADPRINT
;    _OVERLOADIMPLIEDPRINT
;    _OVERLOADHELP
;
; EXAMPLE
;    data = OBJ_NEW('AmesPAHdbIDLSuite_Data')
;    data->Set,data
;    OBJ_DESTROY,data
;
; MODIFICATION HISTORY
;
;   05-20-2015
;   Added COMPLEMENT and ensured uids is an array in INTERSECT.
;   Christiaan Boersma
;   04-07-2015
;   Added _OVERLOADFOREACH, _OVERLOADPLUS, _OVERLOADBRACKETSRIGHTSIDE,
;   _OVERLOADSIZE, _OVERLOADPRINT, _OVERLOADIMPLIEDPRINT,
;   _OVERLOADHELP. Christiaan Boersma
;   02-01-2015
;   First version of the file. Christiaan Boersma.
;-

; Use special plot class that initializes colors, plotsymes, etc.
; Use prototype call

;; OUTPUT

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

;; NORMALIZE

PRO AmesPAHdbIDLSuite_Data::Normalize,FromAll=FromAll,Max=Max

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF KEYWORD_SET(FromAll) THEN BEGIN

     Max = MAX((*self.data).intensity) 

     (*self.data).intensity /= Max
  ENDIF ELSE BEGIN

     Max = DBLARR(self.nuids, /NOZERO)

     FOR i = 0, self.nuids - 1 DO BEGIN

        select = WHERE((*self.data).uid EQ (*self.uids)[i], nselect)

        Max[i] = MAX((*self.data)[select].intensity)

        (*self.data)[select].intensity /= Max[i]
     ENDFOR
  ENDELSE
END

;; SET OPERATIONS

PRO AmesPAHdbIDLSuite_Data::Intersect,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  nuids = N_ELEMENTS(UIDs)

  ndata = N_ELEMENTS(*self.data)

  idx = LINDGEN(ndata, nuids)

  select = WHERE((*self.data)[idx MOD ndata].uid EQ UIDs[idx / ndata], nselect) MOD ndata

  IF nselect EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                 NO INTERSECTION FOUND                   "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF 

  *self.data = (*self.data)[select]

  *self.uids = [(*self.data)[UNIQ((*self.data).uid, SORT((*self.data).uid))].uid]

  self.nuids = N_ELEMENTS(*self.uids)

  Count = self.nuids
  
  PRINT
  PRINT,"========================================================="
  PRINT,"           INTERSECTION FOUND: "+STRING(FORMAT='(I-0)', Count)
  PRINT,"========================================================="
  PRINT
END

PRO AmesPAHdbIDLSuite_Data::Difference,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  mina = MIN(*self.uids, MAX=maxa)

  minb = MIN(UIDs, MAX=maxb)

  IF (minb GT maxa) OR (maxb LT mina) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                 NO INTERSECTION FOUND                   "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN
  ENDIF 
     
  r = WHERE((HISTOGRAM(*self.uids, MIN=mina, MAX=maxa) NE 0) AND (HISTOGRAM(UIDs, MIN=mina, MAX=maxa) EQ 0), Count)

  IF Count EQ 0 THEN BEGIN
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
  PRINT,"          DIFFERENCE FOUND: "+STRING(FORMAT='(I-0)', Count)
  PRINT,"========================================================="
  PRINT
  
  found = r + mina

  nfound = N_ELEMENTS(found)

  ndata = N_ELEMENTS(*self.data)

  idx = LINDGEN(ndata, nfound)

  select = WHERE((*self.data)[idx MOD ndata].uid EQ found[idx / ndata], nselect) MOD ndata  

  *self.data = (*self.data)[select]

  *self.uids = [(*self.data)[UNIQ((*self.data).uid, SORT((*self.data).uid))].uid]

  self.nuids = N_ELEMENTS(*self.uids)
END

;; GET/SET

FUNCTION AmesPAHdbIDLSuite_Data::GetUIDS,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = self.nuids

  RETURN,*self.uids
END

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

;; OPERATOR OVERLOADING

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

FUNCTION AmesPAHdbIDLSuite_Data::_overloadForeach,value,index

  COMPILE_OPT IDL2

  ON_ERROR,2

  index = N_ELEMENTS(index) EQ 0L ? 0L : (index + 1L)

  status = index LT self.nuids

  IF status THEN value = self->_overloadBracketsRightSide(isRange, index)
  
  RETURN,status
END

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

  idx = LINDGEN(ndata, nuids)

  select = WHERE(right_s.data[idx MOD ndata].uid EQ uids[idx / ndata]) MOD ndata
  
  RETURN,OBJ_NEW(OBJ_CLASS(left), $
                 right_s, $
                 Data=[right_s.data, left_s.data[select]], $
                 UIDs=[right_s.uids, uids])
END

FUNCTION AmesPAHdbIDLSuite_Data::_overloadSize

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,SIZE(*self.uids, /DIMENSIONS)
END

FUNCTION AmesPAHdbIDLSuite_Data::_overloadPrint

  COMPILE_OPT IDL2

  ON_ERROR,2

  self->Description,Str
  
  RETURN, STRJOIN(STRSPLIT(Str, "!C", /EXTRACT, /REGEX), STRING(10B))
END

FUNCTION AmesPAHdbIDLSuite_Data::_overloadImpliedPrint,arg

  COMPILE_OPT IDL2

  ON_ERROR,2
  
  RETURN, self->AmesPAHdbIDLSuite_Data::_overloadPrint()
END

FUNCTION AmesPAHdbIDLSuite_Data::_overloadHelp,arg

  COMPILE_OPT IDL2

  ON_ERROR,2

  Str = '<' + arg + '>' + STRING(10B) + $
        STRING(FORMAT='(A-12,":",X,I-0)', "nuids", self.nuids) + STRING(10B) + $
        self->AmesPAHdbIDLSuite_Data::_overloadPrint()
  
  RETURN,Str
END

;; OBJECT CLASS DEFINITION, INITIALIZATION AND CLEAN UP

PRO AmesPAHdbIDLSuite_Data::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

  IF PTR_VALID(self.model) THEN PTR_FREE,self.model
END

FUNCTION AmesPAHdbIDLSuite_Data::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units $
  ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,Model=Model,Units=Units

  RETURN,self.state
END

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
