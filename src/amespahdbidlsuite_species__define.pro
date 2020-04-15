; docformat = 'rst'

;+
;
; Class to hold and access the data and properties in a parsed
; database.
;
; Updated versions of the NASA Ames PAH IR Spectroscopic Database and
; more information can be found at: `www.astrochemistry.org/pahdb <https://www.astrochemistry.org/pahdb>`.
;
; :Examples:
;   Creating, setting and printing Species info::
;
;     IDL> species = OBJ_NEW('AmesPAHdbIDLSuite_Species')
;     IDL> species->Set,data
;     IDL> species->Print
;     IDL> OBJ_DESTROY,info
;
; :Uses:
;    AmesPAHdbIDLSuite_Species,AmesPAHdbIDLSuite_Transitions,AmesPAHdbIDLSuite_Laboratory_Spectrum,AmesPAHdbIDLSuite_Geometry
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
;     05-08-2019
;     In the absence of Michael Galloy's STR_REPLACE function
;     FormatFormulae will print a message and return. Christiaan
;     Boersma.
;     10-17-2017
;     Fixed units definition in TRANSITIONS.
;     Christiaan Boersma.
;     02-01-2015
;     First version of the file. Christiaan Boersma.
;
;-

;+
; Make the formulae look pretty by embedding formatting commands.
;
; :Uses:
;   STR_REPLACE by Michael Galloy
;
; :Categories:
;   MARKUP
;-
PRO AmesPAHdbIDLSuite_Species::FormatFormulae

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF FILE_WHICH("str_replace.pro", /INCLUDE_CURRENT_DIR) EQ "" THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"          FORMATFORMULAE REQUIRES STR_REPLACE            "
     PRINT,"========================================================="
     PRINT
     RETURN
  ENDIF

  FOR i = 0, self.nuids - 1 DO BEGIN

     (*self.data)[i].formula = STR_REPLACE((*self.data)[i].formula, "([A-Z])([0-9]+)", "$1!L$2!N", /GLOBAL)

     (*self.data)[i].formula = STR_REPLACE((*self.data)[i].formula, "((\+)+|(\+[0-9])|(-)+|(-[0-9]))", "!U$1!N")
  ENDFOR
END

;+
;  Output species description.
;
;  :Params:
;    UID: in, optional, type=long
;      UID of the species
;
;   :Keywords:
;     Str: out, optional, type="string array"
;       Ouput to Str
;
; :Categories:
;   INFORMATIVE
;-
PRO AmesPAHdbIDLSuite_Species::Print,UID,Str=Str

  COMPILE_OPT IDL2

  ON_ERROR,2

  nselect1 = self.nuids

  IF N_PARAMS() EQ 0 THEN select1 = ULINDGEN(nselect1) $
  ELSE select1 = WHERE((*self.data).uid EQ UID, nselect1)

  IF nselect1 EQ 0 THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"                     UID NOT FOUND                       "
     PRINT,"========================================================="
     PRINT
     RETURN
  ENDIF

  FOR i = 0, nselect1 - 1 DO BEGIN

     tags = TAG_NAMES((*self.data)[select1[i]])

     ntags = N_TAGS((*self.data)[select1[i]])

     IF KEYWORD_SET(Str) THEN BEGIN

        FOR j = 0, ntags - 1 DO Str = [Str, STRING(FORMAT='(A-10,":",4X,A-0)',tags[j],STRTRIM((*self.data)[select1[i]].(j), 2))]

        IF SIZE(*self.comments, /TNAME) EQ 'STRUCT' THEN BEGIN

           select2 = WHERE((*self.comments).uid EQ (*self.data)[select1[i]].uid, nselect2)

           FOR j = 0, nselect2 - 1 DO Str = [Str, STRING(FORMAT='(A-10,":",4X,A-0)',"COMMENT",STRTRIM((*self.comments)[select2].str, 2))]
        ENDIF

        IF SIZE(*self.references, /TNAME) EQ 'STRUCT' THEN BEGIN

           select2 = WHERE((*self.references).uid EQ (*self.data)[select1[i]].uid, nselect2)

           FOR j = 0, nselect2 - 1 DO Str = [Str, STRING(FORMAT='(A-10,":",4X,A-0)',"REFERENCE",STRTRIM((*self.references)[select2].str, 2))]
        ENDIF
     ENDIF ELSE BEGIN

        PRINT
        PRINT,"========================================================="

        FOR j = 0, ntags - 1 DO PRINT,FORMAT='(A-10,":",4X,A-0)',tags[j],STRTRIM(STRING((*self.data)[select1[i]].(j)), 2)

        IF SIZE(*self.comments, /TNAME) EQ 'STRUCT' THEN BEGIN

           select2 = WHERE((*self.comments).uid EQ (*self.data)[select1[i]].uid, nselect2)

           FOR j = 0, nselect2 - 1 DO PRINT,FORMAT='(A-10,":",4X,A-0)',"COMMENT",STRTRIM((*self.comments)[select2].str, 2)
        ENDIF

        IF SIZE(*self.references, /TNAME) EQ 'STRUCT' THEN BEGIN

           select2 = WHERE((*self.references).uid EQ (*self.data)[select1[i]].uid, nselect2)

           FOR j = 0, nselect2 - 1 DO PRINT,FORMAT='(A-10,":",4X,A-0)',"REFERENCE",STRTRIM((*self.references)[select2].str, 2)
        ENDIF

        PRINT,"========================================================="
        PRINT
     ENDELSE
  ENDFOR

  IF KEYWORD_SET(Str) THEN Str = Str[1:*]
END

;+
; Retrieves species comments.
;
; :Returns:
;   AmesPAHdb_Comment_S array
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Species::Comments

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.comments
END

;+
; Retrieves species references.
;
; :Returns:
;   AmesPAHdb_Reference_S array
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Species::References

  COMPILE_OPT IDL2

  ON_ERROR,2

  RETURN,*self.references
END

;+
; Retrieves species transitions.
;
; :Returns:
;   AmesPAHdbIDLSuite_Transitions-instance
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Species::Transitions

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.database) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"              VALID DATABASE POINTER NEEDED              "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN, OBJ_NEW()
  ENDIF

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Transitions', $
                 Type=(*self.database).type ,$
                 Version=(*self.database).version, $
                 Data=self->GetTagByUID('transitions', *self.uids, Count), $
                 PAHdb=self.database, $
                 Uids=*self.uids, $
                 Model={type:'AMESPAHDBIDLSUITE_MODEL_ZEROKELVIN_S',$
                        Temperature:0D, $
                        description:STRING(FORMAT='(A-11,":",X,A-0)', "model", "ZeroKelvin")}, $
                 Units={AmesPAHdb_Data_Units_S, $
                 abscissa:{AmesPAHdb_Unit_S, $
                    unit:1, $
                    str:'frequency [cm!U-1!N]'}, $
                 ordinate:{AmesPAHdb_Unit_S, $
                    unit:2, $
                    str:'integrated cross-section [km/mol]'}})
END

;+
; Retrieves species geometry.
;
; :Returns:
;   AmesPAHdbIDLSuite_Geometry-instance
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Species::Geometry

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.database) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"              VALID DATABASE POINTER NEEDED              "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN, OBJ_NEW()
  ENDIF

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Geometry', $
                 Data=self->GetTagByUID('geometries', *self.uids, Count), $
                 Uids=*self.uids)
END


;+
; Retrieves species laboratory spectra.
;
; :Returns:
;   AmesPAHdbIDLSuite_Laboratory_Spectrum-instance
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Species::Laboratory

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.database) THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"              VALID DATABASE POINTER NEEDED              "
     PRINT,"========================================================="
     PRINT
     self.state = 0
     RETURN, OBJ_NEW()
  ENDIF

  IF (*self.database).type NE 'experimental' THEN BEGIN
     PRINT
     PRINT,"========================================================="
     PRINT,"            EXPERIMENTAL DATABASE REQUIRED               "
     PRINT,"========================================================="
     PRINT
     RETURN, OBJ_NEW()
  ENDIF

  RETURN,OBJ_NEW('AmesPAHdbIDLSuite_Laboratory_Spectrum', $
                 Type=(*self.database).type ,$
                 Version=(*self.database).version, $
                 Data=self->GetTagByUID('laboratory', *self.uids, Count), $
                 PAHdb=self.database, $
                 Model={type:'AMESPAHDBIDLSUITE_RAW_LABORATORY_S',$
                        Temperature:0D, $
                        description:STRING(FORMAT='(A-11,":",X,A-0)', "model", "Laboratory")}, $
                 Uids=*self.uids, $
                 Units={AmesPAHdb_Data_Units_S, $
                        x:{AmesPAHdb_Unit_S, $
                           unit:1, $
                           str:'frequency [cm!U-1!N]'}, $
                        y:{AmesPAHdb_Unit_S, $
                           unit:1, $
                           str:'absorbance [-log(I/I!L0!N)]'}})
END

;+
; Updates Species to the Intersection with UIDs
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
PRO AmesPAHdbIDLSuite_Species::Intersect,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  nuids = N_ELEMENTS(UIDS)

  ndata = N_ELEMENTS(*self.data)

  idx = ULINDGEN(ndata, nuids)

  select = WHERE((*self.data)[idx MOD ndata].uid EQ UIDS[idx / ndata], nselect) MOD ndata

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

  *self.uids = (*self.data)[UNIQ((*self.data).uid, SORT((*self.data).uid))].uid

  self.nuids = N_ELEMENTS(*self.uids)

  Count = self.nuids

  PRINT
  PRINT,"========================================================="
  PRINT,"           INTERSECTION FOUND: "+STRING(FORMAT='(I-0)', Count)
  PRINT,"========================================================="
  PRINT

  ndata = N_ELEMENTS(*self.references)

  select = WHERE((*self.references)[idx MOD ndata].uid EQ UIDS[idx / ndata], nselect) MOD ndata

  IF nselect GT 0 THEN *self.references = (*self.references)[select]

  ndata = N_ELEMENTS(*self.comments)

  select = WHERE((*self.comments)[idx MOD ndata].uid EQ UIDS[idx / ndata], nselect) MOD ndata

  IF nselect GT 0 THEN *self.comments = (*self.comments)[select]

END

;+
; Returns the data on associated with the given Tag
;
; :Returns:
;  struct (variable)
;
; :Params:
;   Tag: in, required, type=string
;    Name of the Tag
;   UIDs: in, required, type="long array (1D)"
;     UIDs to consider; -1 for all
;   Count: out, optional, type=long
;     Number of species found
;
; :Categories:
;   RETRIEVAL
;
; :Private:
;-
FUNCTION AmesPAHdbIDLSuite_Species::GetTagByUID,Tag,UIDs,Count

  COMPILE_OPT IDL2

  ON_ERROR,2

  Count = 0

  itag = WHERE(STRLOWCASE(TAG_NAMES((*self.database).data)) EQ Tag, ntag)

  IF ntag NE 1 THEN RETURN,-1

  nuids = N_ELEMENTS(UIDs)

  IF SIZE(UIDs, /DIMENSIONS) EQ 0 AND UIDs EQ -1 THEN BEGIN

     Count = N_ELEMENTS((*self.database).data.(itag))

     UIDs = (*self.database).data.(itag)[UNIQ((*self.database).data.(itag).uid, SORT((*self.database).data.(itag).uid))].uid

     RETURN,(*self.database).data.(itag)
  ENDIF

  nitag = N_ELEMENTS((*self.database).data.(itag))

  idx = ULINDGEN(nitag, nuids)

  select = WHERE((*self.database).data.(itag)[idx MOD nitag].uid EQ UIDs[idx / nitag], Count) MOD nitag

  IF Count EQ 0 THEN RETURN,-1

  UIDs = (*self.database).data.(itag)[select[UNIQ((*self.database).data.(itag)[select].uid, SORT((*self.database).data.(itag)[select].uid))]].uid

  RETURN,(*self.database).data.(itag)[select]
END

;+
; Retrieves the AmesPAHdbIDLSuite_Species representation in a
; structure.
;
; :Returns:
;   Structure
;
; :Categories:
;   SET/GET
;-
FUNCTION AmesPAHdbIDLSuite_Species::Get

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF NOT PTR_VALID(self.data) THEN RETURN, 0

  RETURN,{type:OBJ_CLASS(self)+'_S', $
          database:self.type, $
          version:self.version, $
          data:*self.data, $
          uids:*self.uids, $
          nuids:self.nuids, $
          references:PTR_VALID(self.references) ? *self.references : PTR_NEW(), $
          comments:PTR_VALID(self.comments) ? *self.comments : PTR_NEW()}
END

;+
; Populates the AmesPAHdbIDLSuite_Species-instance.
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
;   References: in, optional, type="AmesPAHdb_Reference_S array"
;     References
;   Comments: in, optional, type="AmesPAHdb_Comment_S array"
;     Comments
;
; :Categories:
;   SET/GET
;-
PRO AmesPAHdbIDLSuite_Species::Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,References=References,Comments=Comments

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

           IF NOT KEYWORD_SET(References) THEN BEGIN

              IF PTR_VALID(self.references) THEN PTR_FREE,self.references

              self.references = PTR_NEW(Struct.references)
           ENDIF

           IF NOT KEYWORD_SET(Comments) THEN BEGIN

              IF PTR_VALID(self.comments) THEN PTR_FREE,self.comments

              self.comments = PTR_NEW(Struct.comments)
           ENDIF
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

  IF KEYWORD_SET(References) THEN BEGIN

     IF PTR_VALID(self.references) THEN PTR_FREE,self.references

     self.references = PTR_NEW(References)
  ENDIF

  IF KEYWORD_SET(Comments) THEN BEGIN

     IF PTR_VALID(self.comments) THEN PTR_FREE,self.comments

     self.comments = PTR_NEW(Comments)
  ENDIF

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
; Clean-up an AmesPAHdbIDLSuite_Species-instance
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Species::Cleanup

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF PTR_VALID(self.data) THEN PTR_FREE,self.data

  IF PTR_VALID(self.uids) THEN PTR_FREE,self.uids

  IF PTR_VALID(self.references) THEN PTR_FREE,self.references

  IF PTR_VALID(self.comments) THEN PTR_FREE,self.comments
END

;+
; Create an AmesPAHdbIDLSuite_Species-instance
;
; :Returns:
;   AmesPAHdbIDLSuite_Species-instance
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
;   References: in, optional, type="AmesPAHdb_Reference_S array"
;     References
;   Comments: in, optional, type="AmesPAHdb_Comment_S array"
;     Comments
;
; :Categories:
;   CLASS
;-
FUNCTION AmesPAHdbIDLSuite_Species::Init,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,References=References,Comments=Comments

  COMPILE_OPT IDL2

  ON_ERROR,2

  IF N_PARAMS() GT 0 THEN self->Set,Struct,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,References=References,Comments=Comments $
  ELSE self->Set,Type=Type,Version=Version,Data=Data,PAHdb=PAHdb,Uids=Uids,References=References,Comments=Comments

 RETURN, self.state
END

;+
; Defines the AmesPAHdbIDLSuite_Species Class
;
; :Fields:
;   state: type=int
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
;   references: type=pointer
;     Pointer to the references
;   comments: type=pointer
;     Pointer to the comments
;
; :Categories:
;   CLASS
;
; :Private:
;-
PRO AmesPAHdbIDLSuite_Species__DEFINE

  COMPILE_OPT IDL2

  ON_ERROR,2

  void = {AmesPAHdbIDLSuite_Species, $
          state:0L, $
          type:'', $
          version:'', $
          data:PTR_NEW(), $
          database:PTR_NEW(), $
          uids:PTR_NEW(), $
          nuids:0L, $
          references:PTR_NEW(), $
          comments:PTR_NEW()}
END

; END OF amespahdbidlsuite_species__define.pro
