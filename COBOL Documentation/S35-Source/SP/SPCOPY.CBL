       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPCOPY.
      ******************************************************************
      *
      *   DIR :  SP
      *   DESC:  STATE PARAMETER FILE COPY
      *   THIS PROGRAM WILL COPY THE FIELDS OF AN EXISTING RECORD TO
      * ANOTHER NEW RECORD. THIS NEW RECORD MAY HAVE A DIFFERENT STATE CODE
      * AND/OR DIFFERENT EFFECTIVE AND EXPIRATION DATES.
      *   THE PURPOSE OF THE PROGRAM IS TO ALLOW THE USER TO CREATE A NEW
      * STATE PARAMETER RECORD BY ENTERING ONLY THE NEW STATE CODE, LOAN CLASS,
      * EFFECTIVE DATE AND EXPIRATION DATE. THE REMAINDER OF THE RECORD NEED
      * NOT BE ENTERED BUT IS COPIED FROM THE EXISTING RECORD TO THE NEW
      * RECORD.
      *   THE PROGRAM WORKS AS FOLLOWS:
      *  ENTER FROM THE EXISTING RECORD THE   STATE CODE
      *                                       LOAN CLASS   (TO COPY FROM)
      *                                       SUB CLASS
      *                                       LAW CODE
      *  THE EFFECTIVE AND EXPIRATION DATES OF THIS RECORD WILL NOW DISPLAY.
      *  ENTER THE                        NEW STATE CODE
      *                                   NEW LOAN CLASS
      *                                   EFFECTIVE DATE   (TO COPY TO)
      *                                   EXPIRATION DATE
      *  THE PROGRAM WILL NOW DISPLAY THE APPROPRIATE SUBCLASS FOR THIS RECORD.
      *
      *  IF A RECORD CONTAINING THE SAME STATE CODE AND LOAN CLASS WITH NO
      *     EXPIRATION DATE IS CURRENTLY IN EFFECT, A MESSAGE WILL APPEAR ON THE
      *     SCREEN REQUIRING REENTERING OF DATA.
      *  IF AN EXPIRATION DATE DOES EXIST, THE NEW RECORD'S EFFECTIVE DATE
      *     WILL BE AUTOMATICALLY DISPLAYED ON THE SCREEN TO BE THE SAME AS THE
      *     OLD RECORD'S EXPIRATION DATE.
      *  THE NEW RECORD'S EXPIRATION DATE MUST NOT BE LESS THAN ITS EFFECTIVE
      *     DATE OR A MESSAGE WILL APPEAR.
      *  A MESSAGE WILL ALSO DISPLAY IF THE LOAN CLASSES FROM THE EXISTING
      *     RECORD AND THE NEW RECORD ARE NOT COMPATIBLE.
      *
      *=================================================================
      * REV:
      * JTG 120296 REMOVED TEST ON LN-SERVACCT FOR CONSISTENCY BETWEEN SPR'S
      * JTG 121197 CHANGED TO SKIP SET OF GI-ACCT (BRNO) SOURCE GI-ACCT (BRNO)
      *            IS DIFFERENT THAN SOURCE BI-BRANCH NUMBER
      * JTG 051898 REMOVED TEST ON LN-SALEFIN
      * BLV 061198 FIXED SIZE OF ORIG & CHANGED TO SP-SIZE, NEW SIZE 4513
      *
      * KEC 2017.0224 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED THE HARDCODE VERSION OF THE SP-REC;
      *          ADDED OSP-REC FROM 'REPLACING LEADING' ON SP-REC.
      *
      *  20171017 BAH ACTIVATE 8 DIGIT DATES, PD0006
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/SPCOPY.CBL S34 06/21/21-10:17:44 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

      *=================================================================
      *   OSP-REC FROM 'REPLACING LEADING' ON SP-REC.
      *=================================================================
       01  OSP-REC.
           03  OSP1-KEY.
               05  OSP-ORGST                   PIC XX.
               05  OSP-SPRCLASS                PIC 999.
               05  OSP-SUBCLASS                PIC 99.
               05  OSP-LAWCODE                 PIC 99.

       COPY "LIBLP/LP01SPA_AREA.CPY" REPLACING LEADING "SPA-" BY "OSP-"
                                     LEADING "AEXPR" BY "OEXPR"
                                     LEADING "AASTP" BY "OASTP"
                                     LEADING "ACTBL" BY "OCTBL"
                                     LEADING "ACSTP" BY "OCSTP"
                                     LEADING "AETBL" BY "OETBL"
                                     LEADING "AESTP" BY "OESTP".
      * SIZE=2733
       COPY "LIBLP/LP01SPB_AREA.CPY" REPLACING LEADING "SPB-" BY "OSP-"
                                        LEADING "BILEV" BY "OILEV"
                                        LEADING "BISTP" BY "OISTP"
                                        LEADING "BIVER" BY "OIVER".
      * SIZE=2231
       COPY "LIBLP/LP01SPC_AREA.CPY" REPLACING LEADING "SPC-" BY "OSP-".

      *=================================================================
       01  ONE               PIC S9.
       01  ELE               PIC 99     VALUE 0.
       01  ACTION            VALUE SPACES.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
           03  CHNO-X REDEFINES CHNO
                             PIC X(2).

       01  MSEL-LIST         PIC X(5)    VALUE "MSEL.".
       01  IO-TYPE           PIC X(3)   VALUE SPACES.
       01  NEW-FLAG          PIC 9      VALUE 0.
       01  TIME1             PIC X  VALUE "Y".

       01  REC-VALID         PIC X  VALUE " ".
       01  EX-FLAG           PIC X  VALUE " ".
       01  EX1-FLAG          PIC X  VALUE " ".
       01  FIRST-SCAN        PIC 9      VALUE 0.
       01  NEW-SP-DESC       PIC X(15).

       01  HOLD-SALEFIN      PIC X.
       01  HOLD-SERVACCT     PIC X.
       01  HOLD-LOANTYPE     PIC X.
       01  MESS1.
           03  MESS-LIT      PIC X(49).
           03  MESS-VAR      PIC XX.
       01  DATE-WK-1         PIC 9(8).
       01  DATE-WK-2         PIC 9(8).
       01  RESULT            PIC X  VALUE " ".
       01  EFFECTIV-DATE     PIC 9(8).
       01  EFFEC-DATE REDEFINES EFFECTIV-DATE.
           03  EFF-CC        PIC 99.
           03  EFF-YR        PIC 99.
           03  EFF-MO        PIC 99.
           03  EFF-DA        PIC 99.
       01  EXPIRAT-DATE      PIC 9(8).
       01  EXPIR-DATE REDEFINES EXPIRAT-DATE.
           03  EXP-CC        PIC 99.
           03  EXP-YR        PIC 99.
           03  EXP-MO        PIC 99.
           03  EXP-DA        PIC 99.

       01  EFF-DATE1              PIC 9(8).
       01  EXP-DATE2              PIC 9(8)  VALUE 0.

       01  ERRCD         PIC X.

       01  DISPLAY-BUF      VALUE SPACES.
           03  A01           PIC XX.
           03  A02           PIC 99.
           03  A03           PIC 99.
           03  A04           PIC 99/99/99.
           03  A05           PIC 99/99/99  BLANK ZERO.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS
      *    **********************************
       01  MAX               PIC 9 COMP-3 VALUE 6.
       01  SPEC-TABLE.
      * 1. NEW STATE CODE
           03  FILLER        PIC X(8)   VALUE "E  A020 ".
      * 2. NEW LOAN CLASS
           03  FILLER        PIC X(8)   VALUE "EN N020 ".
      * 3. NEW SUB CLASS
           03  FILLER        PIC X(8)   VALUE "EN N020 ".
      * 4. NEW LAW CODE
           03  FILLER        PIC X(8)   VALUE "EN N020N".
      * 5. EFF DATE
           03  FILLER        PIC X(8)   VALUE "ERNM060 ".
      * 6. EXP DATE
           03  FILLER        PIC X(8)   VALUE "ERNM060 ".
       01  SPEC-TAB       REDEFINES SPEC-TABLE.
           03  SPEC-REC   OCCURS 6 TIMES.
               05  SPEC   PIC X(7).
               05  CHFG   PIC X.

       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  SPCOPY-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/SPCOPY_DEF.CPY".
       COPY "LIBSP/SPCOPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/SPCOPY_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
       MAIN-PROGRAM SECTION.
      **************************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           MOVE "SPCOPY" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SPCOPY-WINDOW-HANDLE.


      D    STOP MESS.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           PERFORM REC-SP-MODULE.
           IF IO-TYPE = "END" OR "E99"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           GO TO MAIN-MODULE.

      *******************************************
       REC-SP-MODULE SECTION.
      *******************************************
       REC-SP-MODULE-NO.
           MOVE SPACES TO IO-TYPE.
           MOVE 0 TO NEW-FLAG.

           MOVE "SPCLASS LCDESC CLASS SUBCLASS DESC STDESC SPNO."
                 TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "F7-EXIT, F6-SCAN, ENTER 'COPY FROM' STATE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A020SPNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              IF VDUBUF = "99"
                 MOVE "E99" TO IO-TYPE
                 GO TO END-REC-SP-MODULE
              ELSE
                 MOVE "END" TO IO-TYPE
                 GO TO END-REC-SP-MODULE.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO END-REC-SP-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-SP-MODULE-NO.
           MOVE VDUBUF TO SP-ORGST.
           PERFORM VERIFY-STATE-CODE.
           IF IO-FG NOT = 0
              GO TO REC-SP-MODULE-NO.
       REC-SP-CLASS.
           MOVE "LCDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "F1, F6-SCAN, ENTER 'COPY FROM' CLASS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE 0 TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-CLASS
              ELSE
                 MOVE "SNNN020CLASS." TO VDU
                 MOVE SPSCAN-SP-SPRCLASS TO VDUNUM0
                                            SP-SPRCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 PERFORM VERIFY-LOAN-CLASS
                 IF IO-FG NOT = 0
                    GO TO REC-SP-CLASS
                 ELSE
                    GO TO READ-SPFILE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-SP-CLASS.
           MOVE VDUNUM0 TO SP-SPRCLASS.
           PERFORM VERIFY-LOAN-CLASS.
           IF IO-FG NOT = 0
              GO TO REC-SP-CLASS.
       REC-SP-SUBCLASS.
           MOVE "F1, F6-SCAN, ENTER 'COPY FROM' SUBCLASS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N020SUBCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-CLASS.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-SUBCLASS
              ELSE
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0
                                            SP-SUBCLASS
                 PERFORM SCREENIO
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.
           IF VDUSTATUS NOT = "00"
              GO TO REC-SP-SUBCLASS.
           MOVE VDUNUM0 TO SP-SUBCLASS.
       REC-SP-LAWCODE.
           MOVE "F1, F6-SCAN, ENTER 'COPY FROM' LAWCODE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N020SPCLASS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO REC-SP-MODULE-NO.
           IF VDUSTATUS = "1U"
              GO TO REC-SP-SUBCLASS.
           IF VDUSTATUS = "1<"
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE 0 TO SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              IF IO-FG = 9
                 GO TO REC-SP-LAWCODE
              ELSE
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0
                                           SP-LAWCODE
                 PERFORM SCREENIO
                 GO TO READ-SPFILE.
           IF VDUSTATUS NOT = "00"
             GO TO REC-SP-LAWCODE.
           MOVE VDUNUM0 TO SP-LAWCODE.
       READ-SPFILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG = 9
               GO TO REC-SP-MODULE-NO.

           MOVE SP-REC TO OSP-REC.

           PERFORM DISPLAY-FIELDS.
       END-REC-SP-MODULE.
           MOVE 1 TO NEW-FLAG.

      *******************************************
       ENTER-SCANKEY SECTION.
      *******************************************
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS
                     SP-LAWCODE FIRST-SCAN.
           MOVE " ENTER STATE CODE, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
       ENTER-KEY.
           MOVE "F1, F6-SCAN, ENTER SCAN STATE(RTN):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA020SCANKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 MOVE SP-REC TO OSP-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
           MOVE VDUBUF TO SP-ORGST.
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE.
           PERFORM START-SP1-FILE.
       READ-FILES-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK SPA-FILE.
      *    UNLOCK SPB-FILE.
      *    UNLOCK SPC-FILE.

           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.

           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.
       DISPLAY-SCAN-KEY.
           MOVE "S  A020SPNO." TO VDU.
           MOVE SP-ORGST TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM VERIFY-STATE-CODE.
           MOVE "SN N020CLASS." TO VDU.
           MOVE SP-SPRCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020SUBCLASS." TO VDU.
           MOVE SP-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-LOAN-CLASS.
           MOVE "SN N020SPCLASS." TO VDU.
           MOVE SP-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.
       CLOSE-SCANKEY.
           PERFORM CLOSE-SP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       END-SCANKEY.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.
       CREATE-ROUTINE.
           MOVE " " TO TIME1.
           IF NEW-FLAG NOT = 1
               MOVE 0 TO SP-LAWCODE
               PERFORM CHECK-SP-RECS
               IF EX-FLAG = "Y"
                  MOVE " " TO EX-FLAG
                  GO TO MODIFY-ROUTINE.
           IF NEW-FLAG NOT = 1
              IF EX1-FLAG ="Y"
                 MOVE " " TO EX1-FLAG
                 MOVE 6 TO ELE
                 GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              IF REC-VALID = "Y"
               GO TO MODIFY-ROUTINE
               ELSE
                MOVE "RECORD CONTAINS INVALID ENTRIES"
                        TO MESS
                PERFORM SEND-MESS.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "F1-CANCEL, (RTN)-ACCEPT, ENTER FIELD #:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = SPACES
              MOVE 0 TO SP-LAWCODE
              PERFORM CHECK-SP-RECS
              IF EX-FLAG = "Y"
                 MOVE " " TO EX-FLAG
                 GO TO MODIFY-ROUTINE.
           IF ACTION = SPACES
              IF EX1-FLAG = "Y"
                 MOVE " " TO EX1-FLAG
                 GO TO ENTER-ELE.
           IF ACTION = SPACES
              IF REC-VALID = "Y"
                 MOVE SP1-KEY TO OSP1-KEY
                 MOVE EFFECTIV-DATE TO SP-EFFDATE OSP-EFFDATE
                 MOVE EXPIRAT-DATE TO SP-EXPDATE OSP-EXPDATE
                 MOVE NEW-SP-DESC TO SP-DESC OSP-DESC
                 MOVE OSP-REC TO SP-REC
                 MOVE "RWR" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE
              ELSE
                MOVE "RECORD CONTAINS INVALID ENTRIES" TO MESS
                PERFORM SEND-MESS
                GO TO MODIFY-ROUTINE.
       CHECK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
       ENTER-ELE.
           IF ELE = 1
              MOVE "F1, ENTER 'COPY TO' STATE:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 2
              MOVE "F1, F6-SCAN, ENTER 'COPY TO' CLASS:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 3
              MOVE "F1, F6-SCAN, ENTER 'COPY TO' SUBCLASS:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 4
              MOVE "F1, ENTER 'COPY TO' LAWCODE:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 5
              MOVE "F1, ENTER 'COPY TO' EFFECTIVE DATE:" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 6
              MOVE "F1, ENTER 'COPY TO' EXPIRATION DATE:" TO MESS
              PERFORM SEND-LEGEND.
           IF CHKEY NOT = ZEROS
              IF CHFG(ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                   GO TO MODIFY-ROUTINE.
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.
       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F1.
           IF ELE = 2
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE 0 TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              GO TO ENTER-ELE.
           IF ELE = 3
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0 TO SPSCAN-SUBCLASS
                        SPSCAN-LAWCODE
              PERFORM SPSCAN-SCAN
              GO TO ENTER-ELE.

              MOVE 1 TO ONE.
              GO TO CREATE-ROUTINE.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
           MOVE 1 TO ONE.
       GOTO-FIELD.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06
                 DEPENDING ON ELE.
           GO TO EXIT-ENTRY-MODULE.

      *    **********************************
      *    *      SET RECORD ELEMENTS
      *    **********************************
       A-01.
           MOVE VDUBUF TO SP-ORGST.
           PERFORM VERIFY-NEW-STATE-CODE.
           IF IO-FG NOT = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-02.
           MOVE VDUNUM0 TO SP-SPRCLASS.
           PERFORM VERIFY-NEW-LOAN-CLASS.
           IF IO-FG NOT = 0
              GO TO ENTER-ELE.
      *    IF (LC-SALEFIN NOT = HOLD-SALEFIN) OR
      *       (LC-SERVACCT NOT = HOLD-SERVACCT) OR
           IF (LC-LOANTYPE NOT = HOLD-LOANTYPE)
                MOVE "INCONSISTANT LOAN CLASS TYPES" TO MESS
                PERFORM SEND-MESS
                GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-03.
           MOVE VDUNUM0 TO SP-SUBCLASS.
       A-03A.
           MOVE "^, ENTER NEW SUBCLASS DESCRIPTION:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A015NEWDESC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-03A.
           MOVE VDUBUF TO SP-DESC
                          NEW-SP-DESC.
           GO TO CREATE-ROUTINE.
       A-04.
           MOVE VDUNUM0 TO SP-LAWCODE.
           GO TO CREATE-ROUTINE.
       A-05.
           MOVE VDU-DATE-YYYYMMDD TO SP-EFFDATE EFFECTIV-DATE
                                       EFF-DATE1.
           IF SP-EFFDATE = 0
              MOVE "EFFECTIVE DATE CANNOT = 0" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       A-06.
           MOVE "Y" TO TIME1.
           MOVE VDU-DATE-YYYYMMDD TO SP-EXPDATE EXPIRAT-DATE.
           IF SP-EXPDATE NOT = 0
              PERFORM COMPARE-DATE
              IF RESULT = "L"
                 MOVE "EXPIRATION DATE CANNOT BE < EFFECTIVE DATE"
                       TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE 0 TO SP-LAWCODE.
           PERFORM CHECK-SP-RECS.
           IF EX1-FLAG = "Y"
              MOVE " " TO EX1-FLAG
              GO TO ENTER-ELE.
           IF TIME1 = "Y"
              GO TO CREATE-ROUTINE.
       EXIT-ENTRY-MODULE.
           EXIT.

      ********************************************
       CHECK-SP-RECS SECTION.
      ********************************************
           MOVE " " TO REC-VALID EX-FLAG.
           ADD 1 TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG = 0
              IF SP-EXPDATE = 0
                 MOVE
              "PRIOR STATE PARAMETER REC IN EFFECT FOR SUBCLASS "
                                                       TO MESS-LIT
                 MOVE SP-LAWCODE TO MESS-VAR
                 MOVE MESS1 TO MESS
                 PERFORM SEND-MESS
                MOVE "Y" TO EX-FLAG
                GO TO EXIT-CHECK-SP
              ELSE
                 MOVE SP-EXPDATE TO EXP-DATE2
                 GO TO CHECK-SP-RECS.
           MOVE "SN N020040." TO VDU.
           MOVE SP-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.
           IF SP-LAWCODE = 1
              MOVE 0 TO EXP-DATE2.
           IF (EXP-DATE2 NOT = 0) AND (EFF-DATE1 NOT = EXP-DATE2)
               MOVE EXP-DATE2 TO EFF-DATE1 EFFECTIV-DATE
               MOVE "S  8080050." TO VDU
               MOVE EFF-DATE1 TO VDU-DATE-YYYYMMDD
               PERFORM SCREENIO.
            IF EXPIRAT-DATE NOT = 0
               PERFORM COMPARE-DATE
               IF RESULT = "L"
                  MOVE "EXPIRATION DATE CANNOT BE < EFFECTIVE DATE"
                         TO MESS
                  PERFORM SEND-MESS
                  MOVE "Y" TO EX1-FLAG
                  GO TO EXIT-CHECK-SP.
           MOVE "Y" TO REC-VALID.
       EXIT-CHECK-SP.
           EXIT.

      *******************************************
       WRITE-MODULE SECTION.
      *******************************************
           PERFORM OPEN-SP1-FILE.

           PERFORM WRITE-SP1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-SP1-FILE.

      *******************************************
       COMPARE-DATE SECTION.
      *******************************************
           MOVE " " TO RESULT.
           IF EXP-YR < 50 AND EFF-YR > 49
              MOVE "G" TO RESULT
           ELSE IF EFF-YR < 50 AND EXP-YR > 49
              MOVE "L" TO RESULT.
           IF RESULT = " "
              COMPUTE DATE-WK-1 = EXP-YR * 372 + EXP-MO * 31
                                   + EXP-DA
              COMPUTE DATE-WK-2 = EFF-YR * 372 + EFF-MO * 31
                                   + EFF-DA
              IF DATE-WK-1 < DATE-WK-2
                 MOVE "L" TO RESULT
               ELSE IF DATE-WK-1 > DATE-WK-2
                 MOVE "G" TO RESULT
               ELSE
                 MOVE "E" TO RESULT.

      *******************************************
       DISPLAY-FIELDS SECTION.
      *******************************************
           MOVE SPACES TO DISPLAY-BUF.
           MOVE "S  8080EFFDATE." TO VDU.
           MOVE SP-EFFDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           IF SP-EXPDATE NOT = 0
              MOVE "S  8080EXPDATE." TO VDU
              MOVE SP-EXPDATE TO VDU-DATE-YYYYMMDD
              PERFORM SCREENIO
            ELSE
              MOVE "S  A080EXPDATE." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "S  A150DESC." TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.
       DISPLAY-FIELDS-EXIT.
           EXIT.

      *******************************************
       SPSCAN-SCAN SECTION.
      *******************************************
           MOVE SPACES TO SPSCAN-SP1-KEY.
           MOVE SPACES TO SPSCAN-LAST-LAW.
           MOVE "WI/SPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SPSCAN-STATUS NOT = "00" OR SPSCAN-SP1-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-SPSCAN-SCAN
           ELSE
              MOVE 0 TO IO-FG.

       EXIT-SPSCAN-SCAN.
           EXIT.

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO SPCOPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPCOPY-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPCOPY-SCREEN UPON SPCOPY-WINDOW-HANDLE.
           MOVE SPCOPY-FORM-FIELDS TO WR-BUF.
           MOVE SPCOPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE SPCOPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPCOPY-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SPCOPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************
       MISC-PARAGRAPHS SECTION.
      *******************************************
       COPY "LIBGB/ACCESS.CPY".
       VERIFY-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO ST-DESC.
           MOVE "S  A160STDESC." TO VDU.
           MOVE ST-DESC TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE LC-SALEFIN TO HOLD-SALEFIN.
           MOVE LC-SERVACCT TO HOLD-SERVACCT.
           MOVE LC-LOANTYPE TO HOLD-LOANTYPE.
           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-NEW-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO ST-DESC.
           MOVE "S  A160NSTDES." TO VDU.
           MOVE ST-DESC TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-NEW-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.
           MOVE "S  A160NLCDES." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPCOPY-SCREEN.
           DESTROY SPCOPY-WINDOW-HANDLE.
           EXIT PROGRAM.
