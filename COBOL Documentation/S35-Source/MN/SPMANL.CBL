       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANL.
       DATE-WRITTEN.    092298.
      ******************************************************************
      *
      *   PROGRAM NAME:  MN/SPMANL.C
      *
      *   DESCRIPTION :  STATE PARAMETER MAINTENANCE - PAGE 21.
      *
      *-----------------------------------------------------------------
      * REV:
      *  KEC 980922 ***NEW***
      *  JTG 991108 ALLOWED MULTIPLE ENTRIES IN SP-REVOL-CYCLE-DATE-FRMLA
      *             RE: CENTRAL FINANCE, TN
      *  JTG 991110 ALLOWED SP-REVOL-CYCLE-DATE-FRMLA TO GO TO THE 31TH
      *  BAH 000314 IF LC-REVOLVING NOT = "Y", AVOID ENTERING THIS PAGE, PL# 276
      *  MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      *   CS 041019 ADD CALL TO CL/SPMEMO WRITES MEMO(SM) WHEN FIELD CHANGED
      *                                                        [WORLD PR#362]
      *  CS 160803 SAVEKEY FROM X(8) TO X(9), ORIGINAL-SP-REC TO
      *            ORIGINAL-SP-REC, CHANGED-SP-REC TO CHANGED-SP-REC
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      *  BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/SPMANL.CBL S34 06/08/21-10:58:45 >".

       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

       01  FIRST-SCAN                  PIC 9(1)     VALUE 0.
       01  IO-TYPE                     PIC X(3)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9(1)     VALUE 0.
       01  ONE                         PIC S9(1).

       01  RECORD-SIZES.
           03  CHANGED-SP-REC             PIC X(SP-SIZE) VALUE SPACES.
           03  ORIGINAL-SP-REC            PIC X(SP-SIZE) VALUE SPACES.

       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).


       01  SPEC-TABLE.
           03  FILLER PIC X(08) VALUE "ENNN020 ".

       01  SPEC-TAB                    REDEFINES SPEC-TABLE.
           03  SPEC-REC                              OCCURS 1 TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

       01  MAX                         PIC 9(02)     VALUE 1.
       01  ELE                         PIC 9(02)     VALUE 0.
           88  ELE-VALID                             VALUE 01.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/SPMANSEL.CPY".
       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANL_DEF.CPY".
       COPY "LIBMN/SPMANL_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANL_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      *******************************************
       MAIN-PROGRAM SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE SPACES TO VDU-WIMENU-SELECTION
                          VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANL" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           PERFORM ENTER-PRIMARY-KEY.
           IF ( IO-TYPE = "END" )
              MOVE "X" TO ERRCD
              GO TO EXIT-MAIN-PROGRAM.
           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( NEW-REC = "Y" )
              MOVE "P" TO FORM-P2
              MOVE 22  TO FORM-SEL2.

           IF ( FORM-SEL1 NOT = 0 ) AND
              ( FORM-P1       = "P"    )
              MOVE SP1-KEY   TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           IF ( FORM-SEL2 NOT = 0 ) AND
              ( FORM-P2       = "P"    )
              MOVE SP1-KEY   TO SAVEKEY
              MOVE FORM-SEL2 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.


      **************************************************
       ENTER-PRIMARY-KEY SECTION.
      **************************************************
       BEGIN-ENTER-PRIMARY-KEY.

           MOVE SPACES TO IO-TYPE.

           IF ( SAVEKEY NOT = SPACES )
              MOVE SAVEKEY TO SP1-KEY
              MOVE SPACES  TO SAVEKEY
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-SPFILE.

           MOVE "SPCLASS SUBCLASS LCDESC CLASS STDESC SPNO." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE 0 TO NEW-FLAG.

           MOVE "E  A020SPNO." TO VDU.
           PERFORM SCREENIO.

           IF ( F6-KEY ) PERFORM ENTER-SCANKEY
                         GO TO EXIT-ENTER-PRIMARY-KEY.

           IF ( F7-KEY ) MOVE "END" TO IO-TYPE
                         GO TO EXIT-ENTER-PRIMARY-KEY.

           IF NOT ( ENTER-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           MOVE VDUBUF TO SP-ORGST.

           PERFORM VERIFY-STATE-CODE.

           IF ( IO-BAD )
              GO TO BEGIN-ENTER-PRIMARY-KEY.

      *-----------------------------------------------------------------
       ENTER-SP-CLASS.

           MOVE "LCDESC." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.
           IF ( F1-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           IF ( F5-KEY )
              PERFORM SCAN-LC-FILE
              IF ( IO-BAD )
                 GO TO ENTER-SP-CLASS
              ELSE
                 MOVE "SN N020CLASS." TO VDU
                 MOVE LCSCAN-KEY      TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE LCSCAN-KEY      TO VDUNUM0.

           IF ( F6-KEY )
              MOVE SP-ORGST TO SPSCAN-ORGST
              MOVE 0        TO SPSCAN-SPRCLASS
              MOVE 0        TO SPSCAN-SUBCLASS
              MOVE 0        TO SPSCAN-LAWCODE
              PERFORM SCAN-SP-FILE
              IF ( IO-BAD )
                 GO TO ENTER-SP-CLASS
              ELSE
                 MOVE "SNNN020CLASS."    TO VDU
                 MOVE SPSCAN-SP-SPRCLASS TO VDUNUM0 SP-SPRCLASS
                 PERFORM SCREENIO

                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0 SP-SUBCLASS
                 PERFORM SCREENIO

                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0 SP-LAWCODE
                 PERFORM SCREENIO

                 GO TO READ-SPFILE.

           IF NOT ( ENTER-KEY ) GO TO ENTER-SP-CLASS.

           MOVE VDUNUM0 TO SP-SPRCLASS.
           PERFORM VERIFY-LOAN-CLASS.

           IF ( IO-BAD )
              GO TO ENTER-SP-CLASS.

      *-----------------------------------------------------------------
       ENTER-SP-SUBCLASS.

           MOVE "EN N020SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-SP-CLASS.
           IF ( F1-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           IF ( F6-KEY )
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              MOVE 0           TO SPSCAN-SUBCLASS
              MOVE 0           TO SPSCAN-LAWCODE
              PERFORM SCAN-SP-FILE
              IF ( IO-BAD )
                 GO TO ENTER-SP-SUBCLASS
              ELSE
                 MOVE "SNNN020SUBCLASS." TO VDU
                 MOVE SPSCAN-SP-SUBCLASS TO VDUNUM0 SP-SUBCLASS
                 PERFORM SCREENIO

                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0 SP-LAWCODE
                 PERFORM SCREENIO

                 GO TO READ-SPFILE.

           IF NOT ( ENTER-KEY ) GO TO ENTER-SP-SUBCLASS.

           MOVE VDUNUM0 TO SP-SUBCLASS.

      *-----------------------------------------------------------------
       ENTER-SP-LAWCODE.

           MOVE "EN N020SPCLASS." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-SP-SUBCLASS.
           IF ( F1-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

           IF ( F6-KEY )
              MOVE SP-ORGST    TO SPSCAN-ORGST
              MOVE SP-SPRCLASS TO SPSCAN-SPRCLASS
              ADD 1 SP-SUBCLASS GIVING SPSCAN-SUBCLASS
              MOVE 0           TO SPSCAN-LAWCODE
              PERFORM SCAN-SP-FILE
              IF ( IO-BAD )
                 GO TO ENTER-SP-LAWCODE
              ELSE
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0 SP-LAWCODE
                 PERFORM SCREENIO

                 GO TO READ-SPFILE.

           IF NOT ( ENTER-KEY ) GO TO ENTER-SP-LAWCODE.

           MOVE VDUNUM0 TO SP-LAWCODE.

      *-----------------------------------------------------------------
       READ-SPFILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF ( IO-BAD )
              GO TO BEGIN-ENTER-PRIMARY-KEY.

           MOVE SP-REC TO ORIGINAL-SP-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0      TO NEW-FLAG.

           IF ( NEW-REC = "Y" )
              MOVE 1 TO NEW-FLAG.

       EXIT-ENTER-PRIMARY-KEY.
           EXIT.

      **************************************************
       ENTER-SCANKEY SECTION.
      **************************************************

           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE FIRST-SCAN.

           MOVE " ENTER STATE CODE, OR F6 TO SCAN" TO MESS.
           PERFORM CALL-TMMSG.
           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.

       ENTER-KEY.

           MOVE "E RA020SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) MOVE "CLR" TO IO-TYPE
                                GO TO END-ENTER-SCANKEY.

           IF (        F6-KEY ) GO TO READ-FILES-NEXT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-KEY.

           IF ( VDUBUF = SPACES )
              IF ( FIRST-SCAN NOT = 0 )
                 MOVE SP-REC  TO ORIGINAL-SP-REC
                 GO TO END-ENTER-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF TO SP-ORGST.
           MOVE 0 TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE.
           PERFORM START-SP1-FILE.

      *-----------------------------------------------------------------
       READ-FILES-NEXT.

           PERFORM READ-SP1-FILE-NEXT.

           IF ( IO-BAD )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTER-SCANKEY.

      *    UNLOCK SPA-FILE.
      *    UNLOCK SPB-FILE.
      *    UNLOCK SPC-FILE.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.

           MOVE "S  A020SPNO." TO VDU.
           MOVE SP-ORGST       TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM VERIFY-STATE-CODE.

           MOVE "SN N020CLASS." TO VDU.
           MOVE SP-SPRCLASS     TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020SUBCLASS." TO VDU.
           MOVE SP-SUBCLASS        TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM VERIFY-LOAN-CLASS.
           PERFORM DISPLAY-SUBCLASS-DESC.

           MOVE "SN N020SPCLASS." TO VDU.
           MOVE SP-LAWCODE        TO VDUNUM0.
           PERFORM SCREENIO.

       END-ENTER-SCANKEY.

           PERFORM CLOSE-SP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       EXIT-ENTER-SCANKEY.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.
           MOVE 0      TO ELE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1   ) MOVE 1 TO ELE ONE.
           IF ( ELE-VALID ) GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE
                         MOVE "N" TO NEW-REC
                         GO TO END-ENTRY-MODULE.

           IF F2-KEY
              MOVE SP1-KEY      TO MEMOSP-SP1-KEY
              MOVE "WI/MEMOSP" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH MEMOSP-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              GO TO MODIFY-ROUTINE.
           IF NOT ( ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P   " ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 <  1 ) OR
                 ( FORM-SEL1 = 21 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

      * FORM-NUM2 CHECKS FOR PAGE 10
           IF ( FORM-KEY2 = SPACES ) AND
              ( FORM-P2   = "P"    ) AND
              ( FORM-SEL2 NUMERIC  )
              IF NOT ( FORM-2-DIGITS )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

           GO TO CHECK-CHNO.

      *-----------------------------------------------------------------
       CHECK-PAGE.
           MOVE "RWR" TO IO-TYPE.
           GO TO EXIT-ENTRY-MODULE.

      *-----------------------------------------------------------------
       CHECK-CHNO.

           INSPECT ACTION REPLACING ALL " " BY "0".

           IF ( CHNO-X NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO     = 0  ) GO TO MODIFY-ROUTINE.
           IF ( CHNO     > MAX     ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF ( CHKEY    NOT = ZEROS  ) AND
              ( CHFG(ELE)    = SPACES ) AND
              ( NEW-FLAG NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( CHFG(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

      * REMOVE THIS WHEN YOU NEED THIS PAGE TO WORK! XXXXXXXXX
           GO TO MODIFY-ROUTINE.
      *

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F1-KEY.

           IF NOT ( F1-KEY ) GO TO TEST-F2-KEY.

           MOVE "CLR" TO IO-TYPE.
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       TEST-F2-KEY.

           IF NOT ( F2-KEY ) GO TO TEST-F3-KEY.

      *-----------------------------------------------------------------
       TEST-F3-KEY.

           IF NOT ( F3-KEY ) GO TO TEST-F4-KEY.

      *-----------------------------------------------------------------
       TEST-F4-KEY.

           IF NOT ( F4-KEY ) GO TO TEST-F5-KEY.

      *-----------------------------------------------------------------
       TEST-F5-KEY.

           IF NOT ( F5-KEY ) GO TO TEST-F6-KEY.

      *-----------------------------------------------------------------
       TEST-F6-KEY.

           IF NOT ( F6-KEY ) GO TO TEST-F7-KEY.

      *-----------------------------------------------------------------
       TEST-F7-KEY.

           IF NOT ( F7-KEY ) GO TO TEST-F8-KEY.

      *-----------------------------------------------------------------
       TEST-F8-KEY.

           IF NOT ( F8-KEY ) GO TO TEST-F9-KEY.

      *-----------------------------------------------------------------
       TEST-F9-KEY.

           IF NOT ( F9-KEY ) GO TO TEST-F10-KEY.

      *-----------------------------------------------------------------
       TEST-F10-KEY.

           IF NOT ( F10-KEY ) GO TO TEST-F11-KEY.

      *-----------------------------------------------------------------
       TEST-F11-KEY.

           IF NOT ( F11-KEY ) GO TO TEST-F12-KEY.

      *-----------------------------------------------------------------
       TEST-F12-KEY.

           IF NOT ( F12-KEY ) GO TO TEST-UP-KEY.

      *-----------------------------------------------------------------
       TEST-UP-KEY.

           IF NOT ( UP-KEY ) GO TO TEST-DOWN-KEY.

           IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-DOWN-KEY.

           IF NOT ( DOWN-KEY ) GO TO TEST-ENTER-KEY.

           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-ENTER-KEY.

           IF NOT ( ENTER-KEY ) GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

      *    GO TO F-01 
      *          DEPENDING ON ELE.

           GO TO CREATE-ROUTINE.
      *    GO TO END-ENTRY-MODULE.

      *
      *    S E T - R E C O R D - E L E M E N T S
      *
       F-01.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.

           MOVE 0 TO FORM-SEL1 FORM-SEL2.

       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-SP1-FILE.
           MOVE SP-REC TO CHANGED-SP-REC.

           IF ( ORIGINAL-SP-REC = CHANGED-SP-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-SP1-FILE.
           IF ( IO-BAD )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( SP-REC NOT = ORIGINAL-SP-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-SP-REC TO SP-REC.
           PERFORM REWRITE-SP1-FILE.

      *WRITES A SPR MEMO WHEN CHANGES OCCUR
           IF IO-GOOD
              MOVE FORM-PATHNAME TO FORM-PATH
              MOVE "CL/SPMEMO"   TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   ORIGINAL-SP-REC
                                   SP-REC
              CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       END-WRITE-MODULE.
           PERFORM CLOSE-SP1-FILE.

       EXIT-WRITE-MODULE.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE "S  8000EFFDATE." TO VDU.
           MOVE SP-EFFDATE        TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000EXPDATE." TO VDU.
           MOVE SP-EXPDATE        TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           PERFORM DISPLAY-SUBCLASS-DESC.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       SCAN-LC-FILE SECTION.
      ******************************************************************

           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-LC-FILE.

           MOVE 0 TO IO-FG.

       EXIT-SCAN-LC-FILE.
           EXIT.

      ******************************************************************
       SCAN-SP-FILE SECTION.
      ******************************************************************

           MOVE SPACES     TO SPSCAN-SP1-KEY.
           MOVE SPACES     TO SPSCAN-LAST-LAW.
           MOVE "WI/SPSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SPSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SPSCAN-STATUS NOT = "00" OR SPSCAN-SP1-KEY = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-SCAN-SP-FILE
           ELSE
              MOVE 0 TO IO-FG.

       EXIT-SCAN-SP-FILE.
           EXIT.

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO SPMANL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANL-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANL-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANL-FORM-FIELDS TO WR-BUF.
           MOVE SPMANL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE SPMANL-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANL-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTBNO.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC         TO VDUBUF.
           PERFORM SCREENIO.
       VERIFY-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "* NOT ON FILE *" TO ST-DESC.

           MOVE "S  A160STDESC." TO VDU.
           MOVE ST-DESC          TO VDUBUF.
           PERFORM SCREENIO.

       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "* NOT ON FILE *" TO LC-DESC.

           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC           TO VDUBUF.
           PERFORM SCREENIO.

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
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
             TO MESS.
           PERFORM SEND-MESS.

       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPST1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMANL-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

