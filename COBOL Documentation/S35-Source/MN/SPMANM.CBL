       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMANM.
       AUTHOR.          KEC.
       DATE-WRITTEN.    092598.
      **************************************************************************
      *
      *   PROGRAM NAME:  MN/SPMANM.C
      *
      *   DESCRIPTION :  STATE PARAMETER MAINTENANCE - PAGE 22.
      *
      *-----------------------------------------------------------------
      * REV:
      * KEC 092598 ***NEW***
      * CS  100198 ADDED SP-REVOL-APPLY-PURCHASE
      *                  SP-REVOL-BAL-FRMLA
      *                  SP-REVOL-CYCLE-DATE-FRMLA
      *                  SP-REVOL-PYMT-DUE-FRMLA
      *                  SP-REVOL-PYMT-DUE-WORK
      *                  SP-REVOL-PYMT-EXCEPTION
      *                  SP-REVOL-PYMT-MINIMUM
      *                  SP-REVOL-PYMT-MAXIMUM
      *                  SP-REVOL-PYMT-PERCENT
      *                  SP-REVOL-PYMT-ROUND-CODE
      *                  SP-REVOL-NO-MTHS-NA-PRINT-STMT
      * KEC 103098 ADDED SP-REVOL-MEMBERSHIP-FEE
      *                  SP-REVOL-MEMBERSHIP-FRMLA.
      * JTG 991111 CHANGED PAYMENT DUE WORKER TO HAVE A VALID RANGE
      *            OF 1 TO 27, SINCE VALUES GREATER THAN 27 CAN CAUSE
      *            PAYMENTS TO BECOME DUE AFTER THE NEXT CYCLE DATE
      * GLB 991115 ADD REVOLVING FIELD SP-REVOL-STMT-FORMAT
      * BAH 000314 IF LC-REVOLVING NOT = "Y", AVOID ENTERING THIS PAGE, PL# 276
      * GLB 000404 ADD OPTION "M" TO SP-REVOL-CYCLE-DATE-FRMLA
      * GLB 000524 FIX EDIT ON SP-REVOL-MEMBERSHIP-FRMLA. "A" OR "B"
      * MJD 000928 ADDED F2-MEMO OPTION FOR NEW SPR MEMO FILE (SMFILE)
      * KEC 020821 {PR#01832} LENDMARK <A15> [JIM ROSENAUER]
      *             ADDED SP-REVOL-STMT-EXTRACTION (#22)
      * GLB 021114 PR#1946 LENDMARK; NEW FIELD SP-REVOL-ZERO-ADB
      *            IF "Y" TEST LN-CURBAL; IF 0 DONT CALCULATE INTEREST
      * GLB 021114 BUG VALIDATING "Y" OR "N" ON FIELD 22
      *  CS 041020 ADD CL/SPMEMO WHICH WRITES SPR MEMO(SMFILE) WHEN FIELD IS
      *             CHANGED [WORLD PR#362]
      *  CS 160728 CHANGED SAVEKEY FROM X(8) TO X(9), ORIGINAL-REC TO
      *            ORIGINAL-SP-REC, CHANGED-REC TO CHANGED-SP-REC
      *  CS 170505 ACTIVATE 8 DIGIT DATES
      *  BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      *  BLM 20210806 FIXED NEW RECORD GOING BACK TO PAGE 1 INSTEAD OF GOING
      *               TO PAGE 23.
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/SPMANM.CBL S34 08/06/21-13:52:55 >".

      *=================================================================
      *
      *    F I L E - P A T H - W O R K E R S
      *
      *=================================================================
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

       01  FIRST-SCAN                  PIC 9(01)     VALUE ZEROES.
       01  IO-TYPE                     PIC X(03)     VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)     VALUE ZEROES.
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


      *=================================================================
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      *=================================================================
       01  SPEC-TABLE.
           03  FILLER PIC X(08) VALUE "E  A010 ".

      *-----------------------------------------------------------------
       01  SPEC-TAB                    REDEFINES SPEC-TABLE.
           03  SPEC-REC                              OCCURS 1 TIMES.
               05  SPEC                PIC X(07).
               05  CHFG                PIC X(01).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MAX                         PIC 9(02)     VALUE 30.
       01  ELE                         PIC 9(02)     VALUE ZEROES.
           88  ELE-NOT-AVAILABLE
               VALUE 08 THRU 13,19,24 THRU 30.
           88  ELE-VALID                             VALUE 01 THRU 30.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/MEMOSPW.CPY".
       COPY "LIBLP/SPMANSEL.CPY".
       COPY "LIBWI/SPSCANW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/SPMANM_DEF.CPY".
       COPY "LIBMN/SPMANM_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).

       SCREEN SECTION.
       COPY "LIBMN/SPMANM_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE SPACES TO VDU-WIMENU-SELECTION
                          VDU-PGMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPMANM" TO VDU-FORM-NAME.
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
              MOVE 23  TO FORM-SEL2.

           IF ( FORM-SEL1 NOT = ZEROES ) AND
              ( FORM-P1       = "P"    )
              MOVE SP1-KEY   TO SAVEKEY
              MOVE FORM-SEL1 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           IF ( FORM-SEL2 NOT = ZEROES ) AND
              ( FORM-P2       = "P"    )
              MOVE SP1-KEY   TO SAVEKEY
              MOVE FORM-SEL2 TO FORMSEL
              GO TO EXIT-MAIN-PROGRAM.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.

      ******************************************************************
       ENTER-PRIMARY-KEY SECTION.
      ******************************************************************
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
           MOVE ZEROES TO NEW-FLAG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E  A020SPNO." TO VDU.
           PERFORM SCREENIO.

           IF ( F6-KEY ) PERFORM ENTER-SCANKEY
                         GO TO EXIT-ENTER-PRIMARY-KEY.

           IF ( F7-KEY ) MOVE "END" TO IO-TYPE
                         GO TO EXIT-ENTER-PRIMARY-KEY.

           IF NOT ( ENTER-KEY ) GO TO BEGIN-ENTER-PRIMARY-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
              MOVE ZEROES   TO SPSCAN-SPRCLASS
              MOVE ZEROES   TO SPSCAN-SUBCLASS
              MOVE ZEROES   TO SPSCAN-LAWCODE
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
              MOVE ZEROES      TO SPSCAN-SUBCLASS
              MOVE ZEROES      TO SPSCAN-LAWCODE
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
              MOVE ZEROES      TO SPSCAN-LAWCODE
              PERFORM SCAN-SP-FILE

              IF ( IO-BAD )
                 GO TO ENTER-SP-LAWCODE
              ELSE
                 MOVE "SNNN020SPCLASS." TO VDU
                 MOVE SPSCAN-SP-LAWCODE TO VDUNUM0 SP-LAWCODE
                 PERFORM SCREENIO

                 GO TO READ-SPFILE.

           IF NOT ( ENTER-KEY ) GO TO ENTER-SP-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE ZEROES TO NEW-FLAG.

           IF ( NEW-REC = "Y" )
              MOVE 1 TO NEW-FLAG.

       EXIT-ENTER-PRIMARY-KEY.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE ZEROES TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE FIRST-SCAN.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = SPACES )
              IF ( FIRST-SCAN NOT = ZEROES )
                 MOVE SP-REC  TO ORIGINAL-SP-REC
                 GO TO END-ENTER-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF TO SP-ORGST.
           MOVE ZEROES TO SP-SPRCLASS SP-SUBCLASS SP-LAWCODE.
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

      *=================================================================
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

      *=================================================================
       END-ENTER-SCANKEY.

           PERFORM CLOSE-SP1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

      *-----------------------------------------------------------------
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
           MOVE ZEROES TO ELE.

       CREATE-ROUTINE.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ( ELE < 1           ) MOVE 1 TO ELE ONE.
           IF ( ELE-NOT-AVAILABLE ) GO TO CREATE-ROUTINE.
           IF ( ELE-VALID         ) GO TO ENTER-ELE.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO ACTION.

           IF ( ACTION = SPACES )
              IF ( IO-FG NOT = ZEROES )
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "RWR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( FORM-KEY1 = SPACES ) AND
              ( FORM-P1   = "P   " ) AND
              ( FORM-SEL1 NUMERIC  )
              IF ( FORM-SEL1 <  1 ) OR
                 ( FORM-SEL1 = 22 )
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FORM-NUM2 CHECKS FOR PAGE 10
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           INSPECT ACTION REPLACING ALL SPACES BY ZEROES.

           IF ( CHNO-X NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO     = ZEROES  ) GO TO MODIFY-ROUTINE.
           IF ( CHNO     > MAX     ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

      *-----------------------------------------------------------------
       ENTER-ELE.

           IF ( ELE-NOT-AVAILABLE ) GO TO CREATE-ROUTINE.

           IF ( CHKEY    NOT = ZEROES ) AND
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
      * REMOVE THIS WHEN YOU NEED THIS PAGE TO WORK! XXXXXXXXX
           GO TO MODIFY-ROUTINE.
      *
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE ZEROES    TO VDUCOL.
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

       GOTO-FIELD.

           GO TO CREATE-ROUTINE.

       F-01.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.

           MOVE ZEROES TO FORM-SEL1 FORM-SEL2.

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

           MOVE ZEROES TO IO-FG.

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
              MOVE ZEROES TO IO-FG.

       EXIT-SCAN-SP-FILE.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTBNO.CPY".


      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO SPMANM-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPMANM-SYSDATE.
           DISPLAY SPACES UPON SPMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPMANM-SCREEN UPON SPMAIN-WINDOW-HANDLE.
           MOVE SPMANM-FORM-FIELDS TO WR-BUF.
           MOVE SPMANM-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE SPMANM-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMANM-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/SPMANM_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       DISPLAY-SUBCLASS-DESC.
           MOVE "S  A150DESC. " TO VDU.
           MOVE SP-DESC         TO VDUBUF.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       VERIFY-STATE-CODE.
           MOVE SP-ORGST TO ST-CODE.
           PERFORM OPEN-ST1-FILE.
           PERFORM READ-ST1-FILE.
           PERFORM CLOSE-ST1-FILE.

           IF ( IO-FG NOT = ZEROES )
              MOVE "* NOT ON FILE *" TO ST-DESC.

           MOVE "S  A160STDESC." TO VDU.
           MOVE ST-DESC          TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       VERIFY-LOAN-CLASS.
           MOVE SP-SPRCLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

           IF ( IO-FG NOT = ZEROES )
              MOVE "* NOT ON FILE *" TO LC-DESC.

           MOVE "S  A160LCDESC. " TO VDU.
           MOVE LC-DESC           TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
             TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       NOT-EXPIRED.
           MOVE "THIS RECORD HAS NOT EXPIRED - CANNOT DELETE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
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
           DESTROY SPMANM-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE SPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.

